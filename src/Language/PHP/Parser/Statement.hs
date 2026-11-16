{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Parser.Statement
  ( parseProgram
  , parseProgramBody
  , parseStmt
  , parseClassMember
  , parseClassMemberInContext
  , ClassContext (..)
  , parseExpr
  , parseParam
  , parseParamInContext
  , ParamContext (..)
  , parseAttributes
  , parseAttributeGroup
  , parseAttribute
  , parseArg
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad (void, when, unless)
import Data.Maybe (isJust, isNothing)
import Data.Monoid (Any (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.PHP.Parser.Megaparsec as M
import qualified Text.Megaparsec.Char as C

import Language.PHP.AST
import Language.PHP.Fold (queryExpr)
import Language.PHP.Span (Span (..), SourcePos (..), combineSpans)
import Language.PHP.Parser.Lexer
import Language.PHP.Parser.Type (parseType, parseReturnType, disallowedPropertyType, disallowedConstantType)
import Language.PHP.Parser.Internal
  ( Grammar (..)
  , parseExprWithGrammar
  , parseAttributesWith
  , parseAttributeGroupWith
  , parseAttributeWith
  , parseArgWith
  , parseParamList
  , hasEmptyDestructure
  , isAssignable
  , isWritable
  )

-- | Expression parser with full statements, parameters, and class members in closures and anonymous classes.
parseExpr :: Parser (Expr Span)
parseExpr = parseExprWithGrammar Grammar
  { gBody   = parseMixedBody
  , gMember = \ro -> parseClassMemberInContext (AnonClassContext ro)
  , gParam  = parseParamInContext NonConstructorParam
  }

-- | Attributes #[ ... ]
parseAttributes :: Parser [AttributeGroup Span]
parseAttributes = parseAttributesWith parseExpr

-- | Single attribute group #[ ... ]
parseAttributeGroup :: Parser (AttributeGroup Span)
parseAttributeGroup = parseAttributeGroupWith parseExpr

-- | Attribute within a group
parseAttribute :: Parser (Attribute Span)
parseAttribute = parseAttributeWith parseExpr

-- | Single call argument
parseArg :: Parser (Arg Span)
parseArg = parseArgWith parseExpr

-- | Parse a complete PHP program, handling optional opening tags, inline HTML, and statements.
-- Nothing precedes a program, so its trivia is whatever follows the last
-- statement: comments with no later node to lead.
parseProgram :: Parser (Program Span)
parseProgram = do
  (sp, stmts) <- spanned parseProgramBody
  takeTrivia >>= recordTrivia sp
  pure (Program sp stmts)

-- | Parse a statement list that switches between PHP code and inline HTML.
-- A file begins in HTML mode, exactly as PHP's lexer does, and only a close
-- tag returns the parser to it -- so an open tag met in code mode is an
-- ordinary @<@ and a parse error, never a silent transition (Issue #271).
parseProgramBody :: Parser [Stmt Span]
parseProgramBody = parseRegion (HtmlRegion parseCodeChunks)

data CodeRegion
  = HtmlRegion (Parser [Stmt Span])
  | PhpRegion

-- | Statements in code mode, until a close tag or end of input. A close tag
-- returns the parser to HTML mode, where the next open tag -- an ordinary
-- @<@ everywhere else -- may open a new region.
parseCodeChunks :: Parser [Stmt Span]
parseCodeChunks = parseRegion PhpRegion

parseRegion :: CodeRegion -> Parser [Stmt Span]
parseRegion PhpRegion =
  (parseCloseTag *> parseRegion (HtmlRegion parseCodeChunks) >>= \html -> (html ++) <$> parseCodeChunks)
  <|> (do
    isEof <- (True <$ M.lookAhead M.eof) <|> pure False
    if isEof
      then pure []
      else (:) <$> parseStmt <*> parseCodeChunks)
parseRegion (HtmlRegion k) = do
  (sp, html) <- spanned takeUntilPhpTag
  let htmlStmts = if null html then [] else [StmtInlineHtml sp (T.pack html)]
  when (not (null html) && not (isLeadingShebang sp html)) $ do
    markScriptStatement
    markNonDeclareContent
    markNamespaceBlockingContent
  isEof <- (True <$ M.lookAhead M.eof) <|> pure False
  if isEof
    then pure htmlStmts
    else do
      isShortEcho <- (True <$ M.try (C.string "<?=")) <|> pure False
      if isShortEcho
        then do
          echoStmt <- parseShortEchoBody
          markScriptStatement
          markNonDeclareContent
          markNamespaceBlockingContent
          rest <- k
          pure (htmlStmts ++ echoStmt : rest)
        else do
          _ <- parseOpenTag
          sc
          rest <- k
          pure (htmlStmts ++ rest)

-- | Parse an opening tag, excluding the whitespace and comments after it,
-- so callers can backtrack over the tag without hiding errors in that trivia.
parseOpenTag :: Parser ()
parseOpenTag =
  (M.try (C.string' "<?php") *> (phpCodeWhitespace1 <|> void M.eof))
    <|> (C.string "<?" *> M.notFollowedBy (C.char '=') *> M.notFollowedBy (C.string "php"))

parseCloseTag :: Parser ()
parseCloseTag = do
  _ <- C.string "?>"
  consumeCloseTag
  -- PHP suppresses the newline immediately following a close tag,
  -- matching its lexer's NEWLINE rule: "\r\n" as a pair, "\n", or "\r".
  _ <- optional (C.char '\n' <|> (C.char '\r' *> optional (C.char '\n') *> pure '\n'))
  pure ()

-- | A statement may end with a semicolon or an immediately following close tag.
-- Leave the close tag for the statement list's mode driver, which switches to
-- HTML mode there.
statementTerminator :: Parser T.Text
statementTerminator = semi <|> (M.lookAhead (C.string "?>") *> noteCloseTagTerminator *> pure ";")

-- | The body of a short echo tag, after its @<?=@ opener. @<?=@ is @echo@,
-- so it takes the same comma-separated expression list.
parseShortEchoBody :: Parser (Stmt Span)
parseShortEchoBody = do
  sc
  firstExpr <- parseExpr
  moreExprs <- M.many (comma *> parseExpr)
  _ <- statementTerminator
  let lastExpr = if null moreExprs then firstExpr else last moreExprs
      echoSpan = combineSpans (annotation firstExpr) (annotation lastExpr)
  pure (StmtEcho echoSpan (firstExpr : moreExprs))

-- | Parse a statement list that can switch between PHP and inline HTML.
-- Close/open tag transitions are not represented as empty statements; only
-- non-empty HTML chunks become 'StmtInlineHtml' nodes. A bare open tag inside
-- code is a parse error (Issue #271), not a silent transition.
parseMixedBody :: Parser [Stmt Span]
parseMixedBody = concat <$> M.many parseMixedBodyElement

parseMixedBodyElement :: Parser [Stmt Span]
parseMixedBodyElement =
  parseHtmlChunk
  <|> ((\s -> [s]) <$> parseStmt)
  where
    -- A close tag returns the parser to HTML mode: the inline HTML that
    -- follows ends at the next tag, which alone may open the next region.
    parseHtmlChunk = M.try parseCloseTag *> parseRegion (HtmlRegion (pure []))

-- | A shebang is not a statement. PHP skips a leading @#!@ line before the
-- file statement list, so it does not push a following namespace out of place.
isLeadingShebang :: Span -> String -> Bool
isLeadingShebang sp html =
  posOffset (spanStart sp) == 0 && isShebangLine html
  where
    isShebangLine ('#':'!':rest) =
      case span (not . isNewline) rest of
        (_, []) -> True
        (_, '\r':'\n':after) -> null after
        (_, nl:after) -> isNewline nl && null after
    isShebangLine _ = False
    isNewline c = c == '\n' || c == '\r'

takeUntilPhpTag :: Parser String
takeUntilPhpTag = do
  isTag <- (True <$ M.lookAhead (C.string "<?php" <|> C.string "<?=" <|> (C.string "<?" <* M.notFollowedBy (C.char '=')))) <|> pure False
  isEof <- (True <$ M.lookAhead M.eof) <|> pure False
  if isTag || isEof
    then pure []
    else do
      c <- M.anySingle
      (c :) <$> takeUntilPhpTag

-- | Parse a single statement.
parseStmt :: Parser (Stmt Span)
parseStmt = do
  stmt <- withStatement parseStmtCore
  case stmt of
    StmtDeclare {} -> pure ()
    StmtEmpty {} -> markNonDeclareContent
    _ -> markNonDeclareContent *> markNamespaceBlockingContent
  pure stmt

parseStmtCore :: Parser (Stmt Span)
parseStmtCore =
  parseNamespace
  <|> parseUse
  <|> parseClass
  <|> parseInterface
  <|> parseTrait
  <|> parseEnum
  <|> parseFunction
  <|> parseConstStmt
  <|> parseIf
  <|> parseWhile
  <|> parseDoWhile
  <|> parseFor
  <|> parseForeach
  <|> parseSwitch
  <|> parseTry
  <|> parseBlock
  <|> parseEcho
  <|> parseGlobal
  <|> parseStaticStmt
  <|> parseBreak
  <|> parseContinue
  <|> parseReturn
  <|> parseThrowStmt
  <|> parseDeclare
  <|> parseGoto
  <|> parseUnset
  <|> parseEmptyStmt
  <|> M.try parseHaltCompiler
  <|> parseLabel
  <|> parseExprStmt

-- | Declare directive statement: declare(...) ; or declare(...) { ... } or declare(...): ... enddeclare;
parseDeclare :: Parser (Stmt Span)
parseDeclare = withSpan $ do
  keyword_ "declare"
  directives <- parens (parseDeclareDirective `M.sepEndBy1` comma)
  hasStrictTypes <- pure (any isStrictTypesDirective directives)
  inPrologue <- inDeclarePrologue
  when (hasStrictTypes && not inPrologue) $
    fail "strict_types declaration must be the very first statement in the script"
  when (any isEncodingDirective directives && not inPrologue) $
    fail "Encoding declaration pragma must be the very first statement in the script"
  bodyBranch directives
  where
    parseDeclareDirective = withSpan $ do
      name <- identifier
      _ <- symbol "="
      val <- parseExpr
      validateDirectiveValue name val
      pure (\sp -> DeclareDirective sp name val)

    validateDirectiveValue name val
      | isStrictTypesName name =
          unless (isStrictTypesValue val) $
            fail "strict_types declaration must have 0 or 1 as its value"
      | isEncodingName name =
          unless (isEncodingValue val) $
            fail "Encoding must be a literal"
      | otherwise =
          unless (isLiteralValue val) $
            fail ("declare(" <> T.unpack (directiveName name) <> ") value must be a literal")

    -- PHP folds a concatenation of literals into a single literal while
    -- parsing, so the encoding value may be any literal or chain of them.
    isEncodingValue (ExprLit _ _) = True
    isEncodingValue (ExprBinary _ OpConcat l r) = isEncodingValue l && isEncodingValue r
    isEncodingValue _ = False

    isLiteralValue (ExprLit _ _) = True
    isLiteralValue _ = False

    isEncodingName (Ident _ name) = T.toLower name == "encoding"

    directiveName (Ident _ name) = name

    bodyBranch dirs =
      -- Semicolon or close tag (e.g. declare(strict_types=1);)
      (statementTerminator *> pure (\sp -> StmtDeclare sp dirs Nothing))
      -- Alternative syntax (declare(...): ... enddeclare;)
      <|> (do
        rejectStrictTypesBody dirs
        _ <- colon
        stmts <- parseAltBody
        keyword_ "enddeclare"
        _ <- statementTerminator
        pure (\sp -> StmtDeclare sp dirs (Just stmts)))
      -- Brace block (declare(...) { ... })
      <|> (do
        rejectStrictTypesBody dirs
        stmts <- braces parseMixedBody
        pure (\sp -> StmtDeclare sp dirs (Just stmts)))
      -- Single statement (declare(...) stmt)
      <|> (do
        rejectStrictTypesBody dirs
        s <- parseStmt
        pure (\sp -> StmtDeclare sp dirs (Just [s])))

    rejectStrictTypesBody dirs =
      when (any isStrictTypesDirective dirs) $
        fail "strict_types declaration must not use block mode"

    isStrictTypesDirective (DeclareDirective _ name _) = isStrictTypesName name

    isEncodingDirective (DeclareDirective _ name _) = isEncodingName name

    isStrictTypesName (Ident _ name) = T.toLower name == "strict_types"

    isStrictTypesValue (ExprLit _ (LitInt _ value _)) = value == 0 || value == 1
    isStrictTypesValue _ = False

-- | Goto statement: goto label;
parseGoto :: Parser (Stmt Span)
parseGoto = withSpan $ do
  keyword_ "goto"
  lbl <- identifier
  _ <- statementTerminator
  pure (\sp -> StmtGoto sp lbl)

-- | Label statement: label:
parseLabel :: Parser (Stmt Span)
parseLabel = withSpan $ M.try $ do
  lbl <- identifier
  _ <- lexeme (C.char ':' <* M.notFollowedBy (C.char ':'))
  pure (\sp -> StmtLabel sp lbl)

-- | Unset statement: unset($a, $b['k']);
parseUnset :: Parser (Stmt Span)
parseUnset = withSpan $ do
  keyword_ "unset"
  targets <- parens (parseExpr `M.sepEndBy1` comma)
  unless (all isWritable targets) $
    fail "Cannot unset a non-writable expression"
  _ <- statementTerminator
  pure (\sp -> StmtUnset sp targets)

-- | Halt compilation and capture the remainder of the file as payload.
parseHaltCompiler :: Parser (Stmt Span)
parseHaltCompiler = withSpan $ do
  keyword_ "__halt_compiler"
  _ <- symbol "("
  _ <- symbol ")"
  _ <- C.char ';'
  payload <- M.takeRest
  pure (\sp -> StmtHaltCompiler sp payload)

-- | Expression statement (expr ;).
parseExprStmt :: Parser (Stmt Span)
parseExprStmt = withSpan $ do
  expr <- parseExpr
  _ <- statementTerminator
  pure (\sp -> StmtExpr sp expr)

-- | Empty statement (;).
parseEmptyStmt :: Parser (Stmt Span)
parseEmptyStmt = withSpan $ do
  _ <- semi
  pure (\sp -> StmtEmpty sp)

-- | Block statement { ... }.
parseBlock :: Parser (Stmt Span)
parseBlock = withSpan $ do
  stmts <- braces parseMixedBody
  pure (\sp -> StmtBlock sp stmts)

-- | Echo statement.
parseEcho :: Parser (Stmt Span)
parseEcho = withSpan $ do
  keyword_ "echo"
  exprs <- parseExpr `M.sepBy1` comma
  _ <- statementTerminator
  pure (\sp -> StmtEcho sp exprs)

-- | Global statement.
parseGlobal :: Parser (Stmt Span)
parseGlobal = withSpan $ do
  keyword_ "global"
  vars <- parseExpr `M.sepBy1` comma
  _ <- statementTerminator
  pure (\sp -> StmtGlobal sp vars)

-- | Static variable declaration in function: static $a = 1, $b;
parseStaticStmt :: Parser (Stmt Span)
parseStaticStmt = withSpan $ M.try $ do
  keyword_ "static"
  items <- parseStaticItem `M.sepBy1` comma
  _ <- statementTerminator
  pure (\sp -> StmtStatic sp items)
  where
    parseStaticItem = do
      var <- variableName
      mDef <- optional (symbol "=" *> parseExpr)
      pure (var, mDef)

-- | Break statement.
parseBreak :: Parser (Stmt Span)
parseBreak = withSpan $ do
  keyword_ "break"
  mNum <- optional parseExpr
  _ <- statementTerminator
  pure (\sp -> StmtBreak sp mNum)

-- | Continue statement.
parseContinue :: Parser (Stmt Span)
parseContinue = withSpan $ do
  keyword_ "continue"
  mNum <- optional parseExpr
  _ <- statementTerminator
  pure (\sp -> StmtContinue sp mNum)

-- | Return statement.
parseReturn :: Parser (Stmt Span)
parseReturn = withSpan $ do
  keyword_ "return"
  mExpr <- optional parseExpr
  _ <- statementTerminator
  pure (\sp -> StmtReturn sp mExpr)

-- | Throw statement.
parseThrowStmt :: Parser (Stmt Span)
parseThrowStmt = withSpan $ do
  keyword_ "throw"
  expr <- parseExpr
  _ <- statementTerminator
  pure (\sp -> StmtThrowStmt sp expr)

-- | If statement (supports if (...) ... elseif (...) ... else ...),
-- including the alternative (colon/keyword) syntax.
parseIf :: Parser (Stmt Span)
parseIf = withSpan $ do
  keyword_ "if"
  cond <- parens parseExpr
  altBranch cond <|> braceBranch cond
  where
    -- if (c): ... elseif (c2): ... else: ... endif;
    altBranch cond = do
      _ <- colon
      thens <- parseAltBody
      elifs <- M.many parseAltElseIf
      mElse <- optional parseAltElse
      keyword_ "endif"
      _ <- statementTerminator
      pure (\sp -> StmtIf sp cond thens elifs mElse)
    -- if (c) ... elseif (c2) ... else ...
    braceBranch cond = do
      thenStmts <- parseStmtBody
      elifs <- M.many parseElseIf
      mElse <- optional parseElse
      pure (\sp -> StmtIf sp cond thenStmts elifs mElse)

    parseElseIf = do
      keyword_ "elseif" <|> M.try (keyword_ "else" *> keyword_ "if")
      c <- parens parseExpr
      body <- parseStmtBody
      pure (c, body)

    parseElse = do
      keyword_ "else"
      parseStmtBody

    parseAltElseIf = do
      keyword_ "elseif"
      c <- parens parseExpr
      _ <- colon
      body <- parseAltBody
      pure (c, body)

    parseAltElse = do
      keyword_ "else"
      _ <- colon
      parseAltBody

    parseStmtBody =
      (braces parseMixedBody)
      <|> ((\s -> [s]) <$> parseStmt)

-- | Body of an alternative-syntax control structure. It ends just before
-- the terminator keyword (endif/endwhile/...).
parseAltBody :: Parser [Stmt Span]
parseAltBody = parseMixedBody

-- | While loop, including the alternative (colon/keyword) syntax.
parseWhile :: Parser (Stmt Span)
parseWhile = withSpan $ do
  keyword_ "while"
  cond <- parens parseExpr
  altBranch cond <|> braceBranch cond
  where
    altBranch cond = do
      _ <- colon
      body <- parseAltBody
      keyword_ "endwhile"
      _ <- statementTerminator
      pure (\sp -> StmtWhile sp cond body)
    braceBranch cond = do
      body <- (braces parseMixedBody) <|> ((\s -> [s]) <$> parseStmt)
      pure (\sp -> StmtWhile sp cond body)

-- | Do-While loop.
parseDoWhile :: Parser (Stmt Span)
parseDoWhile = withSpan $ do
  keyword_ "do"
  body <- (braces parseMixedBody) <|> ((\s -> [s]) <$> parseStmt)
  keyword_ "while"
  cond <- parens parseExpr
  _ <- semi
  pure (\sp -> StmtDoWhile sp body cond)

-- | For loop, including the alternative (colon/keyword) syntax.
parseFor :: Parser (Stmt Span)
parseFor = withSpan $ do
  keyword_ "for"
  _ <- symbol "("
  inits <- parseExpr `M.sepBy` comma
  _ <- semi
  conds <- parseExpr `M.sepBy` comma
  _ <- semi
  incrs <- parseExpr `M.sepBy` comma
  _ <- symbol ")"
  altBranch inits conds incrs <|> braceBranch inits conds incrs
  where
    altBranch inits conds incrs = do
      _ <- colon
      body <- parseAltBody
      keyword_ "endfor"
      _ <- statementTerminator
      pure (\sp -> StmtFor sp inits conds incrs body)
    braceBranch inits conds incrs = do
      body <- (braces parseMixedBody) <|> ((\s -> [s]) <$> parseStmt)
      pure (\sp -> StmtFor sp inits conds incrs body)

-- | Foreach loop, including the alternative (colon/keyword) syntax.
parseForeach :: Parser (Stmt Span)
parseForeach = withSpan $ do
  keyword_ "foreach"
  _ <- symbol "("
  arr <- parseExpr
  keyword_ "as"
  hasLeadingRef <- (True <$ symbol "&") <|> pure False
  (mKey, val, byRef) <- if hasLeadingRef
    then do
      v <- parseExpr
      pure (Nothing, v, True)
    else do
      kOrV <- parseExpr
      isArrow <- (True <$ symbol "=>") <|> pure False
      if isArrow
        then do
          byRef <- (True <$ symbol "&") <|> pure False
          v <- parseExpr
          pure (Just kOrV, v, byRef)
        else pure (Nothing, kOrV, False)
  when (hasEmptyDestructure val || maybe False hasEmptyDestructure mKey) $
    fail "Cannot use empty list"
  -- The key and a by-reference value bind to single variables; a by-value
  -- value may also destructure.  None may be a nullsafe chain.
  unless (maybe True isWritable mKey && (if byRef then isWritable val else isAssignable val)) $
    fail "Cannot assign foreach target to a non-writable expression"
  _ <- symbol ")"
  altBranch arr mKey val byRef <|> braceBranch arr mKey val byRef
  where
    altBranch arr mKey val byRef = do
      _ <- colon
      body <- parseAltBody
      keyword_ "endforeach"
      _ <- statementTerminator
      pure (\sp -> StmtForeach sp arr mKey val byRef body)
    braceBranch arr mKey val byRef = do
      body <- (braces parseMixedBody) <|> ((\s -> [s]) <$> parseStmt)
      pure (\sp -> StmtForeach sp arr mKey val byRef body)

-- | Switch statement, including the alternative (colon/keyword) syntax.
parseSwitch :: Parser (Stmt Span)
parseSwitch = withSpan $ do
  keyword_ "switch"
  expr <- parens parseExpr
  braceBranch expr <|> altBranch expr
  where
    braceBranch expr = do
      cases <- braces (M.many parseSwitchCase)
      checkSingleDefault cases
      pure (\sp -> StmtSwitch sp expr cases)
    altBranch expr = do
      _ <- colon
      cases <- concat <$> M.many ((\c -> [c]) <$> parseSwitchCaseWith parseAltBody)
      keyword_ "endswitch"
      _ <- statementTerminator
      checkSingleDefault cases
      pure (\sp -> StmtSwitch sp expr cases)

    checkSingleDefault cases =
      when (length [() | SwitchDefault {} <- cases] > 1) $
        fail "Switch statements may only contain one default clause"

parseSwitchCase :: Parser (SwitchCase Span)
parseSwitchCase = parseSwitchCaseWith parseMixedBody

parseSwitchCaseWith :: Parser [Stmt Span] -> Parser (SwitchCase Span)
parseSwitchCaseWith parseBody = parseDefault <|> parseCase
  where
    parseDefault = withSpan $ do
      keyword_ "default"
      _ <- colon <|> semi
      stmts <- parseBody
      pure (\sp -> SwitchDefault sp stmts)

    parseCase = withSpan $ do
      keyword_ "case"
      expr <- parseExpr
      _ <- colon <|> semi
      stmts <- parseBody
      pure (\sp -> SwitchCase sp expr stmts)

-- | Try-Catch-Finally (supports non-capturing catch). PHP requires at least one
-- @catch@ clause or a @finally@ block.
parseTry :: Parser (Stmt Span)
parseTry = withSpan $ do
  keyword_ "try"
  body <- braces parseMixedBody
  catches <- M.many parseCatch
  mFinally <- optional (keyword "finally" *> braces parseMixedBody)
  when (null catches && isNothing mFinally) $
    fail "cannot use try without catch or finally"
  pure (\sp -> StmtTry sp body catches mFinally)
  where
    parseCatch = withSpan $ do
      keyword_ "catch"
      _ <- symbol "("
      types <- qualifiedName `M.sepBy1` symbol "|"
      mVar <- optional variableName
      _ <- symbol ")"
      catBody <- braces parseMixedBody
      pure (\sp -> CatchClause sp types mVar catBody)

-- | Namespace declaration (bracketed or unbracketed).
parseNamespace :: Parser (Stmt Span)
parseNamespace = withSpan $ do
  (mName, form) <- M.try $ do
    keyword_ "namespace"
    mName <- optional parseNamespaceName
    case mName of
      Nothing -> do
        _ <- lookAhead (symbol "{")
        pure (Nothing, BracketedNamespace)
      Just _ -> do
        form <- (BracketedNamespace <$ lookAhead (symbol "{")) <|> (UnbracketedNamespace <$ semi)
        pure (mName, form)
  enterNamespace form
  case form of
    BracketedNamespace -> do
      stmts <- braces parseMixedBody
      pure (\sp -> StmtNamespace sp mName (Just stmts))
    UnbracketedNamespace -> pure (\sp -> StmtNamespace sp mName Nothing)
  where
    parseNamespaceName = M.try $ do
      qn@(QualifiedName _ kind _) <- qualifiedName
      case kind of
        NameUnqualified -> pure qn
        NameQualified -> pure qn
        _ -> M.empty

-- | Use imports (standard, grouped, function, const).
parseUse :: Parser (Stmt Span)
parseUse = M.try parseGroupUse <|> parseNormalUse
  where
    parseUseType =
      (UseFunction <$ keyword "function")
      <|> (UseConst <$ keyword "const")
      <|> pure UseNormal

    parseNormalUse = withSpan $ M.try $ do
      keyword_ "use"
      ut <- parseUseType
      clauses <- parseUseClause `M.sepBy1` comma
      _ <- semi
      pure (\sp -> StmtUse sp ut clauses)

    parseGroupUse = withSpan $ do
      keyword_ "use"
      ut <- parseUseType
      prefix <- qualifiedName
      _ <- symbol "\\"
      clauses <- braces (parseGroupUseClause `M.sepEndBy1` comma)
      _ <- semi
      pure (\sp -> StmtGroupUse sp ut prefix clauses)

    parseClauseType =
      (Just UseFunction <$ keyword "function")
      <|> (Just UseConst <$ keyword "const")
      <|> pure Nothing

    parseUseClause = parseUseClauseWith (pure Nothing)

    parseGroupUseClause = parseUseClauseWith parseClauseType

    parseUseClauseWith typeParser = withSpan $ do
      mType <- typeParser
      qn <- qualifiedName
      mAlias <- optional (keyword "as" *> identifier)
      pure (\sp -> UseClause sp qn mAlias mType)



-- | Visibility & Asymmetric visibility (PHP 8.4/8.5).
parseVisibility :: Parser Visibility
parseVisibility =
  (Public <$ keyword "public")
  <|> (Protected <$ keyword "protected")
  <|> (Private <$ keyword "private")

-- | Asymmetric write visibility e.g. private(set), protected(set).
parseAsymmetricWriteVis :: Parser Visibility
parseAsymmetricWriteVis = M.try $ do
  vis <- parseVisibility
  _ <- symbol "(set)"
  pure vis

data ModifierPresence = ModifierAbsent | ModifierSeen

data ModifierProblem
  = DuplicateModifier String
  | ConflictingModifiers String String

registerModifierProblem :: ModifierProblem -> Parser ()
registerModifierProblem (DuplicateModifier what) =
  modifierError ("Multiple " <> what <> " modifiers are not allowed")
registerModifierProblem (ConflictingModifiers this that) =
  modifierError ("Cannot combine the " <> this <> " and " <> that <> " modifiers")

modifierSeen :: ModifierPresence -> Bool
modifierSeen ModifierAbsent = False
modifierSeen ModifierSeen = True

repeatedModifier :: String -> ModifierPresence -> [ModifierProblem]
repeatedModifier what presence = [DuplicateModifier what | modifierSeen presence]

repeatedVisibility :: Maybe Visibility -> [ModifierProblem]
repeatedVisibility vis = [DuplicateModifier "access type" | isJust vis]

modifierConflict :: String -> String -> ModifierPresence -> [ModifierProblem]
modifierConflict this that presence = [ConflictingModifiers this that | modifierSeen presence]

collectModifiers
  :: Parser token
  -> (token -> history -> (history, [ModifierProblem]))
  -> history
  -> Parser history
collectModifiers tokenParser step = loop
  where
    loop history =
      (do
        token <- tokenParser
        let (next, problems) = step token history
        mapM_ registerModifierProblem problems
        loop next)
      <|> pure history

modifierError :: String -> Parser ()
modifierError = M.registerFancyFailure . S.singleton . M.ErrorFail

-- | Returns True if the first visibility is strictly weaker than the second.
-- In PHP 8.4 asymmetric visibility, read visibility cannot be weaker than write visibility.
-- Ordering from weakest to strongest: Private < Protected < Public.
isWeakerVisibility :: Visibility -> Visibility -> Bool
isWeakerVisibility Private Protected = True
isWeakerVisibility Private Public    = True
isWeakerVisibility Protected Public  = True
isWeakerVisibility _ _               = False

checkVisibilityOrdering :: Maybe Visibility -> Maybe Visibility -> Parser ()
checkVisibilityOrdering (Just v) (Just wv)
  | isWeakerVisibility v wv =
      modifierError "Visibility of property must not be weaker than set visibility"
checkVisibilityOrdering _ _ = pure ()

data PropertyModifierToken
  = PropertyWriteVisibility Visibility
  | PropertyReadVisibility Visibility
  | PropertyStatic
  | PropertyReadonly
  | PropertyFinal
  | PropertyAbstract

data PropertyModifierHistory = PropertyModifierHistory
  (Maybe Visibility) (Maybe Visibility) ModifierPresence ModifierPresence ModifierPresence ModifierPresence

stepPropertyModifier
  :: PropertyModifierToken -> PropertyModifierHistory
  -> (PropertyModifierHistory, [ModifierProblem])
stepPropertyModifier token (PropertyModifierHistory vis wVis stat ro fin absMod) = case token of
  PropertyWriteVisibility v ->
    (PropertyModifierHistory vis (Just v) stat ro fin absMod,
     repeatedVisibility wVis)
  PropertyReadVisibility v ->
    (PropertyModifierHistory (Just v) wVis stat ro fin absMod,
     repeatedVisibility vis)
  PropertyStatic ->
    (PropertyModifierHistory vis wVis ModifierSeen ro fin absMod,
     repeatedModifier "static" stat ++ modifierConflict "static" "readonly" ro)
  PropertyReadonly ->
    (PropertyModifierHistory vis wVis stat ModifierSeen fin absMod,
     repeatedModifier "readonly" ro ++ modifierConflict "static" "readonly" stat)
  PropertyFinal ->
    (PropertyModifierHistory vis wVis stat ro ModifierSeen absMod,
     repeatedModifier "final" fin)
  PropertyAbstract ->
    (PropertyModifierHistory vis wVis stat ro fin ModifierSeen,
     repeatedModifier "abstract" absMod)

-- | Property modifiers (can be in any order: public, private(set), readonly, static, final, abstract),
-- or the legacy @var@ on its own.
-- Note: @var@ is an alias for @public@ visibility, but PHP's grammar takes it
-- as the whole modifier list, so it cannot be combined with any other modifier
-- in either order.
-- At least one modifier is required: PHP has no modifierless property, so a
-- bare @$x = 1;@ in a class or trait body is a syntax error, not a property.
parsePropertyModifier :: Parser PropertyModifier
parsePropertyModifier =
  (PropertyModifier (Just Public) Nothing False False False False <$ keyword_ "var")
  <|> modifierList
  where
    modifierList = do
      PropertyModifierHistory vis wVis stat ro fin absMod <- collectModifiers token stepPropertyModifier
        (PropertyModifierHistory Nothing Nothing ModifierAbsent ModifierAbsent ModifierAbsent ModifierAbsent)
      let modif = PropertyModifier vis wVis (modifierSeen stat) (modifierSeen ro) (modifierSeen fin) (modifierSeen absMod)
      when (modif == PropertyModifier Nothing Nothing False False False False) M.empty
      checkVisibilityOrdering vis wVis
      pure modif
    token =
      (PropertyWriteVisibility <$> parseAsymmetricWriteVis)
      <|> (PropertyReadVisibility <$> parseVisibility)
      <|> (PropertyStatic <$ keyword_ "static")
      <|> (PropertyReadonly <$ keyword_ "readonly")
      <|> (PropertyFinal <$ keyword_ "final")
      <|> (PropertyAbstract <$ keyword_ "abstract")

data MethodModifierToken
  = MethodReadVisibility Visibility
  | MethodStatic
  | MethodFinal
  | MethodAbstract

data MethodModifierHistory = MethodModifierHistory
  (Maybe Visibility) ModifierPresence ModifierPresence ModifierPresence

stepMethodModifier
  :: MethodModifierToken -> MethodModifierHistory
  -> (MethodModifierHistory, [ModifierProblem])
stepMethodModifier token (MethodModifierHistory vis stat fin absMod) = case token of
  MethodReadVisibility v ->
    (MethodModifierHistory (Just v) stat fin absMod,
     repeatedVisibility vis)
  MethodStatic ->
    (MethodModifierHistory vis ModifierSeen fin absMod,
     repeatedModifier "static" stat)
  MethodFinal ->
    (MethodModifierHistory vis stat ModifierSeen absMod,
     repeatedModifier "final" fin ++ modifierConflict "final" "abstract" absMod)
  MethodAbstract ->
    (MethodModifierHistory vis stat fin ModifierSeen,
     repeatedModifier "abstract" absMod ++ modifierConflict "final" "abstract" fin)

-- | Method modifiers.
parseMethodModifier :: Parser MethodModifier
parseMethodModifier = do
  MethodModifierHistory vis stat fin absMod <- collectModifiers token stepMethodModifier
    (MethodModifierHistory Nothing ModifierAbsent ModifierAbsent ModifierAbsent)
  pure (MethodModifier vis (modifierSeen stat) (modifierSeen fin) (modifierSeen absMod))
  where
    token =
      (MethodReadVisibility <$> parseVisibility)
      <|> (MethodStatic <$ keyword_ "static")
      <|> (MethodFinal <$ keyword_ "final")
      <|> (MethodAbstract <$ keyword_ "abstract")

data ClassModifierToken
  = ClassFinal
  | ClassAbstract
  | ClassReadonly

data ClassModifierHistory = ClassModifierHistory
  ModifierPresence ModifierPresence ModifierPresence

stepClassModifier
  :: ClassModifierToken -> ClassModifierHistory
  -> (ClassModifierHistory, [ModifierProblem])
stepClassModifier token (ClassModifierHistory fin absMod ro) = case token of
  ClassFinal ->
    (ClassModifierHistory ModifierSeen absMod ro,
     repeatedModifier "final" fin ++ modifierConflict "final" "abstract" absMod)
  ClassAbstract ->
    (ClassModifierHistory fin ModifierSeen ro,
     repeatedModifier "abstract" absMod ++ modifierConflict "final" "abstract" fin)
  ClassReadonly ->
    (ClassModifierHistory fin absMod ModifierSeen,
     repeatedModifier "readonly" ro)

-- | Class modifiers.
parseClassModifier :: Parser ClassModifier
parseClassModifier = do
  ClassModifierHistory fin absMod ro <- collectModifiers token stepClassModifier
    (ClassModifierHistory ModifierAbsent ModifierAbsent ModifierAbsent)
  pure (ClassModifier (modifierSeen fin) (modifierSeen absMod) (modifierSeen ro))
  where
    token =
      (ClassFinal <$ keyword_ "final")
      <|> (ClassAbstract <$ keyword_ "abstract")
      <|> (ClassReadonly <$ keyword_ "readonly")

data ConstModifierToken
  = ConstReadVisibility Visibility
  | ConstFinal

data ConstModifierHistory = ConstModifierHistory
  (Maybe Visibility) ModifierPresence

stepConstModifier
  :: ConstModifierToken -> ConstModifierHistory
  -> (ConstModifierHistory, [ModifierProblem])
stepConstModifier token (ConstModifierHistory vis fin) = case token of
  ConstReadVisibility v ->
    (ConstModifierHistory (Just v) fin,
     repeatedVisibility vis)
  ConstFinal ->
    (ConstModifierHistory vis ModifierSeen,
     repeatedModifier "final" fin)

-- | Class constant modifiers (visibility and final in any order).
parseConstModifier :: Parser (Maybe Visibility, Bool)
parseConstModifier = do
  ConstModifierHistory vis fin <- collectModifiers token stepConstModifier
    (ConstModifierHistory Nothing ModifierAbsent)
  pure (vis, modifierSeen fin)
  where
    token =
      (ConstReadVisibility <$> parseVisibility)
      <|> (ConstFinal <$ keyword_ "final")

-- | Constant item: name and initial value expression.
parseConstItem :: Parser (Ident Span, Expr Span)
parseConstItem = do
  name <- classConstantIdentifier
  _ <- symbol "="
  val <- parseExpr
  pure (name, val)

-- | Class member constant declaration (supports visibility, final, and typed constants PHP 8.3).
parseConstDecl :: Parser (ConstDecl Span)
parseConstDecl = withSpan $ do
  (attrs, vis, isFinal) <- M.try $ do
    attrs <- parseAttributes
    (vis, isFinal) <- parseConstModifier
    keyword_ "const"
    pure (attrs, vis, isFinal)
  mType <- optional (M.try (parseType <* lookAhead classConstantIdentifier))
  case mType of
    Just typ | Just bad <- disallowedConstantType typ ->
      modifierError ("Class constant cannot have type " <> T.unpack bad)
    _ -> pure ()
  items <- parseConstItem `M.sepBy1` comma
  _ <- semi
  pure (\sp -> ConstDecl sp attrs vis isFinal mType items)

-- | Whether an initializer contains a @new@ expression outside any closure
-- body. PHP permits @new@ in global constant initializers but not in class,
-- interface, trait or enum constants. 'queryExpr' stops at the first
-- non-'mempty' node, so a closure answers @Just (Any False)@ to leave its
-- body (a separate scope) unvisited.
containsNew :: Expr a -> Bool
containsNew = maybe False getAny . queryExpr (\case
  ExprNew {}             -> Just (Any True)
  ExprNewAnonClass {}    -> Just (Any True)
  ExprClosure {}         -> Just (Any False)
  ExprArrowFunction {}   -> Just (Any False)
  _                      -> Nothing)

-- | Top-level constant declaration statement.
-- Unlike class member constants, global constants forbid visibility modifiers,
-- 'final', and type annotations.
parseConstStmt :: Parser (Stmt Span)
parseConstStmt = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "const")
  items <- parseConstItem `M.sepBy1` comma
  _ <- semi
  pure (\sp -> StmtConst sp (ConstDecl sp attrs Nothing False Nothing items))

-- | Function declaration.
parseFunction :: Parser (Stmt Span)
parseFunction = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "function" <* M.notFollowedBy (symbol "("))
  byRef <- (True <$ symbol "&") <|> pure False
  name <- identifier
  params <- parens (parseParamList (parseParamInContext NonConstructorParam))
  retType <- parseReturnType
  body <- braces parseMixedBody
  pure (\sp -> StmtFunction sp (FunctionDecl sp attrs byRef name params retType body))

-- | Promoted parameter context for validating parameter modifiers.
data ParamContext
  = ConstructorParam
  | AbstractConstructorParam
  | NonConstructorParam
  deriving (Eq, Show)

-- | Whether a parameter declares promoted property modifiers.
isPromotedParam :: Param a -> Bool
isPromotedParam p = isJust (paramVis p) || isJust (paramWriteVis p) || paramReadonly p || paramFinal p

-- | Parameter parsing (supports constructor property promotion & asymmetric visibility).
parseParam :: Parser (Param Span)
parseParam = parseParamInContext ConstructorParam

parseParamInContext :: ParamContext -> Parser (Param Span)
parseParamInContext pCtx = withSpan $ do
  attrs <- parseAttributes
  (vis, wVis, isRo, isFin) <- parseParamModifiers
  let isPromoted = isJust vis || isJust wVis || isRo
  when (isFin && not isPromoted) $
    modifierError "Cannot use the 'final' modifier on a non-promoted parameter"
  when isPromoted $ case pCtx of
    NonConstructorParam ->
      modifierError "Cannot declare promoted property outside a constructor"
    AbstractConstructorParam ->
      modifierError "Cannot declare promoted property in an abstract constructor"
    ConstructorParam ->
      pure ()
  typ <- optional parseType
  byRef <- (True <$ symbol "&") <|> pure False
  isVariadic <- (True <$ symbol "...") <|> pure False
  when (isPromoted && isVariadic) $
    modifierError "Cannot declare variadic promoted property"
  when (isRo && isNothing typ) $
    modifierError "Readonly property must have type"
  when (isJust wVis && isNothing typ) $
    modifierError "Property with asymmetric visibility must have type"
  when isPromoted $
    case typ of
      Just t | Just bad <- disallowedPropertyType t ->
        modifierError ("Property cannot have type " <> T.unpack bad)
      _ -> pure ()
  var <- variableName
  mDef <- optional (symbol "=" *> parseExpr)
  pure (\sp -> Param sp attrs vis wVis isRo isFin typ byRef isVariadic var mDef)

data ParamModifierToken
  = ParamWriteVisibility Visibility
  | ParamReadVisibility Visibility
  | ParamReadonly
  | ParamFinal

data ParamModifierHistory = ParamModifierHistory
  (Maybe Visibility) (Maybe Visibility) ModifierPresence ModifierPresence

stepParamModifier
  :: ParamModifierToken -> ParamModifierHistory
  -> (ParamModifierHistory, [ModifierProblem])
stepParamModifier token (ParamModifierHistory vis wVis ro fin) = case token of
  ParamWriteVisibility v ->
    (ParamModifierHistory vis (Just v) ro fin,
     repeatedVisibility wVis)
  ParamReadVisibility v ->
    (ParamModifierHistory (Just v) wVis ro fin,
     repeatedVisibility vis)
  ParamReadonly ->
    (ParamModifierHistory vis wVis ModifierSeen fin,
     repeatedModifier "readonly" ro)
  ParamFinal ->
    (ParamModifierHistory vis wVis ro ModifierSeen,
     repeatedModifier "final" fin)

parseParamModifiers :: Parser (Maybe Visibility, Maybe Visibility, Bool, Bool)
parseParamModifiers = do
  ParamModifierHistory vis wVis ro fin <- collectModifiers token stepParamModifier
    (ParamModifierHistory Nothing Nothing ModifierAbsent ModifierAbsent)
  checkVisibilityOrdering vis wVis
  pure (vis, wVis, modifierSeen ro, modifierSeen fin)
  where
    token =
      (ParamWriteVisibility <$> parseAsymmetricWriteVis)
      <|> (ParamReadVisibility <$> parseVisibility)
      <|> (ParamReadonly <$ keyword_ "readonly")
      <|> (ParamFinal <$ keyword_ "final")

-- | The enclosing declaration kind in which class members are parsed.
-- PHP applies different member rules to enums, classes, traits, and interfaces.
data ClassContext
  = ClassContext !T.Text !Bool     -- ^ class name, readonly flag
  | AnonClassContext !Bool         -- ^ anonymous class; readonly flag
  | TraitContext                   -- ^ trait
  | EnumContext !T.Text !Bool      -- ^ enum name, backed flag
  | InterfaceContext !T.Text       -- ^ interface name
  deriving (Eq, Show)

-- | Class member declaration.
parseClassMember :: Parser (ClassMember Span)
parseClassMember = parseClassMemberInContext (ClassContext "" False)

parseClassMemberInContext :: ClassContext -> Parser (ClassMember Span)
parseClassMemberInContext ctx = do
  enclosingReadonly <- pure $ case ctx of
    ClassContext _ ro -> ro
    AnonClassContext ro -> ro
    _ -> False
  member <-
    (MemberConst <$> M.try parseConstDecl)
    <|> (MemberTraitUse <$> M.try parseTraitUse)
    <|> (MemberEnumCase <$> M.try parseEnumCase)
    <|> parseMethodOrProperty ctx enclosingReadonly
  member <$ checkMember ctx member

-- | Reject members that PHP forbids in the enclosing declaration kind.
-- Hooked properties are legal in interfaces (PHP 8.4); bare ones are not.
checkMember :: ClassContext -> ClassMember Span -> Parser ()
checkMember ctx member = do
  case member of
    MemberConst cd
      | any (containsNew . snd) (constItems cd) ->
          forbidden "New expressions are not supported in this context"
    _ -> pure ()
  case (ctx, member) of
    (EnumContext enumName isBacked, MemberEnumCase (EnumCase _ _ (Ident _ caseName) mVal)) -> do
      when (not isBacked && isJust mVal) $
        forbidden ("Case " <> caseName <> " of non-backed enum " <> enumName <> " must not have a value")
      when (isBacked && isNothing mVal) $
        forbidden ("Case " <> caseName <> " of backed enum " <> enumName <> " must have a value")
    (EnumContext _ _, MemberProperty _) -> forbidden "Enums may not include properties"
    (EnumContext _ _, MemberMethod md)
      | any isPromotedParam (methodParams md) -> forbidden "Enums may not include properties"
    (InterfaceContext ifaceName, MemberConst cd) ->
      case constVis cd of
        Just Private ->
          case constItems cd of
            (Ident _ constName, _) : _ ->
              forbidden ("Access type for interface constant " <> ifaceName <> "::" <> constName <> " must be public")
            [] -> pure ()
        Just Protected ->
          case constItems cd of
            (Ident _ constName, _) : _ ->
              forbidden ("Access type for interface constant " <> ifaceName <> "::" <> constName <> " must be public")
            [] -> pure ()
        _ -> pure ()
    (InterfaceContext ifaceName, MemberMethod md) -> do
      let Ident _ mName = methodName md
          modif = methodModifier md
      when (methodAbstract modif) $
        forbidden ("Interface method " <> ifaceName <> "::" <> mName <> "() must not be abstract")
      when (methodVis modif == Just Private || methodVis modif == Just Protected) $
        forbidden ("Access type for interface method " <> ifaceName <> "::" <> mName <> "() must be public")
      when (methodFinal modif) $
        forbidden ("Interface method " <> ifaceName <> "::" <> mName <> "() must not be final")
    (InterfaceContext _, MemberProperty pd)
      | null (propHooks pd) -> forbidden "Interfaces may not include properties"
      | any (\h -> case hookBody h of HookAbstract -> False; _ -> True) (propHooks pd) ->
          forbidden "Abstract property hook cannot have body"
      | any hookFinal (propHooks pd) ->
          forbidden "Property hook cannot be both abstract and final"
    (InterfaceContext _, MemberTraitUse _) -> forbidden "Cannot use traits inside of interfaces"
    (ClassContext className ro, MemberMethod md) -> do
      let Ident _ mName = methodName md
          modif = methodModifier md
          target = if T.null className then mName else className <> "::" <> mName
      when (methodAbstract modif && methodVis modif == Just Private) $
        forbidden ("Abstract function " <> target <> "() cannot be declared private")
      when (ro && any (\p -> isPromotedParam p && isNothing (paramType p)) (methodParams md)) $
        forbidden "Readonly classes cannot declare untyped properties"
    (AnonClassContext True, MemberMethod md) -> do
      when (any (\p -> isPromotedParam p && isNothing (paramType p)) (methodParams md)) $
        forbidden "Readonly classes cannot declare untyped properties"
    (_, MemberProperty pd)
      | propReadonly (propModifier pd) && isNothing (propType pd) ->
          forbidden "Readonly property must have type"
    (ctx', MemberProperty pd)
      | isReadonlyCtx ctx' -> do
          when (propStatic (propModifier pd)) $
            forbidden "Readonly classes cannot declare static properties"
          when (isNothing (propType pd)) $
            forbidden "Readonly classes cannot declare untyped properties"
    (_, MemberEnumCase _) -> forbidden "Enum cases can only be used inside enum declarations"
    _ -> pure ()
  where
    forbidden msg = M.fancyFailure (S.singleton (M.ErrorFail (T.unpack msg)))
    isReadonlyCtx (ClassContext _ ro) = ro
    isReadonlyCtx (AnonClassContext ro) = ro
    isReadonlyCtx _ = False

parseMethodOrProperty :: ClassContext -> Bool -> Parser (ClassMember Span)
parseMethodOrProperty ctx enclosingReadonly = do
  attrs <- parseAttributes
  isMethod <- (True <$ lookAhead (M.try parseMethodLookAhead)) <|> pure False
  if isMethod
    then MemberMethod <$> parseMethod ctx attrs
    else MemberProperty <$> parseProperty isIface enclosingReadonly attrs
  where
    isIface = case ctx of InterfaceContext _ -> True; _ -> False
    parseMethodLookAhead = do
      _ <- parseMethodModifier
      _ <- optional (symbol "&")
      keyword_ "function"

parseMethod :: ClassContext -> [AttributeGroup Span] -> Parser (MethodDecl Span)
parseMethod ctx attrs = withSpan $ do
  modif <- parseMethodModifier
  keyword_ "function"
  byRef <- (True <$ symbol "&") <|> pure False
  name <- semiReservedIdentifier
  let Ident _ nameText = name
      isCtor = T.toLower nameText == "__construct"
      isIface = case ctx of InterfaceContext _ -> True; _ -> False
      isAbs = methodAbstract modif || isIface
      paramCtx
        | not isCtor = NonConstructorParam
        | isAbs      = AbstractConstructorParam
        | otherwise  = ConstructorParam
  params <- parens (parseParamList (parseParamInContext paramCtx))
  retType <- parseReturnType
  body <- if isAbs
    then semi *> pure Nothing
    else Just <$> braces parseMixedBody
  pure (\sp -> MethodDecl sp attrs modif byRef name params retType body)

-- | Property with optional PHP 8.4 hooks and asymmetric visibility.
--
-- Interface properties are implicitly abstract, so their hooks may omit a
-- body without the @abstract@ modifier.
parseProperty :: Bool -> Bool -> [AttributeGroup Span] -> Parser (PropertyDecl Span)
parseProperty implicitlyAbstract enclosingReadonly attrs = withSpan $ do
  modif <- parsePropertyModifier
  mType <- optional parseType
  when (isJust (propWriteVis modif) && isNothing mType) $
    modifierError "Property with asymmetric visibility must have type"
  case mType of
    Just typ | Just bad <- disallowedPropertyType typ ->
      modifierError ("Property cannot have type " <> T.unpack bad)
    _ -> pure ()
  firstVar <- variableName
  mFirstVal <- optional (symbol "=" *> parseExpr)
  hasHooks <- (True <$ lookAhead (symbol "{")) <|> pure False
  if hasHooks
    then if enclosingReadonly || propReadonly modif
      then M.empty
      else do
        when (propStatic modif) $
          M.fancyFailure (S.singleton (M.ErrorFail "Cannot declare hooks for static property"))
        hooks <- braces (parsePropertyHooks NoHooks)
        let hasAbstractHook = any (\h -> case hookBody h of HookAbstract -> True; _ -> False) hooks
        when (not (propAbstract modif || implicitlyAbstract) && hasAbstractHook) $
          modifierError "Non-abstract property hook must have a body"
        when (propAbstract modif && not hasAbstractHook) $
          modifierError "Abstract property must specify at least one abstract hook"
        pure (\sp -> PropertyDecl sp attrs modif mType [(firstVar, mFirstVal)] hooks)
    else do
      when (propAbstract modif) $
        modifierError "Only hooked properties may be declared abstract"
      restItems <- M.many (comma *> parseItem)
      _ <- semi
      pure (\sp -> PropertyDecl sp attrs modif mType ((firstVar, mFirstVal) : restItems) [])
  where
    parseItem = do
      var <- variableName
      mVal <- optional (symbol "=" *> parseExpr)
      pure (var, mVal)

    parsePropertyHooks seen =
      (M.lookAhead (C.char '}') *> pure [])
      <|> do
        hook <- parsePropertyHook
        let hookT = hookType hook
        next <- case stepHook hookT seen of
          Left duplicate ->
            M.fancyFailure (S.singleton (M.ErrorFail ("Cannot redeclare property hook \"" <> hookName duplicate <> "\"")))
          Right history -> pure history
        (hook :) <$> parsePropertyHooks next

    hookName HookGet = "get"
    hookName HookSet = "set"

data HookHistory = NoHooks | GetSeen | SetSeen | GetAndSetSeen

stepHook :: HookType -> HookHistory -> Either HookType HookHistory
stepHook HookGet NoHooks = Right GetSeen
stepHook HookGet SetSeen = Right GetAndSetSeen
stepHook HookGet _ = Left HookGet
stepHook HookSet NoHooks = Right SetSeen
stepHook HookSet GetSeen = Right GetAndSetSeen
stepHook HookSet _ = Left HookSet

-- | PHP 8.4 Property Hook: get => expr; or set(Type $v) { ... }
--
-- A hook takes no visibility modifier of its own; asymmetric visibility
-- belongs to the enclosing property declaration.
parsePropertyHook :: Parser (PropertyHook Span)
parsePropertyHook = withSpan $ do
  attrs <- parseAttributes
  isFinal <- (True <$ keyword "final") <|> pure False
  byRef <- (True <$ symbol "&") <|> pure False
  hookT <- (HookGet <$ keyword "get") <|> (HookSet <$ keyword "set")
  when (byRef && hookT == HookSet) $
    M.fancyFailure (S.singleton (M.ErrorFail "Only get property hooks may return by reference"))
  mParam <- if hookT == HookSet
    then optional (parens parseHookParam)
    else pure Nothing
  body <- parseHookBody
  pure (\sp -> PropertyHook sp attrs isFinal byRef hookT mParam body)
  where
    parseHookParam = do
      typ <- optional parseType
      var <- variableName
      pure (var, typ)

    parseHookBody =
      (do
        _ <- symbol "=>"
        expr <- parseExpr
        _ <- semi
        pure (HookExpr expr))
      <|> (HookBlock <$> braces parseMixedBody)
      <|> (HookAbstract <$ semi)

-- | Trait usage inside class.
parseTraitUse :: Parser (TraitUse Span)
parseTraitUse = withSpan $ do
  keyword_ "use"
  names <- qualifiedName `M.sepBy1` comma
  hasAdaptations <- (True <$ lookAhead (symbol "{")) <|> pure False
  if hasAdaptations
    then do
      adaptations <- braces (M.many parseTraitAdaptation)
      pure (\sp -> TraitUse sp names adaptations)
    else do
      _ <- semi
      pure (\sp -> TraitUse sp names [])

parseTraitAdaptation :: Parser (TraitAdaptation Span)
parseTraitAdaptation = parsePrecedence <|> parseAlias
  where
    parsePrecedence = withSpan $ M.try $ do
      trait <- qualifiedName
      _ <- doubleColon
      method <- identifier
      keyword_ "insteadof"
      otherTraits <- qualifiedName `M.sepBy1` comma
      _ <- semi
      pure (\sp -> TraitPrecedence sp trait method otherTraits)

    parseAlias = withSpan $ do
      mTrait <- optional (M.try (qualifiedName <* doubleColon))
      method <- identifier
      keyword_ "as"
      vis <- optional parseVisibility
      mNewName <- optional identifier
      _ <- semi
      pure (\sp -> TraitAlias sp mTrait method vis mNewName)

-- | Enum Case.
parseEnumCase :: Parser (EnumCase Span)
parseEnumCase = withSpan $ do
  attrs <- parseAttributes
  keyword_ "case"
  -- PHP permits semi-reserved keywords such as `new` as enum case names.
  name <- classConstantIdentifier
  mVal <- optional (symbol "=" *> parseExpr)
  _ <- semi
  pure (\sp -> EnumCase sp attrs name mVal)

-- | Class declaration.
parseClass :: Parser (Stmt Span)
parseClass = withSpan $ do
  (attrs, modif) <- M.try $ do
    attrs <- parseAttributes
    modif <- parseClassModifier
    keyword_ "class"
    pure (attrs, modif)
  name@(Ident _ className) <- declarationIdentifier
  mExtends <- optional (keyword "extends" *> qualifiedName)
  impls <- (keyword "implements" *> (qualifiedName `M.sepBy1` comma)) <|> pure []
  members <- braces (M.many (parseClassMemberInContext (ClassContext className (classReadonly modif))))
  pure (\sp -> StmtClass sp (ClassDecl sp attrs modif name mExtends impls members))

-- | Interface declaration.
parseInterface :: Parser (Stmt Span)
parseInterface = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "interface")
  name@(Ident _ ifaceName) <- declarationIdentifier
  extends <- (keyword "extends" *> (qualifiedName `M.sepBy1` comma)) <|> pure []
  members <- braces (M.many (parseClassMemberInContext (InterfaceContext ifaceName)))
  pure (\sp -> StmtInterface sp (InterfaceDecl sp attrs name extends members))

-- | Trait declaration.
parseTrait :: Parser (Stmt Span)
parseTrait = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "trait")
  name <- declarationIdentifier
  members <- braces (M.many (parseClassMemberInContext TraitContext))
  pure (\sp -> StmtTrait sp (TraitDecl sp attrs name members))

-- | Enum declaration (pure or backed).
parseEnum :: Parser (Stmt Span)
parseEnum = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "enum")
  name@(Ident _ enumName) <- declarationIdentifier
  mBacked <- optional (colon *> parseEnumBackingType)
  impls <- (keyword "implements" *> (qualifiedName `M.sepBy1` comma)) <|> pure []
  members <- braces (M.many (parseClassMemberInContext (EnumContext enumName (isJust mBacked))))
  pure (\sp -> StmtEnum sp (EnumDecl sp attrs name mBacked impls members))

-- | Backed enums must be backed by @int@ or @string@; PHP identifiers match case-insensitively.
parseEnumBackingType :: Parser (Type Span)
parseEnumBackingType = M.label "enum backing type" $ lexeme $ withSpan $ M.try $ do
  tok <- rawIdentifier
  if T.toLower tok `elem` ["int", "string"]
    then pure (\sp -> SimpleType sp (QualifiedName sp NameUnqualified [tok]))
    else M.empty
