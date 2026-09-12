{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Parser.Statement
  ( parseProgram
  , parseProgramBody
  , parseStmt
  , parseClassMember
  , parseExpr
  , parseParam
  , parseAttributes
  , parseAttributeGroup
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad (void, when)
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C

import Language.PHP.AST
import Language.PHP.Span (Span, combineSpans)
import Language.PHP.Parser.Lexer
import Language.PHP.Parser.Type (parseType, parseReturnType)
import Language.PHP.Parser.Expression (parseExprWithContext, parseAttributes, parseAttributeGroup, exprSpan, parseLiteralWith)

-- | Expression parser with full statements and class members in closures and anonymous classes.
parseExpr :: Parser (Expr Span)
parseExpr = parseExprWithContext parseStmt (\ro -> parseClassMemberInContext (ClassLikeContext ro))

-- | Parse a complete PHP program, handling optional opening tags, inline HTML, and statements.
parseProgram :: Parser (Program Span)
parseProgram = withSpan $ do
  stmts <- parseProgramBody
  pure (\sp -> Program sp stmts)

parseProgramBody :: Parser [Stmt Span]
parseProgramBody = do
  mInitHtml <- parseInitialHtml
  case mInitHtml of
    Just htmlStmt -> (htmlStmt :) <$> parsePhpAndHtmlChunks
    Nothing -> parsePhpAndHtmlChunks

-- | Parse HTML outside <?php / <?= tags at the beginning of a file.
parseInitialHtml :: Parser (Maybe (Stmt Span))
parseInitialHtml = do
  hasTag <- (True <$ M.lookAhead (C.string "<?php" <|> C.string "<?=" <|> (C.string "<?" <* M.notFollowedBy (C.char '=')))) <|> pure False
  isEof <- M.lookAhead (True <$ M.eof) <|> pure False
  if hasTag || isEof
    then pure Nothing
    else withSpan $ do
      txt <- takeUntilPhpTag
      pure (\sp -> Just (StmtInlineHtml sp (T.pack txt)))

takeUntilPhpTag :: Parser String
takeUntilPhpTag = do
  isTag <- (True <$ M.lookAhead (C.string "<?php" <|> C.string "<?=" <|> (C.string "<?" <* M.notFollowedBy (C.char '=')))) <|> pure False
  isEof <- (True <$ M.lookAhead M.eof) <|> pure False
  if isTag || isEof
    then pure []
    else do
      c <- M.anySingle
      (c :) <$> takeUntilPhpTag

-- | Parse an opening tag, excluding the whitespace and comments after it,
-- so callers can backtrack over the tag without hiding errors in that trivia.
parseOpenTag :: Parser ()
parseOpenTag =
  (M.try (C.string "<?php") *> (void C.space1 <|> void (C.char '\n') <|> void M.eof))
    <|> (C.string "<?" *> M.notFollowedBy (C.char '=') *> M.notFollowedBy (C.string "php"))

parseCloseTag :: Parser ()
parseCloseTag = do
  _ <- C.string "?>"
  -- PHP suppresses the newline immediately following a close tag,
  -- matching its lexer's NEWLINE rule: "\r\n" as a pair, "\n", or "\r".
  _ <- optional (C.char '\n' <|> (C.char '\r' *> optional (C.char '\n') *> pure '\n'))
  pure ()

-- | A statement may end with a semicolon or an immediately following close tag.
-- Leave the close tag for 'parsePhpAndHtmlChunks' so it can switch to HTML mode.
statementTerminator :: Parser T.Text
statementTerminator = semi <|> (M.lookAhead parseCloseTag *> pure ";")

parsePhpAndHtmlChunks :: Parser [Stmt Span]
parsePhpAndHtmlChunks = do
  isEof <- (True <$ M.lookAhead M.eof) <|> pure False
  if isEof
    then pure []
    else do
      isShortEcho <- (True <$ M.try (C.string "<?=")) <|> pure False
      if isShortEcho
        then do
          sc
          firstExpr <- parseExpr
          moreExprs <- M.many (comma *> parseExpr)
          _ <- optional semi
          hasClose <- (True <$ M.try parseCloseTag) <|> pure False
          let lastExpr = if null moreExprs then firstExpr else last moreExprs
              echoSpan = combineSpans (exprSpan firstExpr) (exprSpan lastExpr)
              echoStmt = StmtEcho echoSpan (firstExpr : moreExprs)
          if hasClose
            then do
              (spHtml, html) <- spanned takeUntilPhpTag
              rest <- parsePhpAndHtmlChunks
              if null html
                then pure (echoStmt : rest)
                else pure (echoStmt : StmtInlineHtml spHtml (T.pack html) : rest)
            else do
              rest <- parsePhpAndHtmlChunks
              pure (echoStmt : rest)
        else do
          isOpenTag <- (True <$ M.try parseOpenTag) <|> pure False
          if isOpenTag
            then sc *> parsePhpAndHtmlChunks
            else do
              isClose <- (True <$ M.try parseCloseTag) <|> pure False
              if isClose
                then do
                  (sp, html) <- spanned takeUntilPhpTag
                  rest <- parsePhpAndHtmlChunks
                  if null html
                    then pure rest
                    else pure (StmtInlineHtml sp (T.pack html) : rest)
                else do
                  s <- parseStmt
                  rest <- parsePhpAndHtmlChunks
                  pure (s : rest)

-- | Parse a single statement.
parseStmt :: Parser (Stmt Span)
parseStmt =
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
  bodyBranch directives
  where
    parseDeclareDirective = withSpan $ do
      name <- identifier
      _ <- symbol "="
      val <- parseLiteralWith parseExpr
      pure (\sp -> DeclareDirective sp name val)

    bodyBranch dirs =
      -- Semicolon or close tag (e.g. declare(strict_types=1);)
      (statementTerminator *> pure (\sp -> StmtDeclare sp dirs Nothing))
      -- Alternative syntax (declare(...): ... enddeclare;)
      <|> (do
        _ <- colon
        stmts <- parseAltBody
        keyword_ "enddeclare"
        _ <- semi
        pure (\sp -> StmtDeclare sp dirs (Just stmts)))
      -- Brace block (declare(...) { ... })
      <|> (do
        stmts <- braces (M.many parseStmt)
        pure (\sp -> StmtDeclare sp dirs (Just stmts)))
      -- Single statement (declare(...) stmt)
      <|> (do
        s <- parseStmt
        pure (\sp -> StmtDeclare sp dirs (Just [s])))

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
  stmts <- braces (M.many parseStmt)
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
      _ <- semi
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
      keyword_ "elseif" <|> M.try (keyword_ "else" *> keyword_ "if")
      c <- parens parseExpr
      _ <- colon
      body <- parseAltBody
      pure (c, body)

    parseAltElse = do
      keyword_ "else"
      _ <- colon
      parseAltBody

    parseStmtBody =
      (braces (M.many parseStmt))
      <|> ((\s -> [s]) <$> parseStmt)

-- | Body of an alternative-syntax control structure: statements possibly
-- interleaved with inline HTML chunks, ending just before the terminator
-- keyword (endif/endwhile/...). The @many@ stops at any token no statement
-- can start with; terminator keywords are reserved words, so they halt it.
parseAltBody :: Parser [Stmt Span]
parseAltBody = concat <$> M.many parseAltBodyElement

parseAltBodyElement :: Parser [Stmt Span]
parseAltBodyElement =
  parseAltHtmlChunk
  <|> parseAltOpenTagChunk
  <|> parseAltShortEchoChunk
  <|> ((\s -> [s]) <$> parseStmt)
  where
    -- ?> html <?php / <?= — must be reopened by a PHP chunk to continue.
    parseAltHtmlChunk = do
      isClose <- (True <$ M.try parseCloseTag) <|> pure False
      if not isClose
        then M.empty
        else do
          (sp, html) <- spanned takeUntilPhpTag
          pure (if null html then [] else [StmtInlineHtml sp (T.pack html)])
    -- Reopening tag after an inline HTML chunk.
    parseAltOpenTagChunk = do
      isOpen <- (True <$ M.try parseOpenTag) <|> pure False
      if not isOpen
        then M.empty
        else pure []
    parseAltShortEchoChunk = do
      isEcho <- (True <$ M.try (C.string "<?=")) <|> pure False
      if not isEcho
        then M.empty
        else do
          sc
          expr <- parseExpr
          _ <- optional semi
          pure [StmtEcho (exprSpan expr) [expr]]

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
      _ <- semi
      pure (\sp -> StmtWhile sp cond body)
    braceBranch cond = do
      body <- (braces (M.many parseStmt)) <|> ((\s -> [s]) <$> parseStmt)
      pure (\sp -> StmtWhile sp cond body)

-- | Do-While loop.
parseDoWhile :: Parser (Stmt Span)
parseDoWhile = withSpan $ do
  keyword_ "do"
  body <- (braces (M.many parseStmt)) <|> ((\s -> [s]) <$> parseStmt)
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
      _ <- semi
      pure (\sp -> StmtFor sp inits conds incrs body)
    braceBranch inits conds incrs = do
      body <- (braces (M.many parseStmt)) <|> ((\s -> [s]) <$> parseStmt)
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
  _ <- symbol ")"
  altBranch arr mKey val byRef <|> braceBranch arr mKey val byRef
  where
    altBranch arr mKey val byRef = do
      _ <- colon
      body <- parseAltBody
      keyword_ "endforeach"
      _ <- semi
      pure (\sp -> StmtForeach sp arr mKey val byRef body)
    braceBranch arr mKey val byRef = do
      body <- (braces (M.many parseStmt)) <|> ((\s -> [s]) <$> parseStmt)
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
      pure (\sp -> StmtSwitch sp expr cases)
    altBranch expr = do
      _ <- colon
      cases <- concat <$> M.many ((\c -> [c]) <$> parseSwitchCaseWith parseAltBody)
      keyword_ "endswitch"
      _ <- semi
      pure (\sp -> StmtSwitch sp expr cases)

parseSwitchCase :: Parser (SwitchCase Span)
parseSwitchCase = parseSwitchCaseWith (M.many parseStmt)

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
  body <- braces (M.many parseStmt)
  catches <- M.many parseCatch
  mFinally <- optional (keyword "finally" *> braces (M.many parseStmt))
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
      catBody <- braces (M.many parseStmt)
      pure (\sp -> CatchClause sp types mVar catBody)

-- | Namespace declaration (bracketed or unbracketed).
parseNamespace :: Parser (Stmt Span)
parseNamespace = withSpan $ do
  (mName, isBracketed) <- M.try $ do
    keyword_ "namespace"
    mName <- optional parseNamespaceName
    case mName of
      Nothing -> do
        _ <- M.lookAhead (symbol "{")
        pure (Nothing, True)
      Just _ -> do
        isBr <- (True <$ M.lookAhead (symbol "{")) <|> (False <$ semi)
        pure (mName, isBr)
  if isBracketed
    then do
      stmts <- braces (M.many parseStmt)
      pure (\sp -> StmtNamespace sp mName (Just stmts))
    else pure (\sp -> StmtNamespace sp mName Nothing)
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
      clauses <- braces (parseUseClause `M.sepEndBy1` comma)
      _ <- semi
      pure (\sp -> StmtGroupUse sp ut prefix clauses)

    parseUseClause = withSpan $ do
      qn <- qualifiedName
      mAlias <- optional (keyword "as" *> identifier)
      pure (\sp -> UseClause sp qn mAlias)



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

-- | Register a custom parse error for a repeated declaration modifier.
-- Registration (rather than failure) keeps the surrounding modifier loop
-- composable across backtracking alternatives while still rejecting the
-- program, mirroring PHP's own diagnostics.
duplicateModifier :: String -> Parser ()
duplicateModifier what = modifierError ("Multiple " <> what <> " modifiers are not allowed")

-- | Register a custom parse error for a pair of modifiers PHP forbids together,
-- such as @final abstract@ on a class or method and @static readonly@ on a
-- property. Naming the pair (rather than the modifier just read) keeps the
-- diagnostic identical whichever order the two were written in.
conflictingModifiers :: String -> String -> Parser ()
conflictingModifiers this that =
  modifierError ("Cannot combine the " <> this <> " and " <> that <> " modifiers")

modifierError :: String -> Parser ()
modifierError = M.registerFancyFailure . S.singleton . M.ErrorFail

-- | Property modifiers (can be in any order: public, private(set), readonly, static, final, abstract, var).
-- Note: @var@ is an alias for @public@ visibility and cannot be combined with explicit visibility.
parsePropertyModifier :: Parser PropertyModifier
parsePropertyModifier = loop Nothing Nothing False False False False
  where
    loop vis wVis isStat isRo isFin isAbs =
      (do
        wv <- parseAsymmetricWriteVis
        when (isJust wVis) $ duplicateModifier "access type"
        loop vis (Just wv) isStat isRo isFin isAbs)
      <|> (do
        v <- (parseVisibility <|> (Public <$ keyword_ "var"))
        when (isJust vis) $ duplicateModifier "access type"
        loop (Just v) wVis isStat isRo isFin isAbs)
      <|> (do
        keyword_ "static"
        when isStat $ duplicateModifier "static"
        when isRo $ conflictingModifiers "static" "readonly"
        loop vis wVis True isRo isFin isAbs)
      <|> (do
        keyword_ "readonly"
        when isRo $ duplicateModifier "readonly"
        when isStat $ conflictingModifiers "static" "readonly"
        loop vis wVis isStat True isFin isAbs)
      <|> (do
        keyword_ "final"
        when isFin $ duplicateModifier "final"
        loop vis wVis isStat isRo True isAbs)
      <|> (do
        keyword_ "abstract"
        when isAbs $ duplicateModifier "abstract"
        loop vis wVis isStat isRo isFin True)
      <|> pure (PropertyModifier vis wVis isStat isRo isFin isAbs)

-- | Method modifiers.
parseMethodModifier :: Parser MethodModifier
parseMethodModifier = loop Nothing False False False
  where
    loop vis isStat isFin isAbs =
      (do
        v <- parseVisibility
        when (isJust vis) $ duplicateModifier "access type"
        loop (Just v) isStat isFin isAbs)
      <|> (do
        keyword_ "static"
        when isStat $ duplicateModifier "static"
        loop vis True isFin isAbs)
      <|> (do
        keyword_ "final"
        when isFin $ duplicateModifier "final"
        when isAbs $ conflictingModifiers "final" "abstract"
        loop vis isStat True isAbs)
      <|> (do
        keyword_ "abstract"
        when isAbs $ duplicateModifier "abstract"
        when isFin $ conflictingModifiers "final" "abstract"
        loop vis isStat isFin True)
      <|> pure (MethodModifier vis isStat isFin isAbs)

-- | Class modifiers.
parseClassModifier :: Parser ClassModifier
parseClassModifier = loop False False False
  where
    loop isFin isAbs isRo =
      (do
        keyword_ "final"
        when isFin $ duplicateModifier "final"
        when isAbs $ conflictingModifiers "final" "abstract"
        loop True isAbs isRo)
      <|> (do
        keyword_ "abstract"
        when isAbs $ duplicateModifier "abstract"
        when isFin $ conflictingModifiers "final" "abstract"
        loop isFin True isRo)
      <|> (do
        keyword_ "readonly"
        when isRo $ duplicateModifier "readonly"
        loop isFin isAbs True)
      <|> pure (ClassModifier isFin isAbs isRo)

-- | Class constant modifiers (visibility and final in any order).
parseConstModifier :: Parser (Maybe Visibility, Bool)
parseConstModifier = loop Nothing False
  where
    loop vis isFin =
      (do
        v <- parseVisibility
        when (isJust vis) $ duplicateModifier "access type"
        loop (Just v) isFin)
      <|> (do
        keyword_ "final"
        when isFin $ duplicateModifier "final"
        loop vis True)
      <|> pure (vis, isFin)

-- | Top-level or Class constant declaration (supports typed constants PHP 8.3).
parseConstDecl :: Parser (ConstDecl Span)
parseConstDecl = withSpan $ do
  (attrs, vis, isFinal) <- M.try $ do
    attrs <- parseAttributes
    (vis, isFinal) <- parseConstModifier
    keyword_ "const"
    pure (attrs, vis, isFinal)
  mType <- optional (M.try (parseType <* M.lookAhead semiReservedIdentifier))
  items <- parseConstItem `M.sepBy1` comma
  _ <- semi
  pure (\sp -> ConstDecl sp attrs vis isFinal mType items)
  where
    parseConstItem = do
      name <- semiReservedIdentifier
      _ <- symbol "="
      val <- parseExpr
      pure (name, val)

parseConstStmt :: Parser (Stmt Span)
parseConstStmt = withSpan $ do
  c <- parseConstDecl
  pure (\sp -> StmtConst sp c)

-- | Function declaration.
parseFunction :: Parser (Stmt Span)
parseFunction = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "function" <* M.notFollowedBy (symbol "("))
  byRef <- (True <$ symbol "&") <|> pure False
  name <- identifier
  params <- parens (parseParamInContext NonConstructorParam `M.sepEndBy` comma)
  retType <- parseReturnType
  body <- braces (M.many parseStmt)
  pure (\sp -> StmtFunction sp (FunctionDecl sp attrs byRef name params retType body))

-- | Promoted parameter context for validating parameter modifiers.
data ParamContext
  = ConstructorParam
  | AbstractConstructorParam
  | NonConstructorParam
  deriving (Eq, Show)

-- | Whether a parameter declares promoted property modifiers.
isPromotedParam :: Param a -> Bool
isPromotedParam p = isJust (paramVis p) || isJust (paramWriteVis p) || paramReadonly p

-- | Parameter parsing (supports constructor property promotion & asymmetric visibility).
parseParam :: Parser (Param Span)
parseParam = parseParamInContext ConstructorParam

parseParamInContext :: ParamContext -> Parser (Param Span)
parseParamInContext pCtx = withSpan $ do
  attrs <- parseAttributes
  (vis, wVis, isRo) <- parseParamModifiers
  let isPromoted = isJust vis || isJust wVis || isRo
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
  var <- variableName
  mDef <- optional (symbol "=" *> parseExpr)
  pure (\sp -> Param sp attrs vis wVis isRo typ byRef isVariadic var mDef)
  where
    parseParamModifiers = loop Nothing Nothing False
      where
        loop vis wVis isRo =
          (do
            wv <- parseAsymmetricWriteVis
            when (isJust wVis) $ duplicateModifier "access type"
            loop vis (Just wv) isRo)
          <|> (do
            v <- parseVisibility
            when (isJust vis) $ duplicateModifier "access type"
            loop (Just v) wVis isRo)
          <|> (do
            keyword_ "readonly"
            when isRo $ duplicateModifier "readonly"
            loop vis wVis True)
          <|> pure (vis, wVis, isRo)

-- | The enclosing declaration kind in which class members are parsed.
-- PHP applies different member rules to enums, classes, and interfaces.
data ClassContext
  = ClassLikeContext !Bool  -- ^ class, trait, or anonymous class; readonly flag
  | EnumContext
  | InterfaceContext
  deriving (Eq, Show)

-- | Class member declaration.
parseClassMember :: Parser (ClassMember Span)
parseClassMember = parseClassMemberInContext (ClassLikeContext False)

parseClassMemberInContext :: ClassContext -> Parser (ClassMember Span)
parseClassMemberInContext ctx = do
  enclosingReadonly <- pure $ case ctx of
    ClassLikeContext ro -> ro
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
checkMember ctx member =
  case (ctx, member) of
    (EnumContext, MemberEnumCase _) -> pure ()
    (EnumContext, MemberProperty _) -> forbidden "Enums may not include properties"
    (EnumContext, MemberMethod md)
      | any isPromotedParam (methodParams md) -> forbidden "Enums may not include properties"
    (InterfaceContext, MemberProperty pd)
      | null (propHooks pd) -> forbidden "Interfaces may not include properties"
    (InterfaceContext, MemberTraitUse _) -> forbidden "Cannot use traits inside of interfaces"
    (_, MemberEnumCase _) -> forbidden "Enum cases can only be used inside enum declarations"
    _ -> pure ()
  where
    forbidden msg = M.fancyFailure (S.singleton (M.ErrorFail (T.unpack msg)))

parseMethodOrProperty :: ClassContext -> Bool -> Parser (ClassMember Span)
parseMethodOrProperty ctx enclosingReadonly = do
  attrs <- parseAttributes
  isMethod <- (True <$ M.lookAhead (M.try parseMethodLookAhead)) <|> pure False
  if isMethod
    then MemberMethod <$> parseMethod ctx attrs
    else MemberProperty <$> parseProperty enclosingReadonly attrs
  where
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
      isAbs = methodAbstract modif || ctx == InterfaceContext
      paramCtx
        | not isCtor = NonConstructorParam
        | isAbs      = AbstractConstructorParam
        | otherwise  = ConstructorParam
  params <- parens (parseParamInContext paramCtx `M.sepEndBy` comma)
  retType <- parseReturnType
  body <- (semi *> pure Nothing) <|> (Just <$> braces (M.many parseStmt))
  pure (\sp -> MethodDecl sp attrs modif byRef name params retType body)

-- | Property with optional PHP 8.4 hooks and asymmetric visibility.
parseProperty :: Bool -> [AttributeGroup Span] -> Parser (PropertyDecl Span)
parseProperty enclosingReadonly attrs = withSpan $ do
  modif <- parsePropertyModifier
  mType <- optional parseType
  firstVar <- variableName
  mFirstVal <- optional (symbol "=" *> parseExpr)
  hasHooks <- (True <$ M.lookAhead (symbol "{")) <|> pure False
  if hasHooks
    then if enclosingReadonly || propReadonly modif
      then M.empty
      else do
        hooks <- braces (M.many parsePropertyHook)
        pure (\sp -> PropertyDecl sp attrs modif mType [(firstVar, mFirstVal)] hooks)
    else do
      restItems <- M.many (comma *> parseItem)
      _ <- semi
      pure (\sp -> PropertyDecl sp attrs modif mType ((firstVar, mFirstVal) : restItems) [])
  where
    parseItem = do
      var <- variableName
      mVal <- optional (symbol "=" *> parseExpr)
      pure (var, mVal)

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
      <|> (HookBlock <$> braces (M.many parseStmt))
      <|> (HookAbstract <$ semi)

-- | Trait usage inside class.
parseTraitUse :: Parser (TraitUse Span)
parseTraitUse = withSpan $ do
  keyword_ "use"
  names <- qualifiedName `M.sepBy1` comma
  hasAdaptations <- (True <$ M.lookAhead (symbol "{")) <|> pure False
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
  name <- semiReservedIdentifier
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
  name <- declarationIdentifier
  mExtends <- optional (keyword "extends" *> qualifiedName)
  impls <- (keyword "implements" *> (qualifiedName `M.sepBy1` comma)) <|> pure []
  members <- braces (M.many (parseClassMemberInContext (ClassLikeContext (classReadonly modif))))
  pure (\sp -> StmtClass sp (ClassDecl sp attrs modif name mExtends impls members))

-- | Interface declaration.
parseInterface :: Parser (Stmt Span)
parseInterface = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "interface")
  name <- declarationIdentifier
  extends <- (keyword "extends" *> (qualifiedName `M.sepBy1` comma)) <|> pure []
  members <- braces (M.many (parseClassMemberInContext InterfaceContext))
  pure (\sp -> StmtInterface sp (InterfaceDecl sp attrs name extends members))

-- | Trait declaration.
parseTrait :: Parser (Stmt Span)
parseTrait = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "trait")
  name <- declarationIdentifier
  members <- braces (M.many (parseClassMemberInContext (ClassLikeContext False)))
  pure (\sp -> StmtTrait sp (TraitDecl sp attrs name members))

-- | Enum declaration (pure or backed).
parseEnum :: Parser (Stmt Span)
parseEnum = withSpan $ do
  attrs <- M.try (parseAttributes <* keyword_ "enum")
  name <- declarationIdentifier
  mBacked <- optional (colon *> parseEnumBackingType)
  impls <- (keyword "implements" *> (qualifiedName `M.sepBy1` comma)) <|> pure []
  members <- braces (M.many (parseClassMemberInContext EnumContext))
  pure (\sp -> StmtEnum sp (EnumDecl sp attrs name mBacked impls members))

-- | Backed enums must be backed by @int@ or @string@; PHP identifiers match case-insensitively.
parseEnumBackingType :: Parser (Type Span)
parseEnumBackingType = M.label "enum backing type" $ lexeme $ withSpan $ M.try $ do
  tok <- rawIdentifier
  if T.toLower tok `elem` ["int", "string"]
    then pure (\sp -> SimpleType sp (QualifiedName sp NameUnqualified [tok]))
    else M.empty
