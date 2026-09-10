{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Parser.Lexer
  ( Parser
  , LexerState (..)
  , initialLexerState
  , runPHPParser
  , spanned
  , withSpan
  , toSourcePos
  -- * Whitespace, Comments, Trivia
  , sc
  , scNoNewline
  , takeTrivia
  , lexeme
  , symbol
  , parens
  , braces
  , brackets
  , semi
  , comma
  , colon
  , doubleColon
  -- * Identifiers & Variables
  , identifier
  , semiReservedIdentifier
  , rawIdentifier
  , variableName
  , qualifiedName
  -- * Keywords
  , keyword
  , keyword_
  , isKeyword
  -- * Literals
  , literalInt
  , literalFloat
  , literalString
  , literalHeredocOrNowdoc
  ) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (void, when)
import Control.Monad.State.Strict (State, runState, get, modify', put)
import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Language.PHP.AST
import Language.PHP.Span (Span, SourcePos (..), combineSpans, mkSpan)

data LexerState = LexerState
  { currentTrivia :: ![Trivia]
  , triviaBySpan  :: !(Map.Map Span [Trivia])
  } deriving (Eq, Show)

initialLexerState :: LexerState
initialLexerState = LexerState [] Map.empty

type Parser = M.ParsecT Void Text (State LexerState)

-- | Run a parser with initial state.
runPHPParser :: Parser a -> FilePath -> Text -> Either (M.ParseErrorBundle Text Void) (a, Map.Map Span [Trivia])
runPHPParser p file input =
  let (res, st) = runState (M.runParserT (spanned p <* M.eof) file input) initialLexerState
  in case res of
    Left err -> Left err
    Right (_, val) -> Right (val, triviaBySpan st)

toSourcePos :: M.SourcePos -> Int -> Language.PHP.Span.SourcePos
toSourcePos sp offset = Language.PHP.Span.SourcePos
  { posFile   = M.sourceName sp
  , posLine   = M.unPos (M.sourceLine sp)
  , posColumn = M.unPos (M.sourceColumn sp)
  , posOffset = offset
  }

sourcePosHere :: Parser Language.PHP.Span.SourcePos
sourcePosHere = toSourcePos <$> M.getSourcePos <*> M.getOffset

-- | Run a parser and record its source span.
spanned :: Parser a -> Parser (Span, a)
spanned p = do
  start <- sourcePosHere
  res <- p
  end <- sourcePosHere
  pure (mkSpan start end, res)

-- | Wrap a parser that expects a Span.
withSpan :: Parser (Span -> a) -> Parser a
withSpan p = do
  start <- sourcePosHere
  original <- get
  leading <- takeTrivia
  result <- M.observing $ do
    f <- p
    end <- sourcePosHere
    pure (f, end)
  case result of
    Left err -> do
      put original
      M.parseError err
    Right (f, end) -> do
      let span' = mkSpan start end
      modify' (\st -> st { triviaBySpan = Map.insert span' leading (triviaBySpan st) })
      pure (f span')

-- | Take all accumulated trivia and reset the trivia buffer.
takeTrivia :: Parser [Trivia]
takeTrivia = do
  triv <- currentTrivia <$> get
  modify' (\s -> s { currentTrivia = [] })
  pure triv

-- | Record trivia into state.
addTrivia :: Trivia -> Parser ()
addTrivia t = modify' (\s -> s { currentTrivia = currentTrivia s ++ [t] })

-- | Space and comment consumer.
sc :: Parser ()
sc = L.space
  (void C.spaceChar)
  (lineComment <|> hashComment)
  blockOrDocComment

-- | Space consumer without consuming newlines.
scNoNewline :: Parser ()
scNoNewline = L.space
  (void (M.satisfy (\c -> c == ' ' || c == '\t' || c == '\r')))
  (lineComment <|> hashComment)
  blockOrDocComment

lineComment :: Parser ()
lineComment = do
  _ <- C.string "//"
  txt <- M.takeWhileP (Just "comment text") (/= '\n')
  addTrivia (CommentLine txt)

hashComment :: Parser ()
hashComment = M.try $ do
  _ <- C.char '#'
  _ <- M.notFollowedBy (C.char '[')
  txt <- M.takeWhileP (Just "comment text") (/= '\n')
  addTrivia (CommentLine txt)

blockOrDocComment :: Parser ()
blockOrDocComment = do
  _ <- C.string "/*"
  isDoc <- (True <$ M.try (C.char '*' <* M.notFollowedBy (C.char '/'))) <|> pure False
  txt <- takeUntilClose
  if isDoc
    then addTrivia (DocBlock (T.pack txt))
    else addTrivia (CommentBlock (T.pack txt))
  where
    takeUntilClose = do
      isEnd <- (True <$ C.string "*/") <|> pure False
      if isEnd
        then pure []
        else do
          c <- M.anySingle
          (c :) <$> takeUntilClose

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = M.between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = M.between (symbol "[") (symbol "]")

semi :: Parser Text
semi = symbol ";"

comma :: Parser Text
comma = symbol ","

colon :: Parser Text
colon = symbol ":"

doubleColon :: Parser Text
doubleColon = symbol "::"

-- | Identifiers in PHP: [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*
rawIdentifier :: Parser Text
rawIdentifier = M.label "identifier" $ do
  first <- M.satisfy (\c -> isAlpha c || c == '_' || c >= '\x80')
  rest <- M.takeWhileP Nothing (\c -> isAlphaNum c || c == '_' || c >= '\x80')
  pure (T.cons first rest)

-- | Case-insensitive keyword matcher.
keyword :: Text -> Parser Text
keyword kw = M.label (T.unpack kw) $ lexeme $ M.try $ do
  tok <- rawIdentifier
  if T.toLower tok == T.toLower kw
    then pure tok
    else M.empty

keyword_ :: Text -> Parser ()
keyword_ = void . keyword

-- | Check if text is a PHP keyword.
isKeyword :: Text -> Bool
isKeyword w = T.toLower w `elem` phpKeywords

phpKeywords :: [Text]
phpKeywords =
  [ "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class"
  , "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else"
  , "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch"
  , "endwhile", "eval", "exit", "extends", "final", "finally", "fn", "for", "foreach"
  , "function", "global", "goto", "if", "implements", "include", "include_once"
  , "instanceof", "insteadof", "interface", "isset", "list", "match", "namespace"
  , "new", "or", "print", "private", "protected", "public", "readonly", "require"
  , "require_once", "return", "static", "switch", "throw", "trait", "try", "unset"
  , "use", "var", "while", "xor", "yield"
  ]

-- | Non-keyword identifier.
identifier :: Parser (Ident Span)
identifier = M.label "identifier" $ lexeme $ withSpan $ M.try $ do
  tok <- rawIdentifier
  if isKeyword tok
    then M.empty
    else pure (\sp -> Ident sp tok)

-- | Identifier in a member-name position (methods, class constants), where PHP
-- allows semi-reserved keywords. Only @class@ stays reserved there.
semiReservedIdentifier :: Parser (Ident Span)
semiReservedIdentifier = M.label "identifier" $ lexeme $ withSpan $ M.try $ do
  tok <- rawIdentifier
  if isKeyword tok && T.toLower tok == "class"
    then M.empty
    else pure (\sp -> Ident sp tok)

-- | Variable name without '$'.
variableName :: Parser (VarName Span)
variableName = M.label "variable" $ lexeme $ withSpan $ do
  _ <- C.char '$'
  name <- rawIdentifier
  pure (\sp -> VarName sp name)

-- | Qualified name (e.g. \Foo\Bar, Foo\Bar, namespace\Bar).
qualifiedName :: Parser (QualifiedName Span)
qualifiedName = M.label "qualified name" $ lexeme $ withSpan $ M.try $ do
  isFullyQualified <- (True <$ C.char '\\') <|> pure False
  isNamespaceRel <- (True <$ M.try (C.string "namespace" *> C.char '\\')) <|> pure False
  firstPart <- rawIdentifier
  restParts <- many (M.try (C.char '\\' *> rawIdentifier))
  if not isFullyQualified && not isNamespaceRel && null restParts && isKeyword firstPart
    then M.empty
    else do
      let allParts = firstPart : restParts
      let kind
            | isFullyQualified = NameFullyQualified
            | isNamespaceRel = NameRelative
            | null restParts = NameUnqualified
            | otherwise = NameQualified
      pure (\sp -> QualifiedName sp kind allParts)

-- | Numeric literals: supports decimal, hex 0x, octal 0o/0, binary 0b, and underscores.
literalInt :: Parser (Literal Span)
literalInt = M.label "integer" $ lexeme $ withSpan $ M.try $ do
  (val, raw) <- parseNumber
  pure (\sp -> LitInt sp val raw)
  where
    parseNumber = parseHex <|> parseBin <|> parseExplicitOctal <|> parseDecOrLegacyOctal

    parseHex = do
      pfx <- M.try (C.char '0' *> (C.char 'x' <|> C.char 'X'))
      digits <- underscoreDigits1 isHexDigit
      let raw = T.pack ['0', pfx] <> digits
      let val = readHexStr (T.filter (/= '_') digits)
      pure (val, raw)

    parseBin = do
      pfx <- M.try (C.char '0' *> (C.char 'b' <|> C.char 'B'))
      digits <- underscoreDigits1 (\c -> c == '0' || c == '1')
      let raw = T.pack ['0', pfx] <> digits
      let val = readBinStr (T.filter (/= '_') digits)
      pure (val, raw)

    parseExplicitOctal = do
      pfx <- M.try (C.char '0' *> (C.char 'o' <|> C.char 'O'))
      digits <- underscoreDigits1 (\c -> c >= '0' && c <= '7')
      let raw = T.pack ['0', pfx] <> digits
      let val = readOctStr (T.filter (/= '_') digits)
      pure (val, raw)

    parseDecOrLegacyOctal = do
      d0 <- C.digitChar
      rest <- underscoreDigits isDigit
      let raw = T.cons d0 rest
      let clean = T.filter (/= '_') raw
      -- Check if float follows (decimal point or exponent)
      isFloat <- (True <$ M.lookAhead (C.char '.' <|> C.char 'e' <|> C.char 'E')) <|> pure False
      if isFloat
        then M.empty
        else if d0 == '0' && not (T.null rest) && T.all (\c -> c >= '0' && c <= '7') clean
          then pure (readOctStr clean, raw)
          else pure (read (T.unpack clean), raw)

    readHexStr s = case reads ("0x" ++ T.unpack s) of
      [(v, "")] -> v
      _ -> 0

    readOctStr s = case reads ("0o" ++ T.unpack s) of
      [(v, "")] -> v
      _ -> 0

    readBinStr s = T.foldl' (\acc c -> acc * 2 + if c == '1' then 1 else 0) 0 s

underscoreDigits :: (Char -> Bool) -> Parser Text
underscoreDigits pred' = do
  let charP = M.satisfy pred' <|> (C.char '_' <* M.lookAhead (M.satisfy pred'))
  T.pack <$> many charP

underscoreDigits1 :: (Char -> Bool) -> Parser Text
underscoreDigits1 pred' = do
  c <- M.satisfy pred'
  rest <- underscoreDigits pred'
  pure (T.cons c rest)

-- | Float literals: decimal point, exponent, underscores.
literalFloat :: Parser (Literal Span)
literalFloat = M.label "float" $ lexeme $ withSpan $ M.try $ do
  raw <- parseRawFloat
  let clean = T.filter (/= '_') raw
  -- Haskell's `reads` needs digits on at least one side of a decimal point,
  -- so pad a mantissa whose last digit before the exponent (or EOF) is a dot.
  let (mantissa, expPart) = T.break (\c -> c == 'e' || c == 'E') clean
      padSuffix = if "." `T.isSuffixOf` mantissa then mantissa <> "0" else mantissa
      readStr =
        (if "." `T.isPrefixOf` padSuffix then ("0" <>) else id) padSuffix <> expPart
  let val = case reads (T.unpack readStr) of
        [(v, "")] -> v
        _ -> 0.0
  pure (\sp -> LitFloat sp val raw)
  where
    parseRawFloat = do
      d1 <- underscoreDigits isDigit
      hasDot <- (True <$ C.char '.') <|> pure False
      if hasDot
        then do
          d2 <- underscoreDigits isDigit
          when (T.null d1 && T.null d2) M.empty
          mExp <- optional parseExp
          pure (d1 <> "." <> d2 <> maybe "" id mExp)
        else do
          when (T.null d1) M.empty
          expPart <- parseExp
          pure (d1 <> expPart)

    parseExp = do
      e <- C.char 'e' <|> C.char 'E'
      sgn <- optional (C.char '+' <|> C.char '-')
      digits <- underscoreDigits1 isDigit
      pure (T.cons e (maybe T.empty T.singleton sgn) <> digits)

-- | String literals: single-quoted (raw) or double-quoted (with variable
-- interpolation). The parser for complex-syntax @{$expr}@ bodies is passed in
-- to avoid a module cycle with the expression parser.
literalString :: Parser (Expr Span) -> Parser (Literal Span)
literalString parseInterpExpr = M.label "string" $ lexeme $ withSpan $ singleQuoted <|> doubleQuoted
  where
    singleQuoted = do
      (raw, val) <- M.match $ do
        _ <- C.char '\''
        content <- many singleChar
        _ <- C.char '\''
        pure content
      pure (\sp -> LitString sp (T.pack val) raw)

    singleChar =
      M.try (C.string "\\'" *> pure '\'')
      <|> M.try (C.string "\\\\" *> pure '\\')
      <|> M.satisfy (/= '\'')

    doubleQuoted = do
      (raw, parts) <- M.match $ do
        _ <- C.char '"'
        ps <- many doublePart
        _ <- C.char '"'
        pure ps
      pure $ \sp -> case [e | StrExpr e <- parts] of
        [] -> LitString sp (T.concat [t | StrLit t <- parts]) raw
        _  -> LitInterpolated sp (mergeLiterals parts)

    -- One chunk of content: an interpolated expression, or literal text. A
    -- failed interpolation attempt backtracks into literal text, so unmatched
    -- @{@ and stray @$@ stay ordinary characters.
    doublePart =
      (StrExpr <$> (interpolatedExpr <|> simpleInterp))
      <|> literalRun
      <|> strayDollar
      <|> strayBrace

    -- Complex syntax @{$expr}@: an arbitrary expression between braces, which
    -- must start with @$@, as in PHP. Once @{@$ matches, the expression is
    -- committed, so an unterminated one is a parse error, as in PHP.
    interpolatedExpr = do
      _ <- M.try (C.char '{' *> M.lookAhead (C.char '$'))
      e <- parseInterpExpr
      _ <- C.char '}'
      pure e

    -- Simple syntax: @$name@ followed by at most one @->prop@ or @[key]@
    -- dereference, matching PHP's greedy scan of the variable expression. A
    -- @->@ without a property name stays literal; once @[@ opens a subscript,
    -- the key and @]@ are required, as in PHP.
    simpleInterp = do
      (sp, name) <- M.try (spanned (C.char '$' *> rawIdentifier))
      let var = ExprVar sp (SimpleVar sp (VarName sp name))
      simpleStep sp var <|> pure var

    simpleStep sp base = propStep <|> subscriptStep
      where
        propStep = M.try $ do
          _ <- C.string "->"
          (idSp, prop) <- spanned rawIdentifier
          pure (ExprPropertyFetch (combineSpans sp idSp) base (MemberIdent (Ident idSp prop)))
        subscriptStep = do
          _ <- C.char '['
          (_, key) <- spanned subscriptKey
          (endSp, _) <- spanned (C.char ']')
          pure (ExprArrayAccess (combineSpans sp endSp) base (Just key))

    -- Subscript keys in simple syntax: a variable, or a run of identifier
    -- characters — plain decimal digits give an integer key, anything else
    -- (including @0x1F@) is taken as a string, as in PHP.
    subscriptKey =
      varKey <|> wordKey
      where
        varKey = do
          _ <- C.char '$'
          (sp, name) <- spanned rawIdentifier
          pure (ExprVar sp (SimpleVar sp (VarName sp name)))
        wordKey = do
          (sp, tok) <- spanned word
          pure $
            if T.all isDigit tok
              then ExprLit sp (LitInt sp (read (T.unpack tok)) tok)
              else ExprLit sp (LitString sp tok tok)
        word = do
          c <- M.satisfy (\x -> isAlphaNum x || x == '_' || x >= '\x80')
          rest <- M.takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x >= '\x80')
          pure (T.cons c rest)

    -- A run of ordinary characters. Dollars, braces, backslashes and quotes end
    -- the run; a dollar or brace at which interpolation just failed becomes a
    -- one-character literal part below, so the surrounding 'many' retries
    -- interpolation at the next character.
    literalRun = StrLit . T.pack <$> some litCh

    litCh =
      escapedChar
      <|> M.satisfy (\c -> c /= '"' && c /= '\\' && c /= '$' && c /= '{')

    -- Stray characters that look like interpolation starts but did not parse
    -- as one: a trailing @$@, @$@ before a non-identifier char, or an
    -- unmatched @{@. Consumed one at a time so later interpolations still get
    -- their chance.
    strayDollar = StrLit . T.singleton <$> C.char '$'

    strayBrace = StrLit . T.singleton <$> C.char '{'

    escapedChar = C.char '\\' *> (unescape <$> M.anySingle)

    -- Merge neighbouring literal chunks left over from backtracking into
    -- single parts, keeping the AST canonical.
    mergeLiterals = \case
      [] -> []
      StrLit a : StrLit b : rest -> mergeLiterals (StrLit (a <> b) : rest)
      p : rest -> p : mergeLiterals rest

    unescape = \case
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      'v' -> '\v'
      'e' -> '\ESC'
      'f' -> '\f'
      '\\' -> '\\'
      '$' -> '$'
      '"' -> '"'
      other -> other

-- | Heredoc and Nowdoc (including flexible indented syntax).
--
-- Only the header (up to and including the opening newline) is guarded by
-- @M.try@; the body is not. A body failure (an unterminated heredoc, Issue
-- #84) must propagate without rolling back, so megaparsec reports it as the
-- furthest error instead of the offset-0 failure of the surrounding
-- alternatives.
literalHeredocOrNowdoc :: Parser (Literal Span)
literalHeredocOrNowdoc = M.label "heredoc or nowdoc" $ lexeme $ withSpan $ do
  (isNowdoc, tag) <- M.try $ do
    _ <- C.string "<<<"
    _ <- many (C.char ' ' <|> C.char '\t')
    t <- parseTag
    _ <- C.char '\n' <|> (C.char '\r' *> optional (C.char '\n') *> pure '\n')
    pure t

  (content, _) <- parseLines tag
  pure (\sp -> LitHeredoc sp tag content isNowdoc)
  where
    parseTag =
      (do
        _ <- C.char '\''
        t <- rawIdentifier
        _ <- C.char '\''
        pure (True, t))
      <|> (do
        _ <- C.char '"'
        t <- rawIdentifier
        _ <- C.char '"'
        pure (False, t))
      <|> (do
        t <- rawIdentifier
        pure (False, t))

    parseLines tag = do
      lineIndent <- many (C.char ' ' <|> C.char '\t')
      let isIdentChar c = isAlphaNum c || c == '_' || c >= '\x80'
      isEnd <- (True <$ M.lookAhead (M.try (C.string tag *> M.notFollowedBy (M.satisfy isIdentChar)))) <|> pure False
      if isEnd
        then do
          _ <- C.string tag
          pure ("", T.pack lineIndent)
        else do
          -- At end of input without a closing label, nothing can be consumed
          -- and the recursion below would never advance: fail with a parse
          -- error instead of looping (Issue #84).
          atEof <- M.atEnd
          when atEof
            (M.label ("heredoc end (" <> T.unpack tag <> ")") M.empty)
          restOfLine <- M.takeWhileP Nothing (/= '\n')
          _ <- optional (C.char '\n')
          (following, closingIndent) <- parseLines tag
          let fullLine = T.pack lineIndent <> restOfLine
          let strippedLine = stripIndent closingIndent fullLine
          let sep = if T.null following then "" else "\n"
          pure (strippedLine <> sep <> following, closingIndent)

    stripIndent ind line
      | T.isPrefixOf ind line = T.drop (T.length ind) line
      | otherwise = line
