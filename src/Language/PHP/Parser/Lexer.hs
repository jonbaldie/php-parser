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
  , recordTrivia
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
  , declarationIdentifier
  , semiReservedIdentifier
  , rawIdentifier
  , variableName
  , qualifiedName
  , isReservedClassName
  , reservedClassNames
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
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
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
      recordTrivia span' leading
      pure (f span')

-- | Take all accumulated trivia and reset the trivia buffer.
takeTrivia :: Parser [Trivia]
takeTrivia = do
  triv <- currentTrivia <$> get
  modify' (\s -> s { currentTrivia = [] })
  pure triv

-- | Attach trivia to the node with the given span.
recordTrivia :: Span -> [Trivia] -> Parser ()
recordTrivia sp triv = modify' (\st -> st { triviaBySpan = Map.insert sp triv (triviaBySpan st) })

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
  txt <- lineCommentText
  addTrivia (CommentLine txt)

hashComment :: Parser ()
hashComment = M.try $ do
  _ <- C.char '#'
  _ <- M.notFollowedBy (C.char '[')
  txt <- lineCommentText
  addTrivia (CommentLine txt)

-- | The body of a @//@ or @#@ comment. PHP ends a line comment at a newline
-- /or/ at a close tag, whichever comes first: in @// c ?>tail@ the comment is
-- @ c @ and the @?>@ still closes the PHP block. The close tag is left
-- unconsumed for the statement parser. Block comments keep the older
-- behaviour: they only end at @*/@.
lineCommentText :: Parser Text
lineCommentText = go
  where
    go = do
      chunk <- M.takeWhileP (Just "comment text") (\c -> c /= '\n' && c /= '?')
      atCloseTag <- (True <$ M.lookAhead (C.string "?>")) <|> pure False
      if atCloseTag
        then pure chunk
        else do
          mQuestion <- optional (C.char '?')
          case mQuestion of
            Nothing -> pure chunk
            Just _ -> ((chunk <> "?") <>) <$> go

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

-- | Check if text is a reserved class-like declaration name.
-- PHP disallows built-in types, literal names, and contextual names as class,
-- interface, trait, or enum names.
isReservedClassName :: Text -> Bool
isReservedClassName w = T.toLower w `elem` reservedClassNames

-- | List of names reserved by PHP as class, interface, trait, or enum declaration identifiers.
reservedClassNames :: [Text]
reservedClassNames =
  [ -- Built-in type names
    "int"
  , "float"
  , "bool"
  , "string"
  , "void"
  , "iterable"
  , "object"
  , "mixed"
  , "never"
  , "array"
  , "callable"
    -- Literal names
  , "true"
  , "false"
  , "null"
    -- Contextual names
  , "self"
  , "parent"
  , "static"
  ]

-- | Non-keyword identifier.
identifier :: Parser (Ident Span)
identifier = M.label "identifier" $ lexeme $ withSpan $ M.try $ do
  tok <- rawIdentifier
  if isKeyword tok
    then M.empty
    else pure (\sp -> Ident sp tok)

-- | Identifier for class, interface, trait, and enum declarations.
-- PHP disallows keywords as well as reserved type, literal, and contextual names.
declarationIdentifier :: Parser (Ident Span)
declarationIdentifier = M.label "identifier" $ lexeme $ withSpan $ M.try $ do
  tok <- rawIdentifier
  if isKeyword tok || isReservedClassName tok
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
      raw <- underscoreDigits1 isDigit
      let d0 = T.head raw
      let rest = T.tail raw
      let clean = T.filter (/= '_') raw
      -- Check if float follows (decimal point or exponent)
      isFloat <- (True <$ M.lookAhead (C.char '.' <|> C.char 'e' <|> C.char 'E')) <|> pure False
      -- A leading zero makes an integer octal, so 8 and 9 are invalid there
      -- (PHP: "Invalid numeric literal"); as a float's integer part they are fine.
      if isFloat
        then M.empty
        else if d0 == '0' && not (T.null rest)
          then if T.all (\c -> c >= '0' && c <= '7') clean
            then pure (readOctStr clean, raw)
            else fail ("Invalid numeric literal " ++ T.unpack raw)
          else pure (read (T.unpack clean), raw)

    readHexStr s = case reads ("0x" ++ T.unpack s) of
      [(v, "")] -> v
      _ -> 0

    readOctStr s = case reads ("0o" ++ T.unpack s) of
      [(v, "")] -> v
      _ -> 0

    readBinStr s = T.foldl' (\acc c -> acc * 2 + if c == '1' then 1 else 0) 0 s

underscoreDigits :: (Char -> Bool) -> Parser Text
underscoreDigits pred' = maybe "" id <$> optional (underscoreDigits1 pred')

underscoreDigits1 :: (Char -> Bool) -> Parser Text
underscoreDigits1 pred' = do
  c <- M.satisfy pred'
  let charP = M.satisfy pred' <|> (C.char '_' <* M.lookAhead (M.satisfy pred'))
  rest <- many charP
  pure (T.pack (c : rest))

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

decodeDoubleQuotedEscapes :: Text -> Text
decodeDoubleQuotedEscapes = decodeEscapes True

-- | Heredoc bodies decode the escapes of a double-quoted string, except @\\"@:
-- a heredoc has no quote to escape, so PHP keeps the backslash.
decodeHeredocEscapes :: Text -> Text
decodeHeredocEscapes = decodeEscapes False

decodeEscapes :: Bool -> Text -> Text
decodeEscapes quoteEscapes = T.concat . go
  where
    go input = case T.uncons input of
      Nothing -> []
      Just ('\\', rest) ->
        let (body, remaining) = takeEscapeBody rest
        in decodeEscapeBody body : go remaining
      Just (c, rest) -> T.singleton c : go rest

    takeEscapeBody input = case T.uncons input of
      Nothing -> (T.empty, T.empty)
      Just (c, rest)
        | isOctalDigit c ->
            let digits = T.take 2 (T.takeWhile isOctalDigit rest)
            in (T.cons c digits, T.drop (T.length digits) rest)
        | c == 'x' ->
            let digits = T.take 2 (T.takeWhile isHexDigit rest)
            in (T.cons c digits, T.drop (T.length digits) rest)
        | otherwise -> (T.singleton c, rest)

    decodeEscapeBody body = case body of
      "n" -> "\n"
      "r" -> "\r"
      "t" -> "\t"
      "v" -> "\v"
      "e" -> "\ESC"
      "f" -> "\f"
      "\\" -> "\\"
      "$" -> "$"
      "\"" | quoteEscapes -> "\""
      _
        | not (T.null body) && T.all isOctalDigit body -> numericEscape 8 body
        | T.length body >= 2 && T.head body == 'x' && T.all isHexDigit (T.tail body) ->
            numericEscape 16 (T.tail body)
        | otherwise -> "\\" <> body

    numericEscape base digits =
      let value = T.foldl' (\acc digit -> acc * base + digitToInt digit) 0 digits
      in T.singleton (toEnum (value `mod` 256))

    isOctalDigit c = c >= '0' && c <= '7'

-- | String literals: single-quoted (raw) or double-quoted (with variable
-- interpolation). The parser for complex-syntax @{$expr}@ bodies is passed in
-- to avoid a module cycle with the expression parser.
literalString :: Parser (Expr Span) -> Parser (Literal Span)
literalString parseInterpExpr = M.label "string" $ lexeme $ withSpan $ singleQuoted <|> doubleQuoted
  where
    -- The binary prefix @b@ / @B@ (Issue #143) is a syntax alias for an
    -- ordinary string literal, so it contributes nothing but its own
    -- characters to the raw text. It must abut the quote: @M.try@ keeps a bare
    -- @b@ identifier available to the alternatives that follow.
    openQuote q = M.try (optional (C.char 'b' <|> C.char 'B') *> C.char q)

    singleQuoted = do
      (raw, val) <- M.match $ do
        _ <- openQuote '\''
        content <- many singleChar
        _ <- C.char '\''
        pure content
      pure (\sp -> LitString sp (T.pack val) raw)

    singleChar =
      M.try (C.string "\\'" *> pure '\'')
      <|> M.try (C.string "\\\\" *> pure '\\')
      <|> M.satisfy (/= '\'')

    doubleQuoted = do
      (raw, parts) <- M.match $
        openQuote '"'
          *> interpolatedParts parseInterpExpr (== '"') decodeDoubleQuotedEscapes
          <* C.char '"'
      pure $ \sp -> case [e | StrExpr e <- parts] of
        [] -> LitString sp (T.concat [t | StrLit t <- parts]) raw
        _  -> LitInterpolated sp parts

-- | The content of a double-quoted string or of one heredoc line: literal text
-- and interpolated expressions, up to the first character satisfying @stop@
-- outside an escape. Literal text is decoded with @decode@.
interpolatedParts
  :: Parser (Expr Span) -> (Char -> Bool) -> (Text -> Text) -> Parser [StringPart Span]
interpolatedParts parseInterpExpr stop decode = mergeLiterals <$> many part
  where
    -- One chunk of content: an interpolated expression, or literal text. A
    -- failed interpolation attempt backtracks into literal text, so unmatched
    -- @{@ and stray @$@ stay ordinary characters.
    part =
      (StrExpr <$> (interpolatedExpr <|> dollarBraceInterp <|> simpleInterp))
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

    -- Dollar-brace syntax @${ident}@: interpolates @$ident@, as in PHP
    -- (deprecated in 8.2, still parsed through 8.5). Once @${@ is followed by
    -- an identifier, the closing @}@ is required. Variable-variable @${$var}@
    -- does not match here and falls through.
    dollarBraceInterp = do
      start <- sourcePosHere
      _ <- M.try (C.string "${" *> M.lookAhead identStartChar)
      name <- rawIdentifier
      _ <- C.char '}'
      end <- sourcePosHere
      let sp = mkSpan start end
      pure (ExprVar sp (SimpleVar sp (VarName sp name)))

    identStartChar = M.satisfy (\c -> isAlpha c || c == '_' || c >= '\x80')

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
    -- (including @0x1F@) is taken as a string, as in PHP. A leading @-@ is
    -- allowed before a number only: @-1@ is the integer @-1@, shaped like the
    -- braced form's unary minus, while a non-canonical number such as @-0@ or
    -- @-0x1F@ stays the string key PHP makes of it (Issue #235).
    subscriptKey =
      varKey <|> negativeKey <|> wordKey
      where
        varKey = do
          _ <- C.char '$'
          (sp, name) <- spanned rawIdentifier
          pure (ExprVar sp (SimpleVar sp (VarName sp name)))
        negativeKey = do
          (minusSp, _) <- spanned (C.char '-')
          (sp, tok) <- spanned (M.lookAhead (M.satisfy isDigit) *> word)
          let whole = combineSpans minusSp sp
          pure $
            if T.all isDigit tok && T.head tok /= '0'
              then ExprUnary whole OpUnaryMinus (ExprLit sp (LitInt sp (read (T.unpack tok)) tok))
              else ExprLit whole (LitString whole ("-" <> tok) ("-" <> tok))
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

    -- A run of ordinary characters. Dollars, braces, backslashes and stop
    -- characters end the run; a dollar or brace at which interpolation just
    -- failed becomes a one-character literal part below, so the surrounding
    -- 'many' retries interpolation at the next character.
    literalRun = StrLit . T.concat <$> some litCh

    litCh =
      escapedText
      <|> T.singleton <$> M.satisfy (\c -> not (stop c) && c /= '\\' && c /= '$' && c /= '{')

    -- Stray characters that look like interpolation starts but did not parse
    -- as one: a trailing @$@, @$@ before a non-identifier char, or an
    -- unmatched @{@. Consumed one at a time so later interpolations still get
    -- their chance.
    strayDollar = StrLit . T.singleton <$> C.char '$'

    strayBrace = StrLit . T.singleton <$> C.char '{'

    -- An escape never swallows a line break, which ends a heredoc line; a
    -- backslash before one is literal, as it is in PHP.
    escapedText = do
      _ <- C.char '\\'
      body <- M.try octalBody <|> M.try hexBody <|> (T.singleton <$> M.satisfy (/= '\n')) <|> pure T.empty
      pure (decode (T.cons '\\' body))

    octalBody = do
      first <- M.satisfy (\c -> c >= '0' && c <= '7')
      second <- optional (M.satisfy (\c -> c >= '0' && c <= '7'))
      third <- optional (M.satisfy (\c -> c >= '0' && c <= '7'))
      pure (T.pack (first : [c | Just c <- [second, third]]))

    hexBody = do
      _ <- C.char 'x'
      first <- M.satisfy isHexDigit
      second <- optional (M.satisfy isHexDigit)
      pure (T.cons 'x' (T.pack (first : [c | Just c <- [second]])))

-- | Merge neighbouring literal chunks left over from backtracking or line
-- joining into single parts, keeping the AST canonical.
mergeLiterals :: [StringPart a] -> [StringPart a]
mergeLiterals = \case
  [] -> []
  StrLit a : StrLit b : rest -> mergeLiterals (StrLit (a <> b) : rest)
  p : rest -> p : mergeLiterals rest

-- | Heredoc and Nowdoc (including flexible indented syntax).
--
-- A heredoc body interpolates like a double-quoted string, so one that embeds
-- expressions parses to 'LitHeredocInterpolated' parts (Issue #234); one that
-- does not stays a 'LitHeredoc'. A nowdoc body is always literal.
--
-- Only the header (up to and including the opening newline) is guarded by
-- @M.try@; the body is not. A body failure (an unterminated heredoc, Issue
-- #84) must propagate without rolling back, so megaparsec reports it as the
-- furthest error instead of the offset-0 failure of the surrounding
-- alternatives.
literalHeredocOrNowdoc :: Parser (Expr Span) -> Parser (Literal Span)
literalHeredocOrNowdoc parseInterpExpr = M.label "heredoc or nowdoc" $ lexeme $ withSpan $ do
  (isNowdoc, tag) <- M.try $ do
    -- An optional binary prefix @b@ / @B@ (Issue #186) immediately precedes
    -- the @<<<@ delimiter, mirroring PHP's lexer. Like string literals
    -- (Issue #143), it must abut @<<<@; @M.try@ keeps an identifier starting
    -- with @b@ / @B@ available to surrounding expression alternatives.
    _ <- optional (C.char 'b' <|> C.char 'B') *> C.string "<<<"
    _ <- many (C.char ' ' <|> C.char '\t')
    t <- parseTag
    _ <- C.char '\n' <|> (C.char '\r' *> optional (C.char '\n') *> pure '\n')
    pure t

  -- Every body line loses the closing label's indentation, so find it first.
  indent <- M.lookAhead (closingIndent tag)
  -- PHP indents with one whitespace character repeated, and holds every body
  -- line to whichever one the closer chose (Issue #262). A closer that mixes
  -- the two answers to nothing, so it fails before any body line is read --
  -- at the first body line, which is where PHP reports it.
  when (T.any (== ' ') indent && T.any (== '\t') indent) mixedIndentation
  if isNowdoc
    then do
      content <- T.intercalate "\n" <$> bodyLines tag indent (\lead -> (lead <>) <$> M.takeWhileP Nothing (/= '\n'))
      pure (\sp -> LitHeredoc sp tag content True content)
    else do
      bodyParts <- bodyLines tag indent $ \lead -> do
        (raw, parts) <- M.match (interpolatedParts parseInterpExpr (== '\n') decodeHeredocEscapes)
        pure (lead <> raw, [StrLit lead | not (T.null lead)] ++ parts)
      let raw = T.intercalate "\n" (map fst bodyParts)
          parts = mergeLiterals (intercalate [StrLit "\n"] (map snd bodyParts))
      pure $ \sp -> case [e | StrExpr e <- parts] of
        [] -> LitHeredoc sp tag (T.concat [t | StrLit t <- parts]) False raw
        _  -> LitHeredocInterpolated sp tag parts
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

    lineIndent = T.pack <$> many (C.char ' ' <|> C.char '\t')

    atClosingLabel tag =
      (True <$ M.lookAhead (M.try (C.string tag *> M.notFollowedBy (M.satisfy isIdentChar)))) <|> pure False

    isIdentChar c = isAlphaNum c || c == '_' || c >= '\x80'

    -- At end of input without a closing label, nothing can be consumed and
    -- the recursion would never advance: fail with a parse error instead of
    -- looping (Issue #84).
    failAtEof tag = do
      atEof <- M.atEnd
      when atEof
        (M.label ("heredoc end (" <> T.unpack tag <> ")") M.empty)

    closingIndent tag = do
      ind <- lineIndent
      isEnd <- atClosingLabel tag
      if isEnd
        then pure ind
        else do
          failAtEof tag
          _ <- M.takeWhileP Nothing (/= '\n')
          _ <- optional (C.char '\n')
          closingIndent tag

    -- The body lines up to and including the closing label. Each line's
    -- leading whitespace, less the closing indentation, is handed to @line@.
    bodyLines :: Text -> Text -> (Text -> Parser l) -> Parser [l]
    bodyLines tag ind line = do
      lineStart <- M.getOffset
      lead <- lineIndent
      isEnd <- atClosingLabel tag
      if isEnd
        then [] <$ C.string tag
        else do
          failAtEof tag
          l <- line =<< stripIndent lineStart ind lead
          _ <- optional (C.char '\n')
          (l :) <$> bodyLines tag ind line

    -- PHP's wording for indentation that disagrees with the closer's. The
    -- offset must already be at the line PHP would name.
    mixedIndentation :: Parser a
    mixedIndentation =
      M.fancyFailure . S.singleton . M.ErrorFail $
        "Invalid indentation - tabs and spaces cannot be mixed"

    -- The closing label's indentation is removed from every body line, so no
    -- body line may be indented less than the closer; PHP rejects one that is
    -- (Issue #240). It counts characters rather than columns, and exempts a
    -- line that is whitespace to its end. Wherever the two indentations do
    -- overlap they must agree character for character (Issue #262), which an
    -- exempt line is held to as well: PHP rejects a tab under a space-indented
    -- closer even on a line that is otherwise blank.
    stripIndent :: Int -> Text -> Text -> Parser Text
    stripIndent lineStart ind lead = do
      let shared = min (T.length ind) (T.length lead)
      when (T.take shared lead /= T.take shared ind) $
        M.setOffset lineStart *> mixedIndentation
      blank <- M.option False (True <$ M.lookAhead (C.char '\n'))
      if blank || T.length lead >= T.length ind
        then pure (T.drop (T.length ind) lead)
        else do
          M.setOffset lineStart
          M.fancyFailure . S.singleton . M.ErrorFail $
            "Invalid body indentation level (expecting an indentation level of at least "
              <> show (T.length ind) <> ")"
