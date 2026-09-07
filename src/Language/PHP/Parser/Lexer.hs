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
import Control.Monad.State.Strict (State, runState, get, modify')
import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Language.PHP.AST
import Language.PHP.Span (Span, SourcePos (..), mkSpan)

data LexerState = LexerState
  { currentTrivia :: ![Trivia]
  } deriving (Eq, Show)

initialLexerState :: LexerState
initialLexerState = LexerState []

type Parser = M.ParsecT Void Text (State LexerState)

-- | Run a parser with initial state.
runPHPParser :: Parser a -> FilePath -> Text -> Either (M.ParseErrorBundle Text Void) (a, [Trivia])
runPHPParser p file input =
  let (res, st) = runState (M.runParserT (spanned p <* M.eof) file input) initialLexerState
  in case res of
    Left err -> Left err
    Right (_, val) -> Right (val, currentTrivia st)

toSourcePos :: M.SourcePos -> Language.PHP.Span.SourcePos
toSourcePos sp = Language.PHP.Span.SourcePos
  { posFile   = M.sourceName sp
  , posLine   = M.unPos (M.sourceLine sp)
  , posColumn = M.unPos (M.sourceColumn sp)
  , posOffset = 0
  }

-- | Run a parser and record its source span.
spanned :: Parser a -> Parser (Span, a)
spanned p = do
  start <- M.getSourcePos
  res <- p
  end <- M.getSourcePos
  pure (mkSpan (toSourcePos start) (toSourcePos end), res)

-- | Wrap a parser that expects a Span.
withSpan :: Parser (Span -> a) -> Parser a
withSpan p = do
  start <- M.getSourcePos
  f <- p
  end <- M.getSourcePos
  pure (f (mkSpan (toSourcePos start) (toSourcePos end)))

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
  isDoc <- (True <$ C.char '*') <|> pure False
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
      let allParts = if isNamespaceRel then "namespace" : firstPart : restParts else firstPart : restParts
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
      digits <- underscoreDigits isHexDigit
      let raw = T.pack ['0', pfx] <> digits
      let val = readHexStr (T.filter (/= '_') digits)
      pure (val, raw)

    parseBin = do
      pfx <- M.try (C.char '0' *> (C.char 'b' <|> C.char 'B'))
      digits <- underscoreDigits (\c -> c == '0' || c == '1')
      let raw = T.pack ['0', pfx] <> digits
      let val = readBinStr (T.filter (/= '_') digits)
      pure (val, raw)

    parseExplicitOctal = do
      pfx <- M.try (C.char '0' *> (C.char 'o' <|> C.char 'O'))
      digits <- underscoreDigits (\c -> c >= '0' && c <= '7')
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
  let readStr =
        (if "." `T.isPrefixOf` clean then ("0" <>) else id) .
        (if "." `T.isSuffixOf` clean then (<> "0") else id) $ clean
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
      sgn <- (C.char '+' <|> C.char '-') <|> pure '+'
      digits <- underscoreDigits1 isDigit
      pure (T.pack [e, sgn] <> digits)

-- | String literals: single-quoted (raw) or double-quoted.
literalString :: Parser (Literal Span)
literalString = M.label "string" $ lexeme $ withSpan $ singleQuoted <|> doubleQuoted
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
      (raw, val) <- M.match $ do
        _ <- C.char '"'
        content <- many doubleChar
        _ <- C.char '"'
        pure content
      pure (\sp -> LitString sp (T.pack val) raw)

    doubleChar =
      (C.char '\\' *> (unescape <$> M.anySingle))
      <|> M.satisfy (\c -> c /= '"' && c /= '\\')

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
literalHeredocOrNowdoc :: Parser (Literal Span)
literalHeredocOrNowdoc = M.label "heredoc or nowdoc" $ lexeme $ withSpan $ M.try $ do
  _ <- C.string "<<<"
  _ <- many (C.char ' ' <|> C.char '\t')
  (isNowdoc, tag) <- parseTag
  _ <- C.char '\n' <|> (C.char '\r' *> optional (C.char '\n') *> pure '\n')

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
