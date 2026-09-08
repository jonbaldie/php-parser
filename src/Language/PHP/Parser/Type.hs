{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Parser.Type
  ( parseType
  , parseReturnType
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C

import Language.PHP.AST
import Language.PHP.Span (Span (..), combineSpans)
import Language.PHP.Parser.Lexer

-- | Allowed keywords that can be used as unqualified type names.
allowedTypeKeywords :: [Text]
allowedTypeKeywords = ["array", "callable", "static"]

-- | Parse any PHP type (simple, nullable, union, intersection, DNF).
parseType :: Parser (Type Span)
parseType = M.label "type" $ do
  parseUnionOrIntersection

-- | Parse optional return type after colon.
parseReturnType :: Parser (Maybe (Type Span))
parseReturnType = (colon *> (Just <$> parseType)) <|> pure Nothing

-- | Parse atomic type: standalone keyword/name, nullable ?Type, or parenthesized intersection (A&B).
parseAtomicType :: Parser (Type Span)
parseAtomicType = nullableType <|> parenthesizedIntersection <|> simpleType
  where
    nullableType = withSpan $ do
      _ <- symbol "?"
      inner <- simpleType
      pure (\sp -> NullableType sp inner)

    parenthesizedIntersection = parens $ do
      (sp, inner) <- spanned parseIntersectionOnly
      pure (DNFType sp [inner])

    simpleType = withSpan $ do
      qn <- parseTypeName
      pure (\sp -> SimpleType sp qn)

-- | Parse a qualified or unqualified type name (including built-ins).
parseTypeName :: Parser (QualifiedName Span)
parseTypeName = M.try $ withSpan $ do
  isFQ <- (True <$ C.char '\\') <|> pure False
  firstPart <- rawIdentifier
  restParts <- M.many (C.char '\\' *> rawIdentifier)
  if T.toLower firstPart == "var" || (not isFQ && null restParts && isKeyword firstPart && T.toLower firstPart `notElem` allowedTypeKeywords)
    then M.empty
    else do
      _ <- sc
      let allParts = firstPart : restParts
      let kind
            | isFQ = NameFullyQualified
            | null restParts = NameUnqualified
            | otherwise = NameQualified
      pure (\sp -> QualifiedName sp kind allParts)

-- | Parse intersection type (A&B&C).
parseIntersectionOnly :: Parser (Type Span)
parseIntersectionOnly = do
  (sp, types) <- spanned $ do
    t1 <- parseAtomicNonParen
    rest <- M.some (symbol "&" *> parseAtomicNonParen)
    pure (t1 : rest)
  pure (IntersectionType sp types)
  where
    parseAtomicNonParen = withSpan $ do
      qn <- parseTypeName
      pure (\sp -> SimpleType sp qn)

-- | Parse union or intersection or DNF type.
parseUnionOrIntersection :: Parser (Type Span)
parseUnionOrIntersection = do
  t1 <- parseIntersectionOrAtomic
  moreUnion <- M.many (symbol "|" *> parseIntersectionOrAtomic)
  case moreUnion of
    [] -> pure t1
    rest -> do
      let allTypes = t1 : rest
      let sp = combineSpans (typeSpan t1) (typeSpan (last allTypes))
      pure (UnionType sp allTypes)

parseIntersectionOrAtomic :: Parser (Type Span)
parseIntersectionOrAtomic = do
  t1 <- parseAtomicType
  moreInter <- M.many (symbol "&" *> parseAtomicType)
  case moreInter of
    [] -> pure t1
    rest -> do
      let allTypes = t1 : rest
      let sp = combineSpans (typeSpan t1) (typeSpan (last allTypes))
      pure (IntersectionType sp allTypes)

typeSpan :: Type Span -> Span
typeSpan = \case
  SimpleType sp _       -> sp
  NullableType sp _     -> sp
  UnionType sp _        -> sp
  IntersectionType sp _ -> sp
  DNFType sp _          -> sp

