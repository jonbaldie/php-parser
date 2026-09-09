{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Parser
  ( -- * Parsing functions (Strict Text)
    parseProgram
  , parseStatement
  , parseExpression
    -- * Parsing functions (Lazy Text)
  , parseProgramLazy
  , parseStatementLazy
  , parseExpressionLazy
    -- * Specific Sub-Parsers
  , P.parseProgramBody
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict (evalState, get, put)
import qualified Data.Set as Set
import Language.PHP.AST (Program, Stmt, Expr, Trivia, Annotated (..))
import Language.PHP.Span (Span)
import Language.PHP.Parser.Error (ParseError, fromMegaparsecError)
import Language.PHP.Parser.Lexer (runPHPParser, sc)
import qualified Language.PHP.Parser.Statement as P

-- | Parse a full PHP script/program from strict Text.
parseProgram :: FilePath -> Text -> Either ParseError (Program (Annotated Span))
parseProgram filePath input =
  case runPHPParser P.parseProgram filePath input of
    Left err -> Left (fromMegaparsecError err)
    Right (prog, trivBySpan) -> Right (annotateWithTrivia trivBySpan prog)

-- | Parse an isolated statement from strict Text (skipping required opening <?php tags).
parseStatement :: FilePath -> Text -> Either ParseError (Stmt (Annotated Span))
parseStatement filePath input =
  case runPHPParser (sc *> P.parseStmt) filePath input of
    Left err -> Left (fromMegaparsecError err)
    Right (stmt, trivBySpan) -> Right (annotateWithTrivia trivBySpan stmt)

-- | Parse a standalone expression from strict Text.
parseExpression :: FilePath -> Text -> Either ParseError (Expr (Annotated Span))
parseExpression filePath input =
  case runPHPParser (sc *> P.parseExpr) filePath input of
    Left err -> Left (fromMegaparsecError err)
    Right (expr, trivBySpan) -> Right (annotateWithTrivia trivBySpan expr)

annotateWithTrivia :: Traversable f => Map.Map Span [Trivia] -> f Span -> f (Annotated Span)
annotateWithTrivia trivBySpan ast = evalState (traverse annotateSpan ast) Set.empty
  where
    annotateSpan sp = do
      seen <- get
      put (Set.insert sp seen)
      let leading = if Set.member sp seen then [] else Map.findWithDefault [] sp trivBySpan
      pure (Annotated sp leading)

-- | Parse a full PHP script from lazy Text.
parseProgramLazy :: FilePath -> TL.Text -> Either ParseError (Program (Annotated Span))
parseProgramLazy filePath = parseProgram filePath . TL.toStrict

-- | Parse an isolated statement from lazy Text.
parseStatementLazy :: FilePath -> TL.Text -> Either ParseError (Stmt (Annotated Span))
parseStatementLazy filePath = parseStatement filePath . TL.toStrict

-- | Parse a standalone expression from lazy Text.
parseExpressionLazy :: FilePath -> TL.Text -> Either ParseError (Expr (Annotated Span))
parseExpressionLazy filePath = parseExpression filePath . TL.toStrict
