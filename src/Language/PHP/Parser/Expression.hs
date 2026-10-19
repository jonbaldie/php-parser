{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Parser.Expression
  ( parseExpr
  , parseExprWith
  , parseExprWithContext
  , parseExprWithContextAndBody
  , parseExprWithGrammar
  , Grammar (..)
  , parsePrimaryExpr
  , parseArg
  , parseArgList
  , parseCallArgs
  , parseParamList
  , parseMatchArm
  , parseArrayItem
  , parseAttributes
  , parseAttributeGroup
  , parseAttribute
  , parseLiteral
  , parseLiteralWith
  , exprSpan
  , hasEmptyDestructure
  , isAssignable
  , isWritable
  ) where

import qualified Text.Megaparsec as M
import Language.PHP.AST
import Language.PHP.Span (Span)
import Language.PHP.Parser.Lexer (Parser)
import Language.PHP.Parser.Internal
  ( Grammar (..)
  , parseExprWithGrammar
  , parseArgList
  , parseCallArgsWith
  , parseMatchArmWith
  , parseArrayItemWith
  , parseLiteralWith
  , parseParamList
  , exprSpan
  , hasEmptyDestructure
  , isAssignable
  , isWritable
  )
import qualified Language.PHP.Parser.Statement as Statement

-- | Parse expression bound to the complete statement and class member grammar.
parseExpr :: Parser (Expr Span)
parseExpr = Statement.parseExpr

-- | Parse expression parameterized by statement and class member parsers.
parseExprWith :: Parser (Stmt Span) -> Parser (ClassMember Span) -> Parser (Expr Span)
parseExprWith pStmt pMember = parseExprWithContext pStmt (const pMember)

-- | Parse an expression with a class member parser that receives the
-- enclosing class's readonly status for anonymous classes.
parseExprWithContext :: Parser (Stmt Span) -> (Bool -> Parser (ClassMember Span)) -> Parser (Expr Span)
parseExprWithContext pStmt pMember =
  parseExprWithContextAndBody (M.many pStmt) pMember

-- | Parse an expression with a separate parser for statement lists embedded
-- in closures. Callers that parse mixed PHP/HTML bodies can supply a body
-- parser that consumes tag transitions without inventing empty statements.
parseExprWithContextAndBody
  :: Parser [Stmt Span]
  -> (Bool -> Parser (ClassMember Span))
  -> Parser (Expr Span)
parseExprWithContextAndBody parseBody pMember =
  parseExprWithGrammar Grammar
    { gBody   = parseBody
    , gMember = pMember
    , gParam  = Statement.parseParamInContext Statement.NonConstructorParam
    }

-- | Primary expression parser exposed to public / tests.
parsePrimaryExpr :: Parser (Expr Span)
parsePrimaryExpr = Statement.parseExpr

-- | Single call argument using expression parser.
parseArg :: Parser (Arg Span)
parseArg = Statement.parseArg

-- | Call arguments list: (arg1, arg2) or first-class callable (...)
parseCallArgs :: Parser (CallArgs Span)
parseCallArgs = parseCallArgsWith Statement.parseExpr

-- | Match arm using expression parser.
parseMatchArm :: Parser (MatchArm Span)
parseMatchArm = parseMatchArmWith Statement.parseExpr

-- | Array item using expression parser.
parseArrayItem :: Parser (ArrayItem Span)
parseArrayItem = parseArrayItemWith Statement.parseExpr

-- | Attributes #[ ... ]
parseAttributes :: Parser [AttributeGroup Span]
parseAttributes = Statement.parseAttributes

-- | Parse a single attribute group @#[ ... ]@.
parseAttributeGroup :: Parser (AttributeGroup Span)
parseAttributeGroup = Statement.parseAttributeGroup

-- | Parse an attribute declaration within an attribute group.
parseAttribute :: Parser (Attribute Span)
parseAttribute = Statement.parseAttribute

-- | Parse a literal using default expression parser.
parseLiteral :: Parser (Literal Span)
parseLiteral = parseLiteralWith Statement.parseExpr
