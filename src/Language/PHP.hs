-- |
-- Module      : Language.PHP
-- Description : Idiomatic, functional-pearl parser for PHP 8.2 through 8.5
-- Copyright   : (c) Jonathan Baldie, 2026
-- License     : BSD-3-Clause
-- Maintainer  : jonathan@jonbaldie.com
-- Stability   : experimental
-- Portability : POSIX / Windows
--
-- A purely functional Haskell library for parsing modern PHP source code
-- (covering PHP 8.2, 8.3, 8.4, and 8.5), designed in the spirit of a functional pearl.
--
-- Features supported include:
--
-- * PHP 8.2 Disjunctive Normal Form (DNF) types, readonly classes, trait constants, standalone types
-- * PHP 8.3 typed class constants, dynamic class constant fetch (@Class::{$var}@), anonymous readonly classes
-- * PHP 8.4 property hooks (@get@/@set@ blocks & expressions), asymmetric visibility, member dereferencing on instantiation
-- * PHP 8.5 pipe operator (@|>@), clone-with syntax, static asymmetric visibility
-- * First-class callable syntax (@fn(...)@), match expressions, nullsafe chains, non-capturing catch
-- * Total, referentially transparent diagnostic error reporting with precise source spans
-- * Wadler/Leijen algebraic pretty printer with round-trip invariance
-- * Recursion schemes and catamorphic folds for tree queries and transformations
module Language.PHP
  ( -- * Parsing API
    parseProgram
  , parseStatement
  , parseExpression
  , parseProgramLazy
  , parseStatementLazy
  , parseExpressionLazy

    -- * Pretty Printing API
  , prettyPrint
  , prettyPrintStmt
  , prettyPrintExpr
  , prettyPrintType

    -- * Traversal & Recursion Schemes
  , stripAnnotations
  , mapAnnotation
  , foldExpr
  , foldStmt
  , queryExpr
  , queryStmt
  , transformExpr
  , transformStmt
  , allExprs
  , allVariables

    -- * Abstract Syntax Tree (AST)
  , Program (..)
  , Stmt (..)
  , Expr (..)
  , Literal (..)
  , Var (..)
  , VarName (..)
  , Ident (..)
  , QualifiedName (..)
  , NameKind (..)
  , Type (..)
  , Visibility (..)
  , PropertyModifier (..)
  , MethodModifier (..)
  , ClassModifier (..)
  , PropertyHook (..)
  , HookType (..)
  , HookBody (..)
  , PropertyDecl (..)
  , ConstDecl (..)
  , ClassMember (..)
  , Param (..)
  , MethodDecl (..)
  , FunctionDecl (..)
  , ClassDecl (..)
  , InterfaceDecl (..)
  , TraitDecl (..)
  , EnumDecl (..)
  , EnumCase (..)
  , TraitUse (..)
  , TraitAdaptation (..)
  , AttributeGroup (..)
  , Attribute (..)
  , Arg (..)
  , MatchArm (..)
  , CatchClause (..)
  , SwitchCase (..)
  , ArrayItem (..)
  , CallArgs (..)
  , ClassTarget (..)
  , ClassConstName (..)
  , MemberName (..)
  , BinOp (..)
  , UnOp (..)
  , CastType (..)
  , IncludeType (..)
  , UseType (..)
  , UseClause (..)
  , Trivia (..)
  , Annotated (..)
  , getAnnotation

    -- * Source Spans and Positions
  , Span (..)
  , SourcePos (..)
  , emptySpan
  , mkSpan
  , combineSpans
  , prettySpan

    -- * Structured Diagnostics and Errors
  , ParseError (..)
  , formatParseError
  ) where

import Language.PHP.AST
import Language.PHP.Span
import Language.PHP.Fold
import Language.PHP.Parser
import Language.PHP.Parser.Error
import Language.PHP.Pretty
