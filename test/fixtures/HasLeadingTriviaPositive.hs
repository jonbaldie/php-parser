{-# LANGUAGE OverloadedStrings #-}

-- | Positive compilation fixture for Issue #406.
--
-- This module is intentionally /not/ listed in @php-parser.cabal@ and is
-- never compiled by @cabal build@ or @cabal test@. It exists only so that
-- @scripts/check-has-leading-trivia.sh@ can verify that a custom annotation
-- type with an explicit 'HasLeadingTrivia' instance does compile at a
-- printing call site. The in-suite counterpart is the @MyAnnotation@ test in
-- @Test.PrettySpec@, which also checks the printed output.
module HasLeadingTriviaPositive where

import Data.Text (Text)
import Language.PHP

-- | A custom annotation wrapping the parser's annotation, delegating
-- trivia to it.
newtype CustomAnn = CustomAnn (Annotated Span)

instance HasLeadingTrivia CustomAnn where
  leadingTrivia (CustomAnn ann) = leadingTrivia ann

printCustom :: Program CustomAnn -> Text
printCustom = prettyPrint

printMapped :: Program (Annotated Span) -> Text
printMapped prog = prettyPrint (mapAnnotation CustomAnn prog)
