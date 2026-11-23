{-# LANGUAGE OverloadedStrings #-}

-- | Negative compilation fixture for Issue #406.
--
-- This module is intentionally /not/ listed in @php-parser.cabal@ and is
-- never compiled by @cabal build@ or @cabal test@. It exists only so that
-- @scripts/check-has-leading-trivia.sh@ can verify that printing a program
-- whose annotation type has no 'HasLeadingTrivia' instance fails to compile
-- with "No instance for 'HasLeadingTrivia CustomAnn'".
--
-- If this module ever compiles, the catch-all instance has returned.
module HasLeadingTriviaNegative where

import Data.Text (Text)
import Language.PHP

-- | A custom annotation type with no 'HasLeadingTrivia' instance.
data CustomAnn = CustomAnn

-- | Printing with this annotation must not type-check.
printCustom :: Program CustomAnn -> Text
printCustom = prettyPrint
