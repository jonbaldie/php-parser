{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.PHP.Span
  ( SourcePos (..)
  , Span (..)
  , HasSpan (..)
  , emptySpan
  , mkSpan
  , combineSpans
  , prettySpan
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T

-- | A position in a source file.
data SourcePos = SourcePos
  { posFile   :: !FilePath
  , posLine   :: !Int
  , posColumn :: !Int
  , posOffset :: !Int
  } deriving (Eq, Ord, Show, Generic)

-- | A source span between two positions.
data Span = Span
  { spanStart :: !SourcePos
  , spanEnd   :: !SourcePos
  } deriving (Eq, Ord, Show, Generic)

-- | Types that have an associated source span.
class HasSpan a where
  getSpan :: a -> Span

instance HasSpan Span where
  getSpan = id

-- | An empty / dummy span.
emptySpan :: Span
emptySpan = Span (SourcePos "" 0 0 0) (SourcePos "" 0 0 0)

-- | Create a span from start and end positions.
mkSpan :: SourcePos -> SourcePos -> Span
mkSpan = Span

-- | Combine two spans covering the extent from start of first to end of second.
combineSpans :: Span -> Span -> Span
combineSpans (Span s1 _) (Span _ e2) = Span s1 e2

-- | Human-readable string representation of a span.
prettySpan :: Span -> Text
prettySpan (Span s e)
  | posFile s == posFile e && posLine s == posLine e =
      T.pack (posFile s) <> ":" <> T.pack (show (posLine s)) <> ":" <>
      T.pack (show (posColumn s)) <> "-" <> T.pack (show (posColumn e))
  | otherwise =
      T.pack (posFile s) <> ":" <> T.pack (show (posLine s)) <> ":" <>
      T.pack (show (posColumn s)) <> " - " <>
      T.pack (posFile e) <> ":" <> T.pack (show (posLine e)) <> ":" <>
      T.pack (show (posColumn e))
