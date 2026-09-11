{-# LANGUAGE DeriveGeneric #-}

module Language.PHP.Parser.Error
  ( ParseError (..)
  , formatParseError
  , fromMegaparsecError
  ) where

import GHC.Generics (Generic)
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Char (isSpace)
import Text.Megaparsec
  ( ParseErrorBundle (..)
  , PosState (..)
  , SourcePos (..)
  , reachOffsetNoLine
  , unPos
  )
import qualified Text.Megaparsec.Error as MPE
import Language.PHP.Span (Span (..), SourcePos (..))

-- | A structured diagnostic parse error.
data ParseError = ParseError
  { errorSpan     :: !Language.PHP.Span.Span
  , errorExpected :: ![Text]
  , errorFound    :: !(Maybe Text)
  , errorContext  :: ![Text]
  , errorCustom   :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Generic)

-- | Format a parse error as a human-readable diagnostic message.
formatParseError :: ParseError -> Text
formatParseError ParseError{..} =
  let start = spanStart errorSpan
      loc = T.pack (Language.PHP.Span.posFile start)
            <> ":" <> T.pack (show (Language.PHP.Span.posLine start))
            <> ":" <> T.pack (show (Language.PHP.Span.posColumn start))
            <> ": error: "
      msg = case errorCustom of
        Just c -> c
        Nothing ->
          let f = maybe "unexpected end of input" (\tok -> "unexpected " <> tok) errorFound
              e = case errorExpected of
                [] -> ""
                [x] -> ", expecting " <> x
                xs -> ", expecting one of " <> T.intercalate ", " xs
          in f <> e
      ctx = if null errorContext
            then ""
            else "\n  in context: " <> T.intercalate " > " errorContext
  in loc <> msg <> ctx

-- | Convert a Megaparsec error bundle into our structured ParseError.
--
-- The span starts at the error's own offset (not the bundle's initial
-- position) and ends just after the offending token, if there is one.
fromMegaparsecError :: ParseErrorBundle Text Void -> ParseError
fromMegaparsecError (ParseErrorBundle (err :| _) pst) =
  let offset = MPE.errorOffset err
      (found, expected, custom) = extractMPE err
      endOffset = offset + maybe 0 foundLength found
      posAt o =
        let sourcePos = pstateSourcePos (reachOffsetNoLine o pst)
        in Language.PHP.Span.SourcePos
             (sourceName sourcePos)
             (unPos (sourceLine sourcePos))
             (unPos (sourceColumn sourcePos))
             o
  in ParseError
      { errorSpan     = Language.PHP.Span.Span (posAt offset) (posAt endOffset)
      , errorExpected = map showErrorItem (Set.toList expected)
      , errorFound    = fmap showErrorItem found
      , errorContext  = []
      , errorCustom   = custom
      }
  where
    extractMPE = \case
      MPE.TrivialError _ f es -> (fmap trimFound f, es, Nothing)
      MPE.FancyError _ customSet ->
        let msgs = [T.pack m | MPE.ErrorFail m <- Set.toList customSet]
        in (Nothing, Set.empty, if null msgs then Nothing else Just (T.intercalate "; " msgs))

    -- Megaparsec sizes the unexpected chunk to the longest expected
    -- string, so it can run past the offending token (e.g. ";\ne").
    -- Keep only the chunk up to the first whitespace.
    trimFound :: MPE.ErrorItem Char -> MPE.ErrorItem Char
    trimFound = \case
      MPE.Tokens (c :| cs) | not (isSpace c) -> MPE.Tokens (c :| takeWhile (not . isSpace) cs)
      MPE.Tokens (c :| _) -> MPE.Tokens (c :| [])
      item -> item

    foundLength :: MPE.ErrorItem Char -> Int
    foundLength = \case
      MPE.Tokens ts -> length (toList ts)
      _ -> 0

    showErrorItem :: MPE.ErrorItem Char -> Text
    showErrorItem = \case
      MPE.Tokens ts  -> T.pack (show (toList ts))
      MPE.Label lbl  -> T.pack (toList lbl)
      MPE.EndOfInput -> "end of input"
    toList = toList'
    toList' = \case
      x :| xs -> x : xs
