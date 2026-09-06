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
import Text.Megaparsec
  ( ParseErrorBundle (..)
  , PosState (..)
  , SourcePos (..)
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
fromMegaparsecError :: ParseErrorBundle Text Void -> ParseError
fromMegaparsecError (ParseErrorBundle (err :| _) pst) =
  let sourcePos = pstateSourcePos pst
      line = unPos (sourceLine sourcePos)
      col = unPos (sourceColumn sourcePos)
      file = sourceName sourcePos
      offset = pstateOffset pst
      sp = Language.PHP.Span.SourcePos file line col offset
      span' = Language.PHP.Span.Span sp sp
      (found, expected, custom) = extractMPE err
  in ParseError
      { errorSpan     = span'
      , errorExpected = expected
      , errorFound    = found
      , errorContext  = []
      , errorCustom   = custom
      }
  where
    extractMPE = \case
      MPE.TrivialError _ f es ->
        let f' = fmap showErrorItem f
            es' = map showErrorItem (Set.toList es)
        in (f', es', Nothing)
      MPE.FancyError _ customSet ->
        let msgs = [T.pack m | MPE.ErrorFail m <- Set.toList customSet]
        in (Nothing, [], if null msgs then Nothing else Just (T.intercalate "; " msgs))

    showErrorItem :: MPE.ErrorItem Char -> Text
    showErrorItem = \case
      MPE.Tokens ts  -> T.pack (show (toList ts))
      MPE.Label lbl  -> T.pack (toList lbl)
      MPE.EndOfInput -> "end of input"
    toList = toList'
    toList' = \case
      x :| xs -> x : xs
