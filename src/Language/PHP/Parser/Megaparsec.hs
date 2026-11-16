module Language.PHP.Parser.Megaparsec
  ( module Megaparsec
  , lookAhead
  ) where

import Text.Megaparsec as Megaparsec hiding (lookAhead)
import Language.PHP.Parser.Lexer (lookAhead)
