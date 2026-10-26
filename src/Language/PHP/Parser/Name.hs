-- | Shared name syntax, independent of the lexer's span and trivia wrappers.
module Language.PHP.Parser.Name (qualifiedNameBody) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C

import Language.PHP.AST (QualifiedName (..), NameKind (..))

-- | Scan one name and build its canonical identity. The caller supplies the
-- identifier reader and its keyword policy; annotation/trivia ownership stays
-- with the existing context-specific wrappers.
qualifiedNameBody
  :: M.MonadParsec e Text m
  => m Text
  -> (NameKind -> Text -> Bool)
  -> m (a -> QualifiedName a)
qualifiedNameBody segment allowed = do
  prefix <- (NameFullyQualified <$ C.char '\\')
    <|> (NameRelative <$ M.try (C.string' "namespace" *> C.char '\\'))
    <|> pure NameUnqualified
  first <- segment
  rest <- M.many (M.try (C.char '\\' *> segment))
  let kind = case prefix of
        NameUnqualified | not (null rest) -> NameQualified
        _ -> prefix
  if allowed kind first
    then pure (\ann -> QualifiedName ann kind (first : rest))
    else M.empty
