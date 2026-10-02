-- |
-- Module      : Language.PHP.Parser.Script
-- Description : PHP's script-structure rules, as one pure pass
--
-- Some of PHP's rules are facts about a file's top-level statement list
-- rather than about any one statement: where @declare(strict_types=…)@ and
-- @declare(encoding=…)@ may appear, where the first namespace may appear,
-- whether bracketed and unbracketed namespaces are mixed, that no code may sit
-- outside a bracketed namespace, and that namespaces do not nest.
--
-- The grammar already keeps @namespace@, @use@, @const@ and
-- @__halt_compiler@ out of inner statement lists, so every namespace this pass
-- sees is at the top level or directly inside a bracketed namespace body.
-- 'checkScript' judges the finished list.
module Language.PHP.Parser.Script
  ( ScriptItem (..)
  , Check
  , checkScript
  ) where

import Control.Monad (foldM, void)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PHP.AST
import Language.PHP.Fold (foldStmt)
import Language.PHP.Span (Span (..), SourcePos (..), emptySpan)

-- | One element of a file's top-level statement list.
--
-- A close tag that does not itself end a statement is an empty statement to
-- PHP's ordering rules, but the AST does not record it, so the parser reports
-- it here as a 'BareCloseTag'. PHP 8.5 treats it as a nop before
-- @declare(strict_types=…)@ where 8.2-8.4 do not; that difference is #338.
data ScriptItem
  = ScriptStmt (Stmt Span)
  | BareCloseTag
  deriving (Eq, Show)

-- | A rule check: the first offending statement's span and PHP's message.
type Check = Either (Span, Text)

data NamespaceForm = Bracketed | Unbracketed
  deriving (Eq, Show)

namespaceForm :: Maybe [Stmt Span] -> NamespaceForm
namespaceForm = maybe Unbracketed (const Bracketed)

-- | What the statements before the current one allow.
data Seen = Seen
  { -- | Only declare statements so far: the declare prologue is still open.
    inPrologue :: !Bool
    -- | Something other than a declare or a nop came first, so a first
    -- namespace would be too late.
  , namespaceBlocked :: !Bool
    -- | The form of the file's first namespace, once there is one.
  , firstForm :: !(Maybe NamespaceForm)
  }

-- | Check a file's top-level statement list against PHP's script-structure
-- rules, reporting the first offending statement and PHP's message for it.
checkScript :: [ScriptItem] -> Check ()
checkScript = void . foldM step start . dropShebang
  where
    start = Seen { inPrologue = True, namespaceBlocked = False, firstForm = Nothing }

    -- A leading shebang line is not a statement: PHP skips it before reading
    -- the file's statement list.
    dropShebang (ScriptStmt (StmtInlineHtml sp html) : rest)
      | isLeadingShebang sp html = rest
    dropShebang other = other

step :: Seen -> ScriptItem -> Check Seen
step seen = \case
  BareCloseTag -> Right seen { inPrologue = False }
  ScriptStmt stmt -> case stmt of
    StmtEmpty _ -> Right seen { inPrologue = False }
    StmtHaltCompiler _ _ -> Right seen { inPrologue = False }
    StmtDeclare sp dirs body -> do
      checkDeclare (inPrologue seen) sp dirs
      mapM_ (mapM_ checkNested) body
      outsideNamespace seen sp
      Right seen
    StmtNamespace sp _ body -> do
      let form = namespaceForm body
      case firstForm seen of
        Nothing | namespaceBlocked seen -> Left (sp, namespaceTooLate)
        Just first | first /= form -> Left (sp, formsMixed)
        _ -> Right ()
      mapM_ (mapM_ (checkNamespaceBody form)) body
      Right seen
        { inPrologue = False
        , namespaceBlocked = True
        , firstForm = Just (fromMaybe form (firstForm seen))
        }
    _ -> do
      -- The statement itself is not a declare, so this only reaches below it.
      checkNested stmt
      outsideNamespace seen (stmtSpan stmt)
      Right seen { inPrologue = False, namespaceBlocked = True }

-- | Once a file has a bracketed namespace, only namespaces, declares before
-- the first one, nops and @__halt_compiler@ may appear outside one.
outsideNamespace :: Seen -> Span -> Check ()
outsideNamespace seen sp
  | firstForm seen == Just Bracketed = Left (sp, "No code may exist outside of namespace {}")
  | otherwise = Right ()

-- | A statement directly inside a bracketed namespace body. A namespace there
-- is mixed if its form differs and nested otherwise, which is the order PHP
-- checks them in.
checkNamespaceBody :: NamespaceForm -> Stmt Span -> Check ()
checkNamespaceBody outer = \case
  StmtNamespace sp _ body
    | namespaceForm body /= outer -> Left (sp, formsMixed)
    | otherwise -> Left (sp, "Namespace declarations cannot be nested")
  stmt -> checkNested stmt

-- | Check a statement below the top level, and everything inside it. No
-- declare there is in the declare prologue.
checkNested :: Stmt Span -> Check ()
checkNested = sequence_ . foldStmt nestedDeclare
  where
    nestedDeclare = \case
      StmtDeclare sp dirs _ -> [checkDeclare False sp dirs]
      _ -> []

-- | @strict_types@ and @encoding@ belong to the declare prologue: the
-- script's leading run of declare statements.
checkDeclare :: Bool -> Span -> [DeclareDirective Span] -> Check ()
checkDeclare prologue sp dirs
  | not prologue && any (named "strict_types") dirs =
      Left (sp, "strict_types declaration must be the very first statement in the script")
  | not prologue && any (named "encoding") dirs =
      Left (sp, "Encoding declaration pragma must be the very first statement in the script")
  | otherwise = Right ()
  where
    named n (DeclareDirective _ (Ident _ name) _) = T.toLower name == n

namespaceTooLate :: Text
namespaceTooLate =
  "Namespace declaration statement has to be the very first statement or after any declare call in the script"

formsMixed :: Text
formsMixed = "Cannot mix bracketed namespace declarations with unbracketed namespace declarations"

-- | Every statement constructor carries its span as its first field, which is
-- where the derived 'Foldable' starts.
stmtSpan :: Stmt Span -> Span
stmtSpan = fromMaybe emptySpan . listToMaybe . toList

-- | A shebang is not a statement. PHP skips a leading @#!@ line before the
-- file statement list, so it does not push a following namespace out of place.
isLeadingShebang :: Span -> Text -> Bool
isLeadingShebang sp html =
  posOffset (spanStart sp) == 0 && isShebangLine (T.unpack html)
  where
    isShebangLine ('#':'!':rest) =
      case span (not . isNewline) rest of
        (_, []) -> True
        (_, '\r':'\n':after) -> null after
        (_, nl:after) -> isNewline nl && null after
    isShebangLine _ = False
    isNewline c = c == '\n' || c == '\r'
