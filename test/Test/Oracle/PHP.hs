{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.Oracle.PHP
-- Description : A real @php@ binary consulted as the oracle for what PHP accepts
--
-- The properties in "Test.CompatibilitySpec" check the library against itself.
-- This module supplies the only thing that can check it against the language:
-- a real interpreter.
--
-- @php -l@ is the query, and it is a stronger oracle than its name suggests --
-- it performs a full compile without executing, so it reports declaration-local
-- semantic rules (a @void@ property type, an abstract private method, a
-- duplicate parameter name) as well as grammar errors.
--
-- Everything variable about talking to an interpreter -- which binary, how it is
-- invoked, caching, diagnostic parsing -- lives behind 'PHPOracle' and
-- 'checkSource', so the properties never learn that a subprocess exists.
module Test.Oracle.PHP
  ( -- * Verdicts
    Verdict (..)
  , DiagKind (..)
  , Diagnostic (..)
  , verdictAccepted

    -- * The oracle
  , PHPOracle
  , oracleBinary
  , oracleReportedVersion
  , resolveOracles
  , checkSource

    -- * Diagnostics
  , classifyDiagnostic
  , outOfContract

    -- * Environment
  , oracleRequiredVar
  , oracleRequired
  , binaryEnvVar
  ) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Data.Char (isDigit)
import Data.IORef
import Data.List (find, isInfixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Gen.PHPSource (PHPVersion (..), allVersions, versionLabel)

--------------------------------------------------------------------------------
-- Verdicts
--------------------------------------------------------------------------------

-- | Which tier of PHP's rejection this is. @php -l@ reports grammar failures as
-- @Parse error:@ and everything the compiler rejects afterwards as
-- @Fatal error:@, which is exactly the split between "this is not PHP" and
-- "this is PHP, but it cannot be compiled".
data DiagKind
  = ParseError
  | CompileFatal
  deriving (Eq, Show)

-- | The interpreter's own words. The library is never asked to reproduce the
-- wording -- only the decision -- so this is used for classification and for
-- putting a readable reason in a counterexample.
data Diagnostic = Diagnostic
  { diagKind :: DiagKind
  , diagMessage :: Text
  , diagLine :: Int
  }
  deriving (Eq, Show)

data Verdict
  = Accepted
  | Rejected Diagnostic
  deriving (Eq, Show)

verdictAccepted :: Verdict -> Bool
verdictAccepted = \case
  Accepted -> True
  Rejected _ -> False

--------------------------------------------------------------------------------
-- The oracle
--------------------------------------------------------------------------------

-- | A resolved interpreter. Opaque: the properties may ask it questions but may
-- not learn how it answers them.
data PHPOracle = PHPOracle
  { oracleBinary :: FilePath
  , -- | What the binary says it is, not what its filename claims.
    oracleReportedVersion :: Text
  , oracleCache :: IORef (Map Text Verdict)
  }

-- | @php.ini@ is ignored and error display is forced on, so a contributor's
-- local configuration cannot change a verdict relative to CI. Without
-- @display_errors@ the lint diagnostic goes to stderr alone.
lintArgs :: [String]
lintArgs = ["-n", "-d", "display_errors=1", "-l"]

versionArgs :: [String]
versionArgs = ["-n", "-r", "echo PHP_MAJOR_VERSION . \".\" . PHP_MINOR_VERSION;"]

-- | Per-version override, e.g. @PHP82_BIN=/opt/php-8.2/bin/php@.
binaryEnvVar :: PHPVersion -> String
binaryEnvVar v = "PHP" ++ filter isDigit (versionLabel v) ++ "_BIN"

-- | Conventionally named binaries to look for on @PATH@, most specific first.
-- A bare @php@ is tried last and is accepted only if it reports the version
-- being looked for.
binaryPathNames :: PHPVersion -> [String]
binaryPathNames v =
  [ "php" ++ versionLabel v
  , "php" ++ filter isDigit (versionLabel v)
  , "php"
  ]

-- | Set this in CI so that a missing or mismatched binary fails the build
-- instead of degrading to a skip.
oracleRequiredVar :: String
oracleRequiredVar = "PHP_ORACLE_REQUIRED"

oracleRequired :: IO Bool
oracleRequired = maybe False (`notElem` ["", "0", "false", "no"]) <$> lookupEnv oracleRequiredVar

-- | Find one interpreter per supported version. Every version gets an entry:
-- @Left@ carries the reason it is unavailable, so a skipped group can say which
-- binaries were tried and what they turned out to be.
--
-- Discovery verifies rather than trusts. A binary called @php8.3@ that reports
-- 8.4 is rejected for the 8.3 slot, so a mislabelled binary cannot make a
-- version group pass vacuously.
resolveOracles :: IO (Map PHPVersion (Either String PHPOracle))
resolveOracles = M.fromList <$> traverse (\v -> (,) v <$> resolveOracle v) allVersions

resolveOracle :: PHPVersion -> IO (Either String PHPOracle)
resolveOracle v = do
  fromEnv <- lookupEnv (binaryEnvVar v)
  onPath <- catMaybes <$> traverse findExecutable (binaryPathNames v)
  let cands = maybe [] pure fromEnv ++ onPath
  probed <- traverse probe cands
  case find ((== Just want) . snd) probed of
    Just (bin, _) -> do
      cache <- newIORef M.empty
      pure (Right (PHPOracle bin want cache))
    Nothing -> pure (Left (unavailable probed))
  where
    want = T.pack (versionLabel v)

    probe bin = do
      r <- try (readProcessWithExitCode bin versionArgs "")
      pure . (,) bin $ case r of
        Left (_ :: IOException) -> Nothing
        Right (ExitSuccess, out, _) -> Just (T.strip (T.pack out))
        Right _ -> Nothing

    unavailable [] =
      "no PHP "
        ++ versionLabel v
        ++ " binary found (set "
        ++ binaryEnvVar v
        ++ ", or put one of "
        ++ unwords (binaryPathNames v)
        ++ " on PATH)"
    unavailable probed =
      "no binary reporting PHP "
        ++ versionLabel v
        ++ "; tried "
        ++ unwords [bin ++ " (" ++ maybe "unusable" (("reports " ++) . T.unpack) rv ++ ")" | (bin, rv) <- probed]

-- | Ask the interpreter whether it accepts this source.
--
-- Verdicts are memoised on the source text. Shrinking a counterexample re-checks
-- many near-identical programs, and an ordinary map keyed on the text costs
-- nothing beyond the packages the suite already depends on.
checkSource :: PHPOracle -> Text -> IO Verdict
checkSource o src = do
  cached <- M.lookup src <$> readIORef (oracleCache o)
  case cached of
    Just v -> pure v
    Nothing -> do
      v <- lint o src
      -- tasty runs tests concurrently, so two properties can miss on the same
      -- source at once. Atomic insertion keeps a losing writer from discarding
      -- the other's entry; the verdicts themselves cannot disagree.
      atomicModifyIORef' (oracleCache o) (\m -> (M.insert src v m, ()))
      pure v

-- | The verdict is the exit status; the diagnostic is the message.
--
-- PHP CLI writes the lint diagnostic to /both/ stdout and stderr, each prefixed
-- with a newline, so reading both would duplicate every message. stdout is the
-- one @display_errors=1@ guarantees, and stderr is consulted only when stdout
-- carried nothing recognisable -- a build that routes diagnostics elsewhere
-- then loses the wording but never the verdict, which is the exit status.
lint :: PHPOracle -> Text -> IO Verdict
lint o src = do
  (code, out, err) <- readProcessWithExitCode (oracleBinary o) lintArgs (T.unpack src)
  pure $ case code of
    ExitSuccess -> Accepted
    _ -> Rejected (diagnosticFrom out err)
  where
    diagnosticFrom out err =
      fromMaybe (unparsed out err) $
        classifyDiagnostic (T.pack out) <|> classifyDiagnostic (T.pack err)
    unparsed out err = Diagnostic CompileFatal (T.strip (T.pack (out ++ err))) 0

-- | Pull the first diagnostic out of @php -l@'s stdout.
--
-- The shape is a leading newline, the diagnostic, then a trailing
-- @Errors parsing ...@ line:
--
-- > \nParse error: Invalid numeric literal in Standard input code on line 1\nErrors parsing Standard input code\n
classifyDiagnostic :: Text -> Maybe Diagnostic
classifyDiagnostic out = do
  line <- find (not . T.null) (map T.strip (T.lines out))
  (kind, rest) <- listToMaybe [(k, r) | (p, k) <- prefixes, Just r <- [T.stripPrefix p line]]
  let (body, ln) = splitLocation rest
  pure (Diagnostic kind (T.strip body) ln)
  where
    prefixes = [("Parse error:", ParseError), ("Fatal error:", CompileFatal)]

-- | Split @<message> in <file> on line <n>@ into its message and line number.
-- A diagnostic without a recognisable location keeps its whole text and line 0.
splitLocation :: Text -> (Text, Int)
splitLocation t = case T.breakOnEnd " on line " t of
  (before, after)
    | not (T.null before)
    , Just n <- readDigits after ->
        -- The location suffix comes off first: dropping the file name from a
        -- string that still ends in @" on line "@ makes the two drops overlap
        -- and eats the end of the message.
        (dropFileSuffix (T.dropEnd (T.length " on line ") before), n)
  _ -> (t, 0)
  where
    dropFileSuffix s = case T.breakOnEnd " in " s of
      (b, _) | not (T.null b) -> T.dropEnd (T.length " in ") b
      _ -> s
    readDigits s =
      let ds = T.takeWhile isDigit (T.strip s)
       in if T.null ds then Nothing else Just (read (T.unpack ds))

--------------------------------------------------------------------------------
-- The contract boundary
--------------------------------------------------------------------------------

-- | Rejections this library is not expected to reproduce, each with the reason
-- it is excluded.
--
-- This is the most dangerous artifact in the differential suite: anything listed
-- here disappears from the test's attention. It is therefore kept in one place,
-- kept short, and every entry carries its justification. Adding an entry is a
-- design change, not a fix.
--
-- The boundary is drawn at name resolution. @php -l@ rejects in three tiers:
-- grammar (in contract), rules decidable from a single declaration (in
-- contract -- this is what issues #198, #199 and #204-#208 implemented), and
-- rules needing a program-wide symbol table (out of contract, because this is a
-- parser and maintains none).
--
-- Duplicate members /within/ one class are deliberately absent: they need a
-- per-declaration member table, not a program-wide one, so they are in contract.
outOfContractRules :: [(String, String)]
outOfContractRules =
  [ ( "because the name is already in use"
    , "two `use` statements binding the same name: needs a program-wide import table"
    )
  , ( "previously declared in"
    , "a name redeclared across two top-level declarations: needs a program-wide symbol table"
    )
  ]

-- | The justification for ignoring this diagnostic, when there is one.
outOfContract :: Diagnostic -> Maybe String
outOfContract d =
  snd <$> find (\(pat, _) -> pat `isInfixOf` T.unpack (diagMessage d)) outOfContractRules
