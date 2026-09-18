{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.OracleSpec
-- Description : Differential properties against a real PHP interpreter
--
-- "Test.CompatibilitySpec" checks the library against its own documented
-- invariants. This module checks it against PHP, by putting the same source in
-- front of a real interpreter ("Test.Oracle.PHP") and comparing decisions.
--
-- Four properties, in two directions:
--
-- 1. /corpus health/ -- every program the generator calls valid for version @v@
--    really is accepted by a PHP @v@ interpreter. Without this the other
--    properties can pass on a corpus of garbage.
-- 2. /no false rejects/ -- if PHP @v@ accepts it, the library parses it.
-- 3. /no false accepts/ -- if the library parses it, /some/ supported version
--    accepts it. The library has no target version, so this direction is
--    version-agnostic.
-- 4. /the printer emits PHP/ -- printing a parsed program yields source a real
--    interpreter still accepts.
--
-- Properties 1, 2 and 4 draw from the valid corpus. Property 3 can only say
-- anything on programs worth rejecting, so it draws from the mutation layer
-- ("Test.Gen.PHPMutation"), and each mutation's current outcome is pinned
-- two-sidedly: a bug that gets fixed fails this suite and forces the pin to be
-- updated in the same change.
--
-- With no interpreter on @PATH@ the whole group degrades to a skip, which is the
-- normal local outcome. Setting @PHP_ORACLE_REQUIRED@ turns that skip into a
-- failure, which is what CI does.
module Test.OracleSpec (oracleTests) where

import Control.Exception (bracket_)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Data.Text as T
-- The library and the oracle both have a @ParseError@; the oracle's is the one
-- this module talks about, and the library's type is never named here.
import Language.PHP hiding (ParseError)
import System.Directory
  ( createDirectoryIfMissing
  , emptyPermissions
  , getTemporaryDirectory
  , removePathForcibly
  , setOwnerExecutable
  , setOwnerReadable
  , setPermissions
  )
import System.Environment (setEnv, unsetEnv)
import Test.Gen.PHPMutation
import Test.Gen.PHPSource
import Test.Oracle.PHP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

type Oracles = Map PHPVersion (Either String PHPOracle)

-- | Discovery runs once, before the tree is built, so that a missing
-- interpreter can be named in the /test names/ rather than silently turning
-- properties into vacuous passes. A skipped oracle and a passing oracle must not
-- read the same, and on a machine without PHP the skip is the normal outcome.
--
-- Each property costs one subprocess per generated program, so the floor is
-- lower than the hermetic suite's. @--quickcheck-tests=N@ still wins when larger.
oracleTests :: IO TestTree
oracleTests = do
  oracles <- resolveOracles
  pure $
    adjustOption (max (QuickCheckTests 50)) $
      testGroup
        ("PHP interpreter oracle (differential)" ++ groupSkip oracles)
        [ testGroup "Oracle handle" (handleTests oracles)
        , testGroup "Mutation layer" mutationLayerTests
        , testGroup
            "Differential properties"
            ( map (versionProperties oracles) allVersions
                ++ [noFalseAccepts oracles, divergenceTable oracles]
            )
        , knownDivergenceTests oracles
        ]
  where
    groupSkip oracles
      | any isRight (M.elems oracles) = ""
      | otherwise = " [SKIPPED: no PHP interpreter found -- see docs/testing/php-oracle.md]"
    isRight = either (const False) (const True)

--------------------------------------------------------------------------------
-- Skipping
--------------------------------------------------------------------------------

-- | A skip is only honest if it is visible. Every group and property that can
-- be skipped carries the reason in its own name, so @All tests passed@ can never
-- be mistaken for evidence that an interpreter agreed with anything.
skipTag :: String -> String
skipTag why = " [SKIPPED: " ++ why ++ "]"

-- | Why this version's interpreter is unusable, if it is.
versionSkip :: Oracles -> PHPVersion -> Maybe String
versionSkip oracles v = case M.lookup v oracles of
  Just (Right _) -> Nothing
  Just (Left why) -> Just why
  Nothing -> Just ("PHP " ++ versionLabel v ++ " was never looked for")

-- | Run a property against one version's interpreter, or pass trivially when
-- that interpreter is missing. The name of the enclosing group already says so.
withOracle :: Testable prop => Oracles -> PHPVersion -> (PHPOracle -> prop) -> Property
withOracle oracles v k = case M.lookup v oracles of
  Just (Right o) -> property (k o)
  _ -> property ()

-- | Run a property that needs every supported version at once. \"No supported
-- version accepts this\" cannot be established from a subset, so a single
-- missing interpreter skips the property rather than weakening it.
withAllOracles :: Testable prop => Oracles -> ([(PHPVersion, PHPOracle)] -> prop) -> Property
withAllOracles oracles k = case traverse (either (const Nothing) Just) oracles of
  Just resolved -> property (k (M.toList resolved))
  Nothing -> property ()

-- | Run a property over whichever interpreters resolved. Sound only for claims
-- of the form \"every one of these rejects it\", which get stronger, not weaker,
-- as more interpreters appear.
withAnyOracles :: Testable prop => Oracles -> ([(PHPVersion, PHPOracle)] -> prop) -> Property
withAnyOracles oracles k = case resolvedOracles oracles of
  [] -> property ()
  resolved -> property (k resolved)

resolvedOracles :: Oracles -> [(PHPVersion, PHPOracle)]
resolvedOracles oracles = [(v, o) | (v, Right o) <- M.toList oracles]

-- | Tag for anything that needs at least one interpreter.
anySkip :: Oracles -> String
anySkip oracles
  | null (resolvedOracles oracles) = skipTag "no PHP interpreter found"
  | otherwise = ""

--------------------------------------------------------------------------------
-- The oracle handle
--------------------------------------------------------------------------------

handleTests :: Oracles -> [TestTree]
handleTests oracles =
  [ testCase "an interpreter is available for every supported version" $ do
      required <- oracleRequired
      let missing = [versionLabel v ++ " -- " ++ why | (v, Left why) <- M.toList oracles]
      case missing of
        [] -> pure ()
        _
          | required ->
              assertFailure . unlines $
                (oracleRequiredVar ++ " is set, but some interpreters are missing:") : map ("  " ++) missing
          | otherwise -> pure ()
  , testCase "a resolved interpreter reports the version it was resolved for" $
      sequence_
        [ assertEqual ("binary " ++ oracleBinary o) (T.pack (versionLabel v)) (oracleReportedVersion o)
        | (v, Right o) <- M.toList oracles
        ]
  , testCase "a binary reporting the wrong version is not used" $
      -- Discovery has already run, before the tree was built, so the override
      -- below cannot affect any other test.
      withFakePHP "9.9" $ \fake ->
        bracket_ (setEnv (binaryEnvVar PHP82) fake) (unsetEnv (binaryEnvVar PHP82)) $ do
          resolved <- resolveOracles
          case M.lookup PHP82 resolved of
            Just (Right o)
              | oracleBinary o == fake ->
                  assertFailure "a binary reporting 9.9 was accepted as the PHP 8.2 oracle"
            _ -> pure ()
  , testCase "an accepted program and a rejected one are told apart" $
      sequence_
        [ do
            good <- checkSource o "<?php echo 1;\n"
            assertEqual "valid source" Accepted good
            bad <- checkSource o "<?php $x = ;\n"
            assertBool ("invalid source was accepted: " ++ show bad) (not (verdictAccepted bad))
        | Right o <- M.elems oracles
        ]
  , testGroup "Diagnostic classification" classificationTests
  ]

-- | Captured verbatim from @php -l@'s stdout, so a change in PHP's wire format
-- fails here rather than silently turning every rejection into an unparsed blob.
classificationTests :: [TestTree]
classificationTests =
  [ testCase "a parse error" $
      assertEqual
        "parse error"
        (Just (Diagnostic ParseError "syntax error, unexpected token \";\"" 3))
        ( classifyDiagnostic
            "\nParse error: syntax error, unexpected token \";\" in Standard input code on line 3\nErrors parsing Standard input code\n"
        )
  , testCase "a compile-time fatal" $
      assertEqual
        "fatal"
        (Just (Diagnostic CompileFatal "Cannot use the static modifier on a parameter" 7))
        ( classifyDiagnostic
            "\nFatal error: Cannot use the static modifier on a parameter in Standard input code on line 7\nErrors parsing Standard input code\n"
        )
  , testCase "a diagnostic with no location keeps its whole text" $
      assertEqual
        "no location"
        (Just (Diagnostic ParseError "Unterminated comment starting line 2" 0))
        (classifyDiagnostic "\nParse error: Unterminated comment starting line 2\n")
  , testCase "output with no diagnostic is not invented" $
      assertBool "no diagnostic" (isNothing (classifyDiagnostic "No syntax errors detected\n"))
  , testCase "a cross-declaration rejection is out of contract" $
      assertBool "duplicate import" $
        outOfContract (Diagnostic CompileFatal "Cannot use App\\Str as Str because the name is already in use" 4)
          /= Nothing
  , testCase "a declaration-local rejection is in contract" $
      assertEqual
        "duplicate class constant"
        Nothing
        (outOfContract (Diagnostic CompileFatal "Cannot redefine class constant Base::VERSION" 9))
  ]

-- | A stand-in @php@ that reports a version no oracle wants.
withFakePHP :: String -> (FilePath -> IO a) -> IO a
withFakePHP reported k = do
  tmp <- getTemporaryDirectory
  -- Plain concatenation rather than a new @filepath@ dependency: the suite
  -- only ever builds this one path, and only on a POSIX temporary directory.
  let dir = tmp ++ "/php-parser-fake-oracle"
      bin = dir ++ "/php"
  bracket_ (install dir bin) (removePathForcibly dir) (k bin)
  where
    install dir bin = do
      createDirectoryIfMissing True dir
      writeFile bin ("#!/bin/sh\nprintf '%s' '" ++ reported ++ "'\n")
      setPermissions bin (setOwnerExecutable True (setOwnerReadable True emptyPermissions))

--------------------------------------------------------------------------------
-- The mutation layer itself
--------------------------------------------------------------------------------

-- | The mutations are derived from the feature catalogue, so catalogue drift can
-- silently stop producing near-misses. These run with no interpreter present.
mutationLayerTests :: [TestTree]
mutationLayerTests =
  [ testCase "every mutation has a distinct name" $
      let names = map mutationName allMutations
       in assertEqual "names" (sort names) (sort (nub names))
  , testCase "every mutation names a real catalogue feature" $
      sequence_
        [ assertBool (mutationName m ++ " targets unknown feature " ++ mutationFeature m) $
            mutationFeature m `elem` map featureName allFeatures
        | m <- allMutations
        ]
  , testProperty "every mutation still finds its anchor" $
      forAll (elements allVersions) $ \v ->
        forAll (elements (mutationsUpTo v)) $ \m ->
          forAll (genMutationOf v m) $ \mp ->
            counterexample
              (mutationName m ++ " no longer matches the text of feature " ++ mutationFeature m)
              (mutatedApplied mp)
  , testProperty "mutating changes the program" $
      forAll (elements allVersions) $ \v ->
        forAll (genMutatedProgram v) $ \mp ->
          counterexample (show mp) $
            T.length (renderMutated mp) > 0
              && length (programSnippets (mutatedProgram mp)) >= 1
  ]

--------------------------------------------------------------------------------
-- Differential properties
--------------------------------------------------------------------------------

-- | Properties 1, 2 and 4: everything that draws from the valid corpus and can
-- therefore be pinned to a single version.
versionProperties :: Oracles -> PHPVersion -> TestTree
versionProperties oracles v =
  testGroup
    ("PHP " ++ versionLabel v ++ maybe "" skipTag (versionSkip oracles v))
    [ testProperty "corpus health: the interpreter accepts every generated program" $
        withOracle oracles v $ \o ->
          forAllProgram v $ \prog src ->
            acceptsOrReports o prog src " rejected a supposedly valid program"
    , testProperty "no false rejects: what the interpreter accepts, the library parses" $
        withOracle oracles v $ \o ->
          forAllProgram v $ \prog src ->
            ioProperty $ do
              verdict <- checkSource o src
              pure $ case (verdict, parseProgram "gen.php" src) of
                (Accepted, Left err) ->
                  counterexample
                    ( unlines
                        [ "PHP " ++ versionLabel v ++ " accepted this program but the library did not"
                        , T.unpack (formatParseError err)
                        , "--- source ---"
                        , T.unpack src
                        , "--- program ---"
                        , show prog
                        ]
                    )
                    False
                _ -> property True
    , testProperty "the printer emits source the interpreter still accepts" $
        withOracle oracles v $ \o ->
          forAllProgram v $ \prog src ->
            case parseProgram "gen.php" src of
              Left _ -> property () -- covered, and reported, by the property above
              Right ast ->
                acceptsOrReports o prog (prettyPrint ast) " rejected the printer's output"
    ]
  where
    -- Both the corpus-health and the printer property are the same claim about
    -- a different piece of text: this interpreter accepts it, and if it does
    -- not, here is the program that produced it.
    acceptsOrReports o prog checked headline = ioProperty $ do
      verdict <- checkSource o checked
      pure $ case verdict of
        Accepted -> property True
        Rejected d ->
          counterexample
            (reportSource prog checked ("PHP " ++ versionLabel v ++ headline) d)
            False

-- | Property 3. Version-agnostic, because the library parses the union of every
-- supported grammar: it is only wrong to accept something /no/ supported
-- version accepts.
--
-- Runs only with all four interpreters present, since "no version accepts this"
-- cannot be established from a subset.
--
-- The @Caught@ guard suppresses exactly the 'KnownFalseAccept' pins, which
-- 'divergenceTable' asserts positively, so a fixed bug still turns the suite
-- red. The gap it leaves is narrow and worth naming: a /new/ false accept that
-- only shows up in a program built from a pinned mutation is masked, because
-- the whole program is excused rather than the pinned construct.
noFalseAccepts :: Oracles -> TestTree
noFalseAccepts oracles =
  testProperty
    ( "no false accepts: the library parses only what some supported version accepts"
        ++ if length (resolvedOracles oracles) == length allVersions
          then ""
          else skipTag "needs all four interpreters; \"no version accepts this\" cannot be checked from a subset"
    )
    $ withAllOracles oracles
    $ \resolved ->
      forAll (elements allVersions) $ \v ->
        forAllShrink (genMutatedProgram v) shrinkMutatedProgram $ \mp ->
          let src = renderMutated mp
           in ioProperty $ do
                verdicts <- traverse (\(w, o) -> (,) w <$> checkSource o src) resolved
                pure $ case (any (verdictAccepted . snd) verdicts, parseProgram "mutated.php" src) of
                  (False, Right _)
                    | Caught <- mutationStance (mutatedMutation mp) ->
                        counterexample (rejectionReport mp verdicts) False
                  _ -> property True

-- | The two-sided pin. Every mutation is checked against both the interpreters
-- and the library, and its recorded stance must hold exactly: a 'Caught'
-- mutation that stops being caught fails, and a 'KnownFalseAccept' that starts
-- being caught fails too -- the second is a bug fix, and this is where it is
-- noticed.
divergenceTable :: Oracles -> TestTree
divergenceTable oracles =
  testGroup
    ("Known divergences: catalogue mutations" ++ anySkip oracles)
    [ testProperty (mutationName m) $
        withAnyOracles oracles $ \resolved ->
          -- The context is generated for the same version as the interpreter
          -- that judges it, so the only thing the interpreter can be objecting
          -- to is the mutation.
          forAllShow (elements resolved) (("PHP " ++) . versionLabel . fst) $ \(v, o) ->
            forAll (genMutationOf v m) $ \mp ->
              let src = renderMutated mp
               in ioProperty $ do
                    verdict <- checkSource o src
                    let verdicts = [(v, verdict)]
                    pure $
                      conjoin
                        [ counterexample ("the mutation no longer applies\n" ++ show mp) (mutatedApplied mp)
                        , counterexample
                            ("the interpreter accepted a near-miss\n" ++ rejectionReport mp verdicts)
                            (not (verdictAccepted verdict))
                        , counterexample
                            ("the rejection is out of contract, so this mutation proves nothing\n" ++ rejectionReport mp verdicts)
                            (inContract verdict)
                        , stanceHolds mp src
                        ]
    | m <- allMutations
    ]
  where
    inContract (Rejected d) = isNothing (outOfContract d)
    inContract Accepted = True

    stanceHolds mp src =
      let parsed = either (const False) (const True) (parseProgram "mutated.php" src)
       in case mutationStance (mutatedMutation mp) of
            Caught ->
              counterexample
                ("recorded as caught, but the library accepted it\n" ++ show mp)
                (not parsed)
            KnownFalseAccept why ->
              counterexample
                ( unlines
                    [ "recorded as a known false accept -- " ++ why
                    , "but the library now rejects it. If that is a fix, change the"
                    , "mutation's stance to Caught in Test.Gen.PHPMutation."
                    , show mp
                    ]
                )
                parsed

--------------------------------------------------------------------------------
-- Reporting
--------------------------------------------------------------------------------

reportSource :: PHPProgram -> T.Text -> String -> Diagnostic -> String
reportSource prog src headline d =
  unlines
    [ headline
    , show (diagKind d) ++ " on line " ++ show (diagLine d) ++ ": " ++ T.unpack (diagMessage d)
    , maybe "" ("out of contract: " ++) (outOfContract d)
    , "--- source ---"
    , T.unpack src
    , "--- program ---"
    , show prog
    ]

rejectionReport :: MutatedProgram -> [(PHPVersion, Verdict)] -> String
rejectionReport mp verdicts =
  unlines $
    ("expected rule: " ++ mutationPHPRule (mutatedMutation mp))
      : [ "  PHP " ++ versionLabel v ++ ": " ++ describe verdict
        | (v, verdict) <- verdicts
        ]
      ++ [show mp]
  where
    describe Accepted = "accepted"
    describe (Rejected d) = show (diagKind d) ++ " -- " ++ T.unpack (diagMessage d)

-- | Same shape as "Test.CompatibilitySpec"'s helper, kept local so the two
-- suites can diverge without one breaking the other.
forAllProgram :: Testable prop => PHPVersion -> (PHPProgram -> T.Text -> prop) -> Property
forAllProgram v k =
  forAllShrink (genProgram v) shrinkProgram $ \prog ->
    tabulate "features exercised" (map snippetFeature (programSnippets prog)) $
      k prog (renderProgram prog)

--------------------------------------------------------------------------------
-- The known-divergence table
--------------------------------------------------------------------------------

-- | The constructs "Test.Gen.PHPSource" excludes from the valid corpus, checked
-- against both sides.
--
-- The table in 'knownDivergences' records, per construct, the issue it belongs
-- to, PHP's decision, the library's decision and whether a lint oracle can see
-- the difference at all. Here both halves are checked: the library's decision
-- needs no interpreter and always runs; PHP's needs one and says so in the test
-- name when it is missing.
--
-- Entries marked 'ByVerdict' are gates. When such a divergence is fixed, the
-- library's decision stops matching what is recorded and this test fails --
-- which is the point, because the fix must update the table and remove the
-- generator's exclusion in the same change.
--
-- Entries marked 'NotByVerdict' are records, not gates, and their names say so:
-- both sides accept the program, so no exit status can distinguish them. They
-- are listed because a table that silently omitted them would read as if this
-- oracle covered all six.
knownDivergenceTests :: Oracles -> TestTree
knownDivergenceTests oracles =
  testGroup
    "Known divergences: constructs excluded from the generator"
    [ testCase (divergenceTestName oracles d) $ do
        assertEqual
          ("the library's decision on #" ++ show (divergenceIssue d) ++ " changed")
          (divergenceLibrary d)
          (libraryDecision (divergenceSource d))
        sequence_
          [ do
              verdict <- checkSource o (divergenceSource d)
              assertEqual
                ("PHP " ++ versionLabel v ++ "'s decision on #" ++ show (divergenceIssue d) ++ " changed")
                (divergencePHP d)
                (decisionOf verdict)
          | (v, o) <- resolvedOracles oracles
          ]
    | d <- knownDivergences
    ]
  where
    libraryDecision src = either (const Rejects) (const Accepts) (parseProgram "divergence.php" src)
    decisionOf v = if verdictAccepted v then Accepts else Rejects

divergenceTestName :: Oracles -> KnownDivergence -> String
divergenceTestName oracles d =
  "#"
    ++ show (divergenceIssue d)
    ++ " "
    ++ divergenceName d
    ++ detect
    ++ phpHalf
  where
    detect = case divergenceDetect d of
      ByVerdict -> " [gated by verdict]"
      NotByVerdict _ -> " [recorded only: no exit status can see this]"
    phpHalf
      | null (resolvedOracles oracles) = skipTag "the PHP half needs an interpreter; the library half still runs"
      | otherwise = ""
