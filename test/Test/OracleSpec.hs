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
import Data.List (sort)
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

-- | Each property costs one subprocess per generated program, so the floor is
-- lower than the hermetic suite's. @--quickcheck-tests=N@ still wins when larger.
oracleTests :: TestTree
oracleTests =
  adjustOption (max (QuickCheckTests 50)) $
    withResource resolveOracles (const (pure ())) $ \getOracles ->
      testGroup
        "PHP interpreter oracle (differential)"
        [ testGroup "Oracle handle" (handleTests getOracles)
        , testGroup "Mutation layer" mutationLayerTests
        , testGroup
            "Differential properties"
            ( map (versionProperties getOracles) allVersions
                ++ [noFalseAccepts getOracles, divergenceTable getOracles]
            )
        ]

--------------------------------------------------------------------------------
-- Skipping
--------------------------------------------------------------------------------

-- | Run a property against one version's interpreter, or pass trivially when
-- that interpreter is missing. Absence is reported once, by
-- @an interpreter is available for every supported version@, rather than by
-- every property failing for the same reason.
withOracle :: Testable prop => IO Oracles -> PHPVersion -> (PHPOracle -> prop) -> Property
withOracle getOracles v k = ioProperty $ do
  oracles <- getOracles
  pure $ case M.lookup v oracles of
    Just (Right o) -> property (k o)
    _ -> property ()

-- | Run a property that needs every supported version at once. \"No supported
-- version accepts this\" cannot be established from a subset, so a single
-- missing interpreter skips the property rather than weakening it.
withAllOracles :: Testable prop => IO Oracles -> ([(PHPVersion, PHPOracle)] -> prop) -> Property
withAllOracles getOracles k = ioProperty $ do
  oracles <- getOracles
  pure $ case traverse (either (const Nothing) Just) oracles of
    Just resolved -> property (k (M.toList resolved))
    Nothing -> property ()

-- | Run a property over whichever interpreters resolved. Sound only for claims
-- of the form \"every one of these rejects it\", which get stronger, not weaker,
-- as more interpreters appear.
withAnyOracles :: Testable prop => IO Oracles -> ([(PHPVersion, PHPOracle)] -> prop) -> Property
withAnyOracles getOracles k = ioProperty $ do
  oracles <- getOracles
  pure $ case [(v, o) | (v, Right o) <- M.toList oracles] of
    [] -> property ()
    resolved -> property (k resolved)

--------------------------------------------------------------------------------
-- The oracle handle
--------------------------------------------------------------------------------

handleTests :: IO Oracles -> [TestTree]
handleTests getOracles =
  [ testCase "an interpreter is available for every supported version" $ do
      oracles <- getOracles
      required <- oracleRequired
      let missing = [versionLabel v ++ " -- " ++ why | (v, Left why) <- M.toList oracles]
      case missing of
        [] -> pure ()
        _
          | required ->
              assertFailure . unlines $
                (oracleRequiredVar ++ " is set, but some interpreters are missing:") : map ("  " ++) missing
          | otherwise -> pure ()
  , testCase "a resolved interpreter reports the version it was resolved for" $ do
      oracles <- getOracles
      sequence_
        [ assertEqual ("binary " ++ oracleBinary o) (T.pack (versionLabel v)) (oracleReportedVersion o)
        | (v, Right o) <- M.toList oracles
        ]
  , testCase "a binary reporting the wrong version is not used" $ do
      -- Forces the shared resource to have been resolved already, so setting
      -- the override below cannot affect any other test.
      _ <- getOracles
      withFakePHP "9.9" $ \fake ->
        bracket_ (setEnv (binaryEnvVar PHP82) fake) (unsetEnv (binaryEnvVar PHP82)) $ do
          resolved <- resolveOracles
          case M.lookup PHP82 resolved of
            Just (Right o)
              | oracleBinary o == fake ->
                  assertFailure "a binary reporting 9.9 was accepted as the PHP 8.2 oracle"
            _ -> pure ()
  , testCase "an accepted program and a rejected one are told apart" $ do
      oracles <- getOracles
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
       in assertEqual "names" (sort names) (sort (dedupe (sort names)))
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
  where
    dedupe (x : y : rest) | x == y = dedupe (y : rest)
    dedupe (x : rest) = x : dedupe rest
    dedupe [] = []

--------------------------------------------------------------------------------
-- Differential properties
--------------------------------------------------------------------------------

-- | Properties 1, 2 and 4: everything that draws from the valid corpus and can
-- therefore be pinned to a single version.
versionProperties :: IO Oracles -> PHPVersion -> TestTree
versionProperties getOracles v =
  testGroup
    ("PHP " ++ versionLabel v)
    [ testProperty "corpus health: the interpreter accepts every generated program" $
        withOracle getOracles v $ \o ->
          forAllProgram v $ \prog src ->
            ioProperty $ do
              verdict <- checkSource o src
              pure $ case verdict of
                Accepted -> property True
                Rejected d ->
                  counterexample
                    (reportSource prog src ("PHP " ++ versionLabel v ++ " rejected a supposedly valid program") d)
                    False
    , testProperty "no false rejects: what the interpreter accepts, the library parses" $
        withOracle getOracles v $ \o ->
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
        withOracle getOracles v $ \o ->
          forAllProgram v $ \prog src ->
            case parseProgram "gen.php" src of
              Left _ -> property () -- covered, and reported, by the property above
              Right ast -> ioProperty $ do
                let printed = prettyPrint ast
                verdict <- checkSource o printed
                pure $ case verdict of
                  Accepted -> property True
                  Rejected d ->
                    counterexample
                      (reportSource prog printed "the printer produced source PHP rejects" d)
                      False
    ]

-- | Property 3. Version-agnostic, because the library parses the union of every
-- supported grammar: it is only wrong to accept something /no/ supported
-- version accepts.
--
-- Runs only with all four interpreters present, since "no version accepts this"
-- cannot be established from a subset.
noFalseAccepts :: IO Oracles -> TestTree
noFalseAccepts getOracles =
  testProperty "no false accepts: the library parses only what some supported version accepts" $
    withAllOracles getOracles $ \oracles ->
      forAll (elements allVersions) $ \v ->
        forAllShrink (genMutatedProgram v) shrinkMutatedProgram $ \mp ->
          let src = renderMutated mp
           in ioProperty $ do
                verdicts <- traverse (\(w, o) -> (,) w <$> checkSource o src) oracles
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
divergenceTable :: IO Oracles -> TestTree
divergenceTable getOracles =
  testGroup
    "Known divergences"
    [ testProperty (mutationName m) $
        withAnyOracles getOracles $ \oracles ->
          -- The context is generated for the same version as the interpreter
          -- that judges it, so the only thing the interpreter can be objecting
          -- to is the mutation.
          forAllShow (elements oracles) (("PHP " ++) . versionLabel . fst) $ \(v, o) ->
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
