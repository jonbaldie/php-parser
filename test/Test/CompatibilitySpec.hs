{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.CompatibilitySpec
-- Description : High-level QuickCheck properties for PHP version compatibility
--
-- One property group per PHP version the library claims to support. Each group
-- draws random programs from that version's grammar (see "Test.Gen.PHPSource")
-- and asserts the three promises the README makes about them:
--
-- 1. the program parses;
-- 2. printing it and reparsing yields an equal AST ("round-trip invariance");
-- 3. printing is stable — a second pass changes nothing.
--
-- Unlike the example-based @Test.PHP8xSpec@ modules, these properties say
-- something about the whole version-indexed grammar rather than about one
-- construct, and they shrink a failure down to the smallest set of features
-- that still reproduces it.
--
-- The generators are hermetic: they do not shell out to a PHP runtime, so the
-- oracle here is the library's own documented invariants, not PHP itself.
module Test.CompatibilitySpec (compatibilityTests) where

import qualified Data.Text as T
import Language.PHP
import Test.Gen.PHPSource
import Test.Tasty
import Test.Tasty.QuickCheck

compatibilityTests :: TestTree
compatibilityTests =
  -- A floor, not a ceiling: @--quickcheck-tests=N@ still wins when N is larger,
  -- so the same properties can be run as a deep fuzz without editing this file.
  adjustOption (max (QuickCheckTests 200)) $
    testGroup
      "PHP Version Compatibility (property-based)"
      (map versionCompatibility allVersions ++ [catalogueInvariants])

-- | The three compatibility promises, checked over random programs drawn from
-- one version's grammar.
versionCompatibility :: PHPVersion -> TestTree
versionCompatibility v =
  testGroup
    ("PHP " ++ versionLabel v ++ " compatibility")
    [ testProperty "every program in the grammar parses" $
        forAllProgram v $ \prog src ->
          case parseProgram "gen.php" src of
            Left err -> failedWith prog "parse failed" (T.unpack (formatParseError err))
            Right _ -> property True
    , testProperty "printing and reparsing preserves the AST" $
        forAllProgram v $ \prog src ->
          withParsed prog src $ \ast ->
            let printed = prettyPrint ast
             in case parseProgram "printed.php" printed of
                  Left err ->
                    failedWith prog "printed output does not reparse" $
                      T.unpack (formatParseError err) ++ "\n--- printed ---\n" ++ T.unpack printed
                  Right ast' ->
                    counterexample
                      (report prog "round-trip changed the AST" ("--- printed ---\n" ++ T.unpack printed))
                      (stripAnnotations ast == stripAnnotations ast')
    , testProperty "printing reaches a fixed point after one pass" $
        forAllProgram v $ \prog src ->
          withParsed prog src $ \ast ->
            let printed = prettyPrint ast
             in case parseProgram "printed.php" printed of
                  Left err ->
                    failedWith prog "printed output does not reparse" $
                      T.unpack (formatParseError err) ++ "\n--- printed ---\n" ++ T.unpack printed
                  Right ast' ->
                    let printed' = prettyPrint ast'
                     in counterexample
                          ( report prog "second printing pass differs" $
                              "--- first pass ---\n"
                                ++ T.unpack printed
                                ++ "\n--- second pass ---\n"
                                ++ T.unpack printed'
                          )
                          (printed == printed')
    ]

-- | Invariants of the feature catalogue itself, so a mistagged feature cannot
-- silently weaken a version's property.
catalogueInvariants :: TestTree
catalogueInvariants =
  testGroup
    "Feature catalogue"
    [ testProperty "feature sets grow monotonically with the version" $
        forAll (elements [(a, b) | a <- allVersions, b <- allVersions, a <= b]) $
          \(older, newer) ->
            let names = map featureName . featuresUpTo
             in counterexample
                  ("PHP " ++ versionLabel older ++ " features missing from PHP " ++ versionLabel newer)
                  (all (`elem` names newer) (names older))
    , testProperty "every version introduces at least one feature" $
        forAll (elements allVersions) $ \v ->
          counterexample
            ("no feature is tagged as introduced in PHP " ++ versionLabel v)
            (not (null (featuresIntroducedIn v)))
    , testProperty "a program never uses syntax newer than its version" $
        forAll (elements allVersions) $ \v ->
          forAllProgram v $ \prog _ ->
            counterexample "snippet tagged newer than the program's version" $
              all ((<= programVersion prog) . snippetSince) (programSnippets prog)
    ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Generate, shrink, and record which features a test case exercised.
-- @tabulate@ makes the coverage visible with @--quickcheck-verbose@ and keeps a
-- silently-narrowed generator from passing unnoticed.
forAllProgram :: Testable prop => PHPVersion -> (PHPProgram -> T.Text -> prop) -> Property
forAllProgram v k =
  forAllShrink (genProgram v) shrinkProgram $ \prog ->
    tabulate "features exercised" (map snippetFeature (programSnippets prog)) $
      k prog (renderProgram prog)

-- | Run the continuation on a successfully parsed program; a failure to parse
-- at all is reported here too rather than discarded, so no property can pass
-- vacuously.
withParsed :: Testable prop => PHPProgram -> T.Text -> (Program (Annotated Span) -> prop) -> Property
withParsed prog src k = case parseProgram "gen.php" src of
  Left err -> failedWith prog "parse failed" (T.unpack (formatParseError err))
  Right ast -> property (k ast)

failedWith :: PHPProgram -> String -> String -> Property
failedWith prog headline detail = counterexample (report prog headline detail) False

report :: PHPProgram -> String -> String -> String
report prog headline detail =
  unlines
    [ headline
    , detail
    , "--- program ---"
    , show prog
    ]
