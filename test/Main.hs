module Main (main) where

import Test.Tasty
import Test.PHP82Spec (php82Tests)
import Test.PHP83Spec (php83Tests)
import Test.PHP84Spec (php84Tests)
import Test.PHP85Spec (php85Tests)
import Test.ExpressionSpec (expressionTests)
import Test.StatementSpec (statementTests)
import Test.PrettySpec (prettyTests)
import Test.RoundTripSpec (roundTripTests)
import Test.RecursionSchemesSpec (recursionSchemesTests)
import Test.CompatibilitySpec (compatibilityTests)
import Test.OracleSpec (oracleTests)

main :: IO ()
main = do
  -- The oracle group discovers its interpreters before the tree is built, so a
  -- missing one can be named in the test names rather than passing silently.
  oracle <- oracleTests
  defaultMain (tests oracle)

tests :: TestTree -> TestTree
tests oracle = testGroup "PHP Parser Test Suite"
  [ php82Tests
  , php83Tests
  , php84Tests
  , php85Tests
  , expressionTests
  , statementTests
  , prettyTests
  , roundTripTests
  , recursionSchemesTests
  , compatibilityTests
  , oracle
  ]
