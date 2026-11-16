{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (forM)
import Language.PHP
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.FoldQueryCases (foldQueryCases)

main :: IO ()
main = do
  filters <- getArgs
  let cases = filter (\(label, _, _) -> all (`isInfixOf` label) filters) foldQueryCases
  results <- forM cases $ \(label, source, expected) ->
    case parseExpression "history.php" source of
      Left err -> print err >> pure False
      Right expr -> do
        let variable = \case
              ExprVar _ (SimpleVar _ (VarName _ name)) -> [name]
              _ -> []
            actual = [allVariables expr, foldExpr variable expr, queryExpr variable expr]
            passed = all (== expected) actual
        putStrLn ((if passed then "PASS " else "FAIL ") ++ label ++ ": " ++ show actual)
        pure passed
  if not (null results) && and results then pure () else exitFailure
