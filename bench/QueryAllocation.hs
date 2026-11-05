{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (foldl')
import GHC.Stats (allocated_bytes, getRTSStats, getRTSStatsEnabled)
import Language.PHP
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  enabled <- getRTSStatsEnabled
  unless enabled (fail "Run with +RTS -T -RTS")
  case args of
    [kind, sizeText] | Just size <- readMaybe sizeText, size > 0 -> run kind size
    _ -> fail "Usage: QueryAllocation (variables|expressions) SIZE +RTS -T -RTS"

run :: String -> Int -> IO ()
run kind size = do
  let variable = ExprVar () (SimpleVar () (VarName () "x"))
      tree = foldl' (\left _ -> ExprBinary () OpAdd left variable) variable [2 .. size]
      (result, expected) = case kind of
        "variables" -> (length (allVariables tree), size)
        "expressions" -> (length (allExprs tree), 2 * size - 1)
        _ -> error "Expected variables or expressions"
  _ <- evaluate tree
  performGC
  before <- allocated_bytes <$> getRTSStats
  !count <- evaluate result
  unless (count == expected) (fail ("Wrong result count: " ++ show count))
  performGC
  after <- allocated_bytes <$> getRTSStats
  putStrLn (unwords [kind, show size, show count, show (after - before)])
