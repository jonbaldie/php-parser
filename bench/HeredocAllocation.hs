{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Control.Monad (unless)
import qualified Data.Text as T
import Language.PHP
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [kind, count] | kind `elem` ["heredoc", "nowdoc"], Just n <- readMaybe count, n > 0 -> do
      let prefix = if kind == "nowdoc" then "<<<'END'\n" else "<<<END\n"
          source = prefix <> T.replicate n "line\n" <> "END"
          expected = T.intercalate "\n" (replicate n "line")
      case parseExpression "bench.php" source of
        Right (ExprLit _ (LitHeredoc _ _ value _ _)) -> do
          correct <- evaluate (value == expected)
          unless correct (fail "Wrong literal content")
          print (T.length value)
        result -> fail (show result)
    _ -> fail "Usage: HeredocAllocation (heredoc|nowdoc) LINE_COUNT +RTS -s"
