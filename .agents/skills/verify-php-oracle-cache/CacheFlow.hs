{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import SourceCheck
import System.Environment (getEnv, setEnv, unsetEnv)
import Test.Gen.PHPSource (PHPVersion (PHP84))
import Test.Oracle.PHP

resolveFake :: IO PHPOracle
resolveFake = do
  oracles <- resolveOracles
  case Map.lookup PHP84 oracles of
    Just (Right oracle) -> pure oracle
    Just (Left why) -> fail why
    Nothing -> fail "PHP 8.4 slot missing"

logLines :: FilePath -> IO [Text]
logLines path = T.lines <$> T.readFile path

main :: IO ()
main = do
  emptyPath <- getEnv "FAKE_ORACLE_PATH"
  logPath <- getEnv "FAKE_ORACLE_LOG"
  setEnv "PATH" emptyPath
  mapM_ unsetEnv ["PHP82_BIN", "PHP83_BIN", "PHP85_BIN"]
  first <- resolveFake
  good1 <- checkSource first "good"
  good2 <- checkSource first "good"
  bad1 <- checkSource first "bad"
  bad2 <- checkSource first "bad"
  other <- checkSource first "good "
  sequentialLog <- logLines logPath
  second <- resolveFake
  newHandle <- checkSource second "good"
  handleLog <- logLines logPath
  let keys = ["parallel-" <> T.pack (show n) | n <- [(1 :: Int)..8]]
  results <- forM keys $ \key -> do
    done <- newEmptyMVar
    _ <- forkFinally (checkSource first key) (putMVar done)
    pure done
  concurrent <- mapM takeMVar results
  beforeReplay <- logLines logPath
  replay <- mapM (checkSource first) keys
  afterReplay <- logLines logPath
  let expectedBad = Rejected (Diagnostic ParseError "bad input" 2)
  runChecks
    [ check "accepted verdict is cached" (good1 == Accepted && good2 == Accepted) (show (good1, good2))
    , check "rejected verdict and diagnostic are cached" (bad1 == expectedBad && bad2 == expectedBad) (show (bad1, bad2))
    , check "source text is the key" (other == Accepted && sequentialLog == ["good", "bad", "good "]) (show sequentialLog)
    , check "new handle has a separate cache" (newHandle == Accepted && handleLog == sequentialLog ++ ["good"]) (show handleLog)
    , check "concurrent distinct misses all succeed" (all (either (const False) (== Accepted)) concurrent) (show concurrent)
    , check "concurrent entries survive and replay without lint" (length beforeReplay == length handleLog + length keys && afterReplay == beforeReplay && all (== Accepted) replay) (show (beforeReplay, afterReplay))
    ]
