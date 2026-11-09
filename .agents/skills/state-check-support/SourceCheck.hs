{-# LANGUAGE OverloadedStrings #-}

module SourceCheck (Check (..), check, program, expression, runChecks) where

import Control.Monad (forM, unless, when)
import Data.Either (isRight)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Language.PHP (formatParseError, parseExpression, parseProgram)
import System.Exit (exitFailure)

data Check = Check String (IO (Maybe String))

check :: String -> Bool -> String -> Check
check name passed detail = Check name (pure (if passed then Nothing else Just detail))

program :: String -> Bool -> Text -> Check
program name expected source = Check name $ pure $
  let result = parseProgram "state-check.php" source
      actual = isRight result
      detail = either (show . formatParseError) (const "accepted") result
  in if actual == expected then Nothing else Just
       ("expected " ++ decision expected ++ ", got " ++ detail ++ "\n  source: " ++ T.unpack source)

expression :: String -> Bool -> Text -> Check
expression name expected source = Check name $ pure $
  let result = parseExpression "state-check.php" source
      actual = isRight result
      detail = either (show . formatParseError) (const "accepted") result
  in if actual == expected then Nothing else Just
       ("expected " ++ decision expected ++ ", got " ++ detail ++ "\n  source: " ++ T.unpack source)

decision :: Bool -> String
decision True = "acceptance"
decision False = "rejection"

runChecks :: [Check] -> IO ()
runChecks checks = do
  when (null checks) $ do
    putStrLn "COVERAGE FAILURE: no checks selected"
    exitFailure
  failures <- fmap catMaybes $ forM checks $ \(Check name action) -> do
    result <- action
    pure (fmap (\detail -> (name, detail)) result)
  mapM_ (\(name, detail) -> putStrLn ("FAIL " ++ name ++ "\n  " ++ detail)) failures
  putStrLn ("Checked " ++ show (length checks) ++ " cases; failures " ++ show (length failures) ++ ".")
  unless (null failures) exitFailure
