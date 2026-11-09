{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (foldM, forM_, unless, when)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.PHP
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode (ExitFailure))

data Case = Case
  { caseGroup :: String
  , caseName :: String
  , caseSource :: Text
  }

data Options = Options
  { selectedGroup :: Maybe String
  , allFailures :: Bool
  , showRejections :: Bool
  , allRejections :: Bool
  }

data Counts = Counts
  { accepted :: !Int
  , rejected :: !Int
  , failed :: !Int
  }

data Failure = Failure
  { failureCase :: Case
  , failurePrinted :: Text
  , failureDetail :: String
  }

data Results = Results
  { counts :: !(Map.Map String Counts)
  , examples :: !(Map.Map String Failure)
  , rejectionExamples :: !(Map.Map String (Case, String))
  }

emptyResults :: Results
emptyResults = Results Map.empty Map.empty Map.empty

parseOptions :: [String] -> Maybe Options
parseOptions = go (Options Nothing False False False)
  where
    go options [] = Just options
    go options ("--all-failures" : rest) = go options { allFailures = True } rest
    go options ("--show-rejections" : rest) = go options { showRejections = True } rest
    go options ("--all-rejections" : rest) = go options { allRejections = True } rest
    go options ("--group" : group : rest)
      | selectedGroup options == Nothing = go options { selectedGroup = Just group } rest
    go _ _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  options <- case parseOptions args of
    Nothing -> do
      putStrLn "Usage: check.sh [--group NAME] [--all-failures] [--show-rejections] [--all-rejections]"
      exitWith (ExitFailure 2)
    Just parsed -> pure parsed
  let groups = maybe requiredGroups pure (selectedGroup options)
      selected = maybe cases (\group -> filter ((== group) . caseGroup) cases) (selectedGroup options)
  when (null selected) $ do
    putStrLn "COVERAGE FAILURE: the selected group has no generated cases."
    exitWith (ExitFailure 2)
  forM_ (selectedGroup options) $ \group -> putStrLn ("Selected group: " ++ group)
  results <- foldM (checkCase (allFailures options) (allRejections options)) emptyResults selected
  let allCounts = Map.elems (counts results)
      totalAccepted = sum (map accepted allCounts)
      totalRejected = sum (map rejected allCounts)
      totalFailed = sum (map failed allCounts)
      missing = [ group | group <- groups
                        , maybe True ((== 0) . accepted) (Map.lookup group (counts results)) ]
  putStrLn $ "Generated " ++ show (totalAccepted + totalRejected) ++ " source cases."
  putStrLn $ "Accepted " ++ show totalAccepted ++ "; rejected " ++ show totalRejected
          ++ "; round-trip failures " ++ show totalFailed ++ "."
  when (showRejections options) $
    forM_ (Map.toAscList (rejectionExamples results)) $ \(group, (testCase, problem)) -> do
      let count = maybe 0 rejected (Map.lookup group (counts results))
      putStrLn $ "\n" ++ group ++ ": " ++ show count ++ " rejected cases; one example follows."
      printRejection testCase problem
  unless (allFailures options) $
    forM_ (Map.toAscList (examples results)) $ \(group, failure) -> do
      let count = maybe 0 failed (Map.lookup group (counts results))
      putStrLn $ "\n" ++ group ++ ": " ++ show count ++ " failing cases; one example follows."
      printFailure failure
  unless (null missing) $ do
    putStrLn $ "\nCOVERAGE FAILURE: no accepted input for " ++ intercalate ", " missing
    exitWith (ExitFailure 2)
  if totalFailed == 0
    then putStrLn "PASS: every accepted source preserved its expression AST."
    else exitFailure

checkCase :: Bool -> Bool -> Results -> Case -> IO Results
checkCase showAll showAllRejected results testCase =
  case parseExpression "input.php" (caseSource testCase) of
    Left err -> do
      let problem = show (formatParseError err)
      when showAllRejected (printRejection testCase problem)
      pure results
        { counts = record (\c -> c { rejected = rejected c + 1 })
        , rejectionExamples = Map.insertWith (\_ old -> old) group
            (testCase, problem) (rejectionExamples results)
        }
    Right original -> do
      let ast = stripAnnotations original
          printed = prettyPrintExpr ast
          problem = case parseExpression "printed.php" printed of
            Left err -> Just ("printed source was rejected: " ++ show (formatParseError err))
            Right reparsed
              | ast == stripAnnotations reparsed -> Nothing
              | otherwise -> Just ("AST changed\n  expected: " ++ show ast
                                  ++ "\n  actual:   " ++ show (stripAnnotations reparsed))
      case problem of
        Nothing -> pure results { counts = record (\c -> c { accepted = accepted c + 1 }) }
        Just detail -> do
          let failure = Failure testCase printed detail
          when showAll (printFailure failure)
          pure results
            { counts = record (\c -> c { accepted = accepted c + 1, failed = failed c + 1 })
            , examples = Map.insertWith (\_ old -> old) group failure (examples results)
            }
  where
    group = caseGroup testCase
    record f = Map.alter (Just . f . maybe (Counts 0 0 0) id) group (counts results)

printRejection :: Case -> String -> IO ()
printRejection testCase problem = do
  putStrLn $ "REJECTED " ++ caseGroup testCase ++ " / " ++ caseName testCase
  putStrLn $ "  source: " ++ show (caseSource testCase)
  putStrLn $ "  error:  " ++ problem

printFailure :: Failure -> IO ()
printFailure failure = do
  let testCase = failureCase failure
  putStrLn $ "FAIL " ++ caseGroup testCase ++ " / " ++ caseName testCase
  putStrLn $ "  input:   " ++ show (caseSource testCase)
  putStrLn $ "  printed: " ++ show (failurePrinted failure)
  putStrLn $ "  " ++ failureDetail failure

par :: Text -> Text
par text = "(" <> text <> ")"

seeds :: [(String, Text)]
seeds =
  [ ("variable", "$x")
  , ("dynamic variable", "$$x")
  , ("integer", "42")
  , ("float", "1.5")
  , ("string", "'hi'")
  , ("interpolated string", "\"value=$x\"")
  , ("heredoc", "<<<EOT\nbody\nEOT")
  , ("nowdoc", "<<<'EOT'\nbody\nEOT")
  , ("interpolated heredoc", "<<<EOT\nvalue=$x\nEOT")
  , ("constant", "Foo\\BAR")
  , ("binary", "$x + 42")
  , ("unary", "!$x")
  , ("assignment", "$x = 42")
  , ("reference assignment", "$x =& $y")
  , ("destructuring", "list($x, , $y) = $z")
  , ("ternary", "$x ? 42 : 1")
  , ("short ternary", "$x ?: 1")
  , ("coalesce", "$x ?? 42")
  , ("clone", "clone $x")
  , ("new", "new Foo($x)")
  , ("anonymous class", "new class { function f() { return 1; } }")
  , ("call", "$x(42)")
  , ("method call", "$x->f(42)")
  , ("nullsafe method call", "$x?->f(42)")
  , ("property", "$x->f")
  , ("nullsafe property", "$x?->f")
  , ("static call", "Foo::f(42)")
  , ("static property", "Foo::$x")
  , ("class constant", "Foo::BAR")
  , ("array", "[$x, 42]")
  , ("array access", "$x[42]")
  , ("match", "match ($x) { 42 => $x, default => 1 }")
  , ("closure", "function ($a) use ($x) { return $a; }")
  , ("arrow with DNF return", "fn($a): (A&B)|C => $a")
  , ("yield", "yield 42")
  , ("keyed yield", "yield 42 => $x")
  , ("yield from", "yield from $x")
  , ("cast", "(int)$x")
  , ("isset", "isset($x)")
  , ("empty", "empty($x)")
  , ("eval", "eval('hi')")
  , ("include", "include 'a.php'")
  , ("print", "print $x")
  , ("shell", "`echo hi`")
  , ("exit", "exit(42)")
  , ("throw", "throw $x")
  ] ++ [ ("binary operator " ++ T.unpack op, "$x " <> op <> " " <> rhs op)
       | op <- binaryOperators ]
  where
    rhs "instanceof" = "Foo"
    rhs _ = "42"

atoms :: [Text]
atoms =
  [ "$x", "$y", "$$x", "42", "0", "1.5", "'hi'", "true", "false", "null"
  , "FOO", "Foo\\BAR", "<<<EOT\nbody\nEOT", "<<<'EOT'\nbody\nEOT" ]

binaryOperators :: [Text]
binaryOperators =
  [ "+", "-", "*", "/", "%", "**", ".", "&", "|", "^", "<<", ">>"
  , "==", "===", "!=", "!==", "<", "<=", ">", ">=", "<=>"
  , "&&", "||", "and", "or", "xor", "instanceof", "|>", "??" ]

contexts :: [(String, Text -> Text)]
contexts =
  [ ("unary not", \e -> "!" <> par e)
  , ("unary minus", \e -> "-" <> par e)
  , ("prefix increment", \e -> "++" <> par e)
  , ("postfix increment", \e -> par e <> "++")
  , ("cast", \e -> "(int)" <> par e)
  , ("assignment rhs", \e -> "$y = " <> par e)
  , ("reference rhs", \e -> "$y =& " <> par e)
  , ("ternary condition", \e -> par e <> " ? $y : 42")
  , ("ternary true", \e -> "$y ? " <> par e <> " : 42")
  , ("ternary false", \e -> "$y ? 42 : " <> par e)
  , ("coalesce lhs", \e -> par e <> " ?? $y")
  , ("coalesce rhs", \e -> "$y ?? " <> par e)
  , ("clone", \e -> "clone " <> par e)
  , ("call base", \e -> par e <> "()")
  , ("first-class call base", \e -> par e <> "(...)")
  , ("call argument", \e -> "foo(" <> par e <> ")")
  , ("method base", \e -> par e <> "->f()")
  , ("first-class method base", \e -> par e <> "->f(...)")
  , ("nullsafe method base", \e -> par e <> "?->f()")
  , ("property base", \e -> par e <> "->f")
  , ("nullsafe property base", \e -> par e <> "?->f")
  , ("static call target", \e -> par e <> "::f()")
  , ("array element", \e -> "[" <> par e <> "]")
  , ("array base", \e -> par e <> "[42]")
  , ("array index", \e -> "$y[" <> par e <> "]")
  , ("match subject", \e -> "match (" <> e <> ") { default => $y }")
  , ("match result", \e -> "match ($y) { default => " <> par e <> " }")
  , ("arrow body", \e -> "fn() => " <> par e)
  , ("yield value", \e -> "yield " <> par e)
  , ("yield key", \e -> "yield " <> par e <> " => $y")
  , ("yield from", \e -> "yield from " <> par e)
  , ("empty", \e -> "empty(" <> e <> ")")
  , ("eval", \e -> "eval(" <> e <> ")")
  , ("include", \e -> "include " <> par e)
  , ("print", \e -> "print " <> par e)
  , ("exit", \e -> "exit(" <> e <> ")")
  , ("throw", \e -> "throw " <> par e)
  ] ++ [ ("binary lhs " ++ T.unpack op, \e -> par e <> " " <> op <> " $y")
       | op <- binaryOperators ]
    ++ [ ("binary rhs " ++ T.unpack op, \e -> "$y " <> op <> " " <> par e)
       | op <- binaryOperators ]
    ++ [ ("compound assignment " ++ T.unpack op, \e -> "$y " <> op <> " " <> par e)
       | op <- ["+=", "-=", "*=", "/=", "%=", "**=", ".=", "&=", "|=", "^=", "<<=", ">>=", "??="] ]

cases :: [Case]
cases =
  [ Case ("seed/" ++ name) name source | (name, source) <- seeds ]
  ++ [ Case ("binary/" ++ T.unpack op) (show lhs ++ " and " ++ show rhs)
            (lhs <> " " <> op <> " " <> rhs)
     | op <- binaryOperators, lhs <- atoms, rhs <- atoms ]
  ++ [ Case ("context/" ++ name) child (context source)
     | (name, context) <- contexts, (child, source) <- seeds ]

requiredGroups :: [String]
requiredGroups = ["seed/" ++ name | (name, _) <- seeds]
  ++ ["binary/" ++ T.unpack op | op <- binaryOperators]
  ++ ["context/" ++ name | (name, _) <- contexts]
