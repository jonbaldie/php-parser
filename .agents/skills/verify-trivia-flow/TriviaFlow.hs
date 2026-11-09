{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable (toList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.PHP
import SourceCheck
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

data Scenario = Scenario String Text Text

data Comment = Comment String Text Trivia

scenarios :: [Scenario]
scenarios =
  [ Scenario "shared variable span" "<?php " " $x;"
  , Scenario "if control" "<?php if (true) { " " echo 1; }"
  , Scenario "bracketed namespace" "<?php namespace A { " " echo 1; }"
  , Scenario "anonymous namespace" "<?php namespace { " " echo 1; }"
  , Scenario "property hook" "<?php class C { public string $x { " " get => $this->x; } }"
  , Scenario "trait adaptation" "<?php class C { use T { " " T::m as n; } }"
  , Scenario "first-class callable" "<?php foo(...) " "; echo 1;"
  , Scenario "exit callable" "<?php exit(...) " "; echo 1;"
  , Scenario "method" "<?php class C { public " " function m() {} }"
  , Scenario "typed constant" "<?php class C { const string X " " = 'x'; }"
  ]

comments :: [Comment]
comments =
  [ Comment "block" "/* c */" (CommentBlock " c ")
  , Comment "doc" "/** c */" (DocBlock " c ")
  , Comment "line" "// c\n" (CommentLine " c")
  , Comment "hash" "# c\n" (CommentLine " c")
  ]

commentCase :: Scenario -> Comment -> Check
commentCase (Scenario name before after) (Comment style sourceComment expected) =
  Check (name ++ "/" ++ style) $ pure $
    let source = before <> sourceComment <> after
    in case parseProgram "trivia.php" source of
      Left err -> Just ("source rejected: " ++ show (formatParseError err) ++ "\n  source: " ++ T.unpack source)
      Right ast ->
        let originalTrivia = foldMap annTrivia ast
            printed = prettyPrint ast
        in case parseProgram "printed.php" printed of
          Left err -> Just ("printed output rejected: " ++ show (formatParseError err) ++ "\n  printed: " ++ T.unpack printed)
          Right reparsed ->
            let printedTrivia = foldMap annTrivia reparsed
            in if originalTrivia == [expected] && printedTrivia == [expected]
              then Nothing
              else Just ("expected exactly " ++ show [expected] ++ " at both parses; got " ++ show originalTrivia ++ " then " ++ show printedTrivia ++ "\n  printed: " ++ T.unpack printed)

ownershipCase :: Scenario -> Check
ownershipCase (Scenario name before after) = Check (name ++ "/first-span-owner") $ pure $
  case parseProgram "owner.php" (before <> "/* c */" <> after) of
    Left err -> Just ("source rejected: " ++ show (formatParseError err))
    Right ast ->
      let annotations = toList ast
          (_, ok) = foldl' visit (Set.empty, True) annotations
          visit (seen, valid) ann =
            let span' = annValue ann
            in (Set.insert span' seen, valid && (Set.notMember span' seen || null (annTrivia ann)))
      in if ok then Nothing else Just "a later annotation with the same span also owns trivia"

rootCase :: Check
rootCase = Check "widened root does not replace child spans" $ pure $
  case parseExpression "root.php" "(1 + $x)" of
    Right expr@(ExprBinary _ _ left right) ->
      let offsets node = let sp = annValue (annotation node)
                         in (posOffset (spanStart sp), posOffset (spanEnd sp))
          actual = (offsets expr, offsets left, offsets right)
      in if actual == ((0, 8), (1, 2), (5, 7)) then Nothing else Just (show actual)
    result -> Just ("unexpected parse: " ++ show result)

main :: IO ()
main = do
  args <- getArgs
  let commentChecks = [commentCase scenario comment | scenario <- scenarios, comment <- comments]
      ownershipChecks = map ownershipCase scenarios ++ [rootCase]
  case args of
    [] -> runChecks (commentChecks ++ ownershipChecks)
    ["--comments"] -> runChecks commentChecks
    ["--ownership"] -> runChecks ownershipChecks
    _ -> do
      putStrLn "Usage: TriviaFlow [--comments|--ownership]"
      exitWith (ExitFailure 2)
