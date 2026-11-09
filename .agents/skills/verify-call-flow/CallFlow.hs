{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import SourceCheck

data Argument = Positional | Unpacked | Named deriving (Eq, Show)
data Parameter = Parameter Text Bool deriving (Eq, Show)

lists :: [a] -> [[a]]
lists xs = concatMap (`replicateM` xs) [0..3]

validArguments :: [Argument] -> Bool
validArguments = go False False
  where
    go _ _ [] = True
    go unpack named (x : xs) = case x of
      Positional -> not unpack && not named && go unpack named xs
      Unpacked -> go True named xs
      Named -> go unpack True xs

argument :: Argument -> Text
argument Positional = "1"
argument Unpacked = "...$args"
argument Named = "name: 1"

validParameters :: [Parameter] -> Bool
validParameters xs =
  let names = [name | Parameter name _ <- xs]
      initial = case xs of [] -> []; _ -> init xs
  in length names == length (nub names) && all (\(Parameter _ variadic) -> not variadic) initial

parameter :: Parameter -> Text
parameter (Parameter name variadic) = (if variadic then "..." else "") <> "$" <> name

argumentCases :: [Check]
argumentCases =
  [ expression ("arguments " ++ show xs) (validArguments xs)
      ("f(" <> T.intercalate ", " (map argument xs) <> ")")
  | xs <- lists [Positional, Unpacked, Named] ]

parameterCases :: [Check]
parameterCases =
  [ program ("parameters " ++ show xs) (validParameters xs)
      ("<?php function f(" <> T.intercalate ", " (map parameter xs) <> ") {}")
  | xs <- lists [Parameter "a" False, Parameter "b" False, Parameter "a" True, Parameter "b" True] ]

postfixCases :: [Check]
postfixCases =
  [ expression ("postfix " ++ T.unpack source) (not hasNullsafe || permitsNullsafe) source
  | (base, hasNullsafe) <- [("$x->p", False), ("$x?->p", True)]
  , (suffix, permitsNullsafe) <-
      [("->m(...)", False), ("->p->m(...)", False), ("->m()", True),
       ("[0]++", False), ("[0] = 1", False), ("->p = 1", False)]
  , let source = base <> suffix
  ]

main :: IO ()
main = runChecks (argumentCases ++ parameterCases ++ postfixCases)
