{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import SourceCheck

lists :: [a] -> [[a]]
lists xs = concatMap (`replicateM` xs) [0..3]

unique :: Eq a => [a] -> Bool
unique xs = length (nub xs) == length xs

hasAtMostOne :: Eq a => [a] -> [a] -> Bool
hasAtMostOne options xs = length (filter (`elem` options) xs) <= 1

classValid :: [Text] -> Bool
classValid xs = unique xs && not ("abstract" `elem` xs && "final" `elem` xs)

methodValid :: [Text] -> Bool
methodValid xs = unique xs && hasAtMostOne ["public", "private"] xs
  && not ("abstract" `elem` xs && ("final" `elem` xs || "private" `elem` xs))

propertyValid :: [Text] -> Bool
propertyValid xs = unique xs && hasAtMostOne ["public", "private"] xs
  && not ("abstract" `elem` xs)
  && not ("static" `elem` xs && "readonly" `elem` xs)

constantValid :: [Text] -> Bool
constantValid xs = unique xs && hasAtMostOne ["public", "private"] xs

prefix :: [Text] -> Text
prefix = T.concat . map (<> " ")

modifierCases :: [Check]
modifierCases =
  [ program ("class " ++ show xs) (classValid xs) ("<?php " <> prefix xs <> "class C {}")
  | xs <- lists ["abstract", "final", "readonly"] ]
  ++ [ program ("method " ++ show xs) (methodValid xs)
       ("<?php class C { " <> prefix xs <> "function f()" <> if "abstract" `elem` xs then "; }" else " {} }")
     | xs <- lists ["public", "private", "static", "final", "abstract"] ]
  ++ [ program ("property " ++ show xs) (not (null xs) && propertyValid xs)
       ("<?php class C { " <> prefix xs <> "int $x; }")
     | xs <- lists ["public", "private", "static", "readonly", "final", "abstract"] ]
  ++ [ program ("constant " ++ show xs) (constantValid xs)
       ("<?php class C { " <> prefix xs <> "const X = 1; }")
     | xs <- lists ["public", "private", "final"] ]

hookCases :: [Check]
hookCases =
  [ program ("hooks " ++ show hooks) (unique hooks)
      ("<?php class C { public int $x { " <> T.concat (map render hooks) <> "} }")
  | hooks <- lists (["get", "set"] :: [Text])
  ]
  where
    render "get" = "get => 1; "
    render _ = "set => 1; "

main :: IO ()
main = runChecks (modifierCases ++ hookCases)
