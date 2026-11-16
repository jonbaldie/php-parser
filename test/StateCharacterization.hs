{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, replicateM)
import qualified Data.Text as T
import Language.PHP
import Test.Gen.PHPSource
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

main :: IO ()
main = do
  forM_ programs $ \source -> print (source, fmap (\ast -> (show ast, prettyPrint ast)) (parseProgram "contract.php" source))
  forM_ expressions $ \source -> print (source, fmap (\ast -> (show ast, prettyPrintExpr ast)) (parseExpression "contract.php" source))
  putStrLn ("Characterized " ++ show (length programs) ++ " programs and " ++ show (length expressions) ++ " expressions.")

programs :: [T.Text]
programs =
  [ "<?php\n" <> snippetText (unGen (renderFeature 0 feature) (mkQCGen seed) 30)
  | feature <- allFeatures, seed <- [0 .. 7]
  ] ++
  [ renderProgram (unGen (genProgram version) (mkQCGen seed) 30)
  | version <- allVersions, seed <- [0 .. 24]
  ] ++
  [ "<?php declare(strict_types=1); declare(ticks=1); namespace A; echo 1; namespace B; echo 2;"
  , "html<?php echo 1 ?>\ntext<?=2?>\r\n"
  , "<?php namespace A {} namespace B;"
  , "<?php echo 1; namespace A;"
  , "<?php if (true) { declare(strict_types=1); }"
  , "<?php __halt_compiler();\n?>ignored"
  , "<?php switch ($x) {default: break; default: break;}"
  , "<?php try {}"
  ] ++
  [ "<?php " <> T.concat events
  | events <- sequences 3 ["declare(strict_types=1); ", "echo 1; ", "; ", "namespace A {} ", "namespace A; "]
  ] ++
  [ "<?php " <> T.unwords modifiers <> " class C {}"
  | modifiers <- sequences 3 ["abstract", "final", "readonly"]
  ] ++
  [ "<?php class C { " <> T.unwords modifiers <> " function f() {} }"
  | modifiers <- sequences 3 ["public", "private", "static", "abstract", "final"]
  ] ++
  [ "<?php class C { " <> T.unwords modifiers <> " int $x; }"
  | modifiers <- sequences 3 ["public", "private", "static", "readonly"]
  ] ++
  [ "<?php class C { public string $x { " <> T.concat hooks <> " } }"
  | hooks <- sequences 3 ["get => $this->x; ", "set => $value; "]
  ]

sequences :: Int -> [a] -> [[a]]
sequences count choices = concatMap (`replicateM` choices) [0 .. count]

expressions :: [T.Text]
expressions =
  [ "f(1, a: 2)", "f(a: 2, 1)", "f(...$x, 1)", "f(a: 2, ...$x)"
  , "function ($x, $x) {}", "fn(...$x, $y) => 1", "fn($x, ...$y) => 1"
  , "$a ? $b : $c ? $d : $e", "($a ? $b : $c) ? $d : $e", "$a ?: $b ?: $c"
  , "$a?->b = 1", "($a?->b)(1)", "new Foo()->bar", "new Foo->bar"
  , "$x instanceof $y->z", "$x instanceof ($y + $z)"
  , "0x1_f", "0b1_0", "08", "1_", "1.0e+2", "1.e-2"
  , "'a\\nb'", "\"a\\n$x\"", "\"\\u{10ffff}\"", "\"\\u{110000}\"", "\"\\u{d800}\""
  , "<<<'END'\n$x\nEND", "<<<\"END\"\n$x\nEND", "<<<END\n  x\n  END"
  , "match ($x) {default => 1, default => 2}", "[1, 2]", "[$a, ...$b]"
  ]
  ++ [ "f(" <> T.intercalate ", " args <> ")"
     | args <- sequences 4 ["1", "name: 1", "...$xs", "name: ...$xs"] ]
  ++ [ "fn(" <> T.intercalate ", " params <> ") => 1"
     | params <- sequences 3 ["$a", "$b", "...$a", "...$b"] ]
