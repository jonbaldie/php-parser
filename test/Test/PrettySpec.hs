{-# LANGUAGE OverloadedStrings #-}

module Test.PrettySpec (prettyTests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import Language.PHP

prettyTests :: TestTree
prettyTests = testGroup "Pretty Printer Specifications"
  [ testCase "Pretty print simple class with property and method" $ do
      let src = "<?php\n\nclass Greeter {\n    public string $greeting = \"Hello\";\n    public function greet(string $name): string {\n        return ($this->greeting . $name);\n    }\n}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrint ast
          assertBool "Printed contains class Greeter" ("class Greeter" `T.isInfixOf` printed)
          assertBool "Printed contains function greet" ("function greet" `T.isInfixOf` printed)

  , testCase "Pretty print PHP 8.4 property hooks" $ do
      let src = "<?php\n\nclass User {\n    public string $name {\n        get => $this->name;\n        set(string $val) {\n            $this->name = $val;\n        }\n    }\n}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrint ast
          assertBool "Printed contains get =>" ("get =>" `T.isInfixOf` printed)
          assertBool "Printed contains set(" ("set(" `T.isInfixOf` printed)

  , testCase "Pretty print PHP 8.4 asymmetric visibility" $ do
      let src = "<?php\n\nclass Status {\n    public private(set) string $title;\n}"
      case parseProgram "test.php" src of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrint ast
          assertBool "Printed contains private(set)" ("private(set)" `T.isInfixOf` printed)

  , testCase "Pretty print PHP 8.5 pipe operator" $ do
      let exprSrc = "(($x |> 'trim') |> 'strtolower')"
      case parseExpression "test.php" exprSrc of
        Left err -> assertFailure (show (formatParseError err))
        Right ast -> do
          let printed = prettyPrintExpr ast
          assertBool "Printed contains |>" ("|>" `T.isInfixOf` printed)
  ]
