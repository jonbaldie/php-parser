{-# LANGUAGE OverloadedStrings #-}

module Test.FoldQueryCases (foldQueryCases) where

import Data.Text (Text)

foldQueryCases :: [(String, Text, [Text])]
foldQueryCases =
  [ ("closure captures (Issue #109)", "function () use ($outer, &$ref) { return $outer + $ref; }", ["outer", "ref", "outer", "ref"])
  , ("interpolated strings (Issue #122)", "\"prefix {$value} {$object->field}\"", ["value", "object"])
  , ("attribute arguments (Issue #52)", "#[A($attribute)] function ($parameter = $default) { return $body; }", ["attribute", "default", "body"])
  , ("static bindings (Issue #188)", "function () { static $state = $initial; return $state; }", ["state", "initial", "state"])
  , ("catch bindings (Issue #194)", "function () { try { run($input); } catch (Exception $error) { log($error); } }", ["input", "error", "error"])
  , ("interpolated heredocs (Issue #234)", "<<<PHP\n$value {$object->field}\nPHP", ["value", "object"])
  ]
