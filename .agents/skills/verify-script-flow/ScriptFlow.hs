{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM)
import Control.Monad.State.Strict (get)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import Language.PHP.Parser.Lexer
import SourceCheck

data Event = Strict | Echo | Empty | Bracketed | Unbracketed
  deriving (Eq, Show)

data Script = Script
  { prologue :: Bool
  , blocksNamespace :: Bool
  , namespaceFormSeen :: Maybe Event
  } deriving (Show)

initial :: Script
initial = Script True False Nothing

advance :: Maybe Script -> Event -> Maybe Script
advance Nothing _ = Nothing
advance (Just s) event = case event of
  Strict | prologue s -> Just s
         | otherwise -> Nothing
  Echo -> Just s { prologue = False, blocksNamespace = True }
  Empty -> Just s { prologue = False }
  _ | blocksNamespace s && namespaceFormSeen s == Nothing -> Nothing
    | maybe False (/= event) (namespaceFormSeen s) -> Nothing
    | otherwise -> Just s
        { prologue = False, blocksNamespace = True, namespaceFormSeen = Just event }

render :: Event -> Text
render Strict = "declare(strict_types=1); "
render Echo = "echo 1; "
render Empty = "; "
render Bracketed = "namespace A {} "
render Unbracketed = "namespace A; "

sequences :: [[Event]]
sequences = concatMap (`replicateM` [Strict, Echo, Empty, Bracketed, Unbracketed]) [0..3]

modeCases :: [Check]
modeCases =
  [ program "plain HTML" True "before <?php echo 1; ?> after"
  , program "mixed statement body" True "<?php if (true) { ?>text<?php echo 1; }"
  , program "close tag terminates declare" True "<?php declare(strict_types=1) ?><?php declare(strict_types=1);"
  , program "standalone close tag blocks strict" False "<?php declare(strict_types=1); ?><?php declare(strict_types=1);"
  , program "leading HTML blocks namespace" False "html<?php namespace A;"
  , program "leading shebang permits namespace" True "#!/usr/bin/php\n<?php namespace A;"
  , program "short echo blocks namespace" False "<?= 1 ?><?php namespace A;"
  , program "bare open tag in PHP rejected" False "<?php echo 1; <?php echo 2;"
  ]

frameCases :: [Check]
frameCases =
  [ case runPHPParser frame "frame" "" of
      Left err -> check "statement frame" False (show err)
      Right ((before, first, nested, afterNested, second, depth, seen, current), _) ->
        check "statement frame" (not before && first && not nested && afterNested && not second && depth == 0 && seen && not current)
          (show (before, first, nested, afterNested, second, depth, seen, current))
  , case runPHPParser failedFrame "frame" "" of
      Left err -> check "failed statement restores lexer state" False (show err)
      Right ((before, after), _) -> check "failed statement restores lexer state" (before == after) (show (before, after))
  , case runPHPParser closeHandshake "frame" "" of
      Left err -> check "close-tag flag clears after take" False (show err)
      Right ((first, second), _) -> check "close-tag flag clears after take" (first && not second) (show (first, second))
  ]
  where
    frame = do
      before <- atScriptStart
      (first, nested, afterNested) <- withStatement $ do
        first <- atScriptStart
        nested <- withStatement atScriptStart
        afterNested <- atScriptStart
        pure (first, nested, afterNested)
      second <- withStatement atScriptStart
      st <- get
      pure (before, first, nested, afterNested, second, statementDepth st, scriptStatementSeen st, currentStatementIsFirst st)
    failedFrame = do
      before <- get
      _ <- M.observing (withStatement (markScriptStatement *> fail "expected failure"))
      after <- get
      pure (before, after)
    closeHandshake = do
      noteCloseTagTerminator
      first <- takeCloseTagTerminator
      second <- takeCloseTagTerminator
      pure (first, second)

main :: IO ()
main = runChecks $
  [ program (show events) (case foldl' advance (Just initial) events of Just _ -> True; Nothing -> False)
      ("<?php " <> T.concat (map render events))
  | events <- sequences
  ] ++ modeCases ++ frameCases
