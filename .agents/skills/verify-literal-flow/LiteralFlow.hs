{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import Language.PHP
import SourceCheck
import System.Timeout (timeout)

roundTrip :: String -> Text -> Check
roundTrip name source = Check name $ pure $
  case parseExpression "literal.php" source of
    Left err -> Just ("source rejected: " ++ show (formatParseError err) ++ "\n  source: " ++ T.unpack source)
    Right ast ->
      let printed = prettyPrintExpr ast
      in case parseExpression "printed.php" printed of
        Left err -> Just ("printed source rejected: " ++ show (formatParseError err) ++ "\n  printed: " ++ T.unpack printed)
        Right actual -> if stripAnnotations ast == stripAnnotations actual
          then Nothing
          else Just ("AST changed\n  source: " ++ T.unpack source ++ "\n  printed: " ++ T.unpack printed)

decoded :: String -> Text -> Text -> Check
decoded name source expected = Check name $ pure $
  case parseExpression "literal.php" source of
    Right (ExprLit _ (LitString _ value _)) | value == expected -> Nothing
    result -> Just ("expected decoded " ++ show expected ++ ", got " ++ show result)

commentBoundary :: String -> Text -> Check
commentBoundary name source = Check name $ pure $
  case parseProgram "comment.php" source of
    Right ast@(Program _ stmts)
      | length (foldMap annTrivia ast) == 1
      , any isHtml stmts -> Nothing
    result -> Just ("expected one comment and inline HTML, got " ++ show result)
  where
    isHtml StmtInlineHtml {} = True
    isHtml _ = False

unterminated :: String -> Text -> Check
unterminated name source = Check name $ do
  result <- timeout 2000000 (evaluate (parseExpression "unterminated.php" source))
  pure $ case result of
    Just (Left _) -> Nothing
    Nothing -> Just "parser did not finish within two seconds"
    Just (Right ast) -> Just ("accepted unterminated source: " ++ show ast)

strings :: [Check]
strings =
  [ decoded "single quote preserves unknown escape" "'a\\nb'" "a\\nb"
  , decoded "single quote decodes quote and backslash" "'a\\\'b\\\\c'" "a'b\\c"
  , decoded "double quote decodes newline and hex" "\"a\\n\\x41\"" "a\nA"
  ] ++
  [ roundTrip ("string or command " ++ show n) source
  | (n, source) <- zip [(1 :: Int)..]
      ["'hello'", "'a\\\'b\\\\c'", "\"a\\n\\x41\"", "\"hello $name\"",
       "\"$name \\$literal\"", "\"\\u{2603} $name\"", "`echo $name`", "`$name \\$literal`"]
  ]

heredocs :: [Check]
heredocs =
  [ roundTrip ("heredoc " ++ T.unpack tag ++ "/" ++ show index)
      ("<<<" <> tag <> "\n" <> body <> "\n" <> tag)
  | tag <- ["EOF", "END"]
  , let escapedTag = "\\x45" <> T.drop 1 tag
  , (index, body) <- zip [(1 :: Int)..]
      [ escapedTag <> " $name", "$name" <> escapedTag, "text\n" <> escapedTag <> " $name",
        tag <> "_MORE\n$name", "hello \\$name", "one\ntwo" ]
  ] ++
  [ roundTrip ("nowdoc " ++ T.unpack tag) ("<<<'" <> tag <> "'\n\\n $name\n" <> tag)
  | tag <- ["EOF", "END"]
  ]

indentCase :: Check
indentCase = Check "heredoc strips closing-label indentation" $ pure $
  case parseExpression "literal.php" "<<<EOF\n  one\n  two\n  EOF" of
    Right (ExprLit _ (LitHeredoc _ "EOF" "one\ntwo" False _)) -> Nothing
    result -> Just ("unexpected indentation result: " ++ show result)

main :: IO ()
main = runChecks $ strings ++ heredocs ++ [indentCase] ++
  [ commentBoundary "slash comment stops at close tag" "<?php echo 1; // c ?>tail"
  , commentBoundary "hash comment stops at close tag" "<?php echo 1; # c ?>tail"
  , unterminated "unterminated heredoc" "<<<EOF\ntext"
  , unterminated "unterminated nowdoc" "<<<'EOF'\ntext"
  , unterminated "unterminated quote" "\"text"
  , unterminated "unterminated command" "`echo"
  ]
