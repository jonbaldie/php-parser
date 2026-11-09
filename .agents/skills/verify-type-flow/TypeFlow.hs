{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM)
import Data.Text (Text)
import qualified Data.Text as T
import Language.PHP (parseProgram)
import Language.PHP.AST
import Language.PHP.Parser.Lexer (runPHPParser)
import Language.PHP.Parser.Type (parseType)
import SourceCheck

data Atom = A | B | Nullable | Group deriving (Eq, Show)
data Op = Union | Intersection deriving (Eq, Show)

data Shape = Simple | Optional | Dnf | UnionShape | IntersectionShape deriving (Eq, Show)

renderAtom :: Atom -> Text
renderAtom A = "A"
renderAtom B = "B"
renderAtom Nullable = "?A"
renderAtom Group = "(A&B)"

renderOp :: Op -> Text
renderOp Union = "|"
renderOp Intersection = "&"

expectedShape :: [Atom] -> [Op] -> Maybe Shape
expectedShape [atom] [] = Just $ case atom of
  A -> Simple
  B -> Simple
  Nullable -> Optional
  Group -> Dnf
expectedShape atoms ops
  | all (== Union) ops && Nullable `notElem` atoms = Just UnionShape
  | all (== Intersection) ops && all (`elem` [A, B]) atoms = Just IntersectionShape
  | otherwise = Nothing

shape :: Type a -> Shape
shape SimpleType {} = Simple
shape NullableType {} = Optional
shape DNFType {} = Dnf
shape UnionType {} = UnionShape
shape IntersectionType {} = IntersectionShape

caseFor :: [Atom] -> [Op] -> Check
caseFor atoms ops = Check (show atoms ++ " " ++ show ops) $ pure $
  let source = case atoms of
        [] -> ""
        first : rest -> T.concat (renderAtom first : concat (zipWith (\op atom -> [renderOp op, renderAtom atom]) ops rest))
      actual = fmap (shape . fst) (runPHPParser parseType "type.php" source)
      expected = expectedShape atoms ops
      ok = case (expected, actual) of
        (Nothing, Left _) -> True
        (Just wanted, Right got) -> wanted == got
        _ -> False
  in if ok then Nothing else Just ("source " ++ T.unpack source ++ "; expected " ++ show expected ++ ", got " ++ show actual)

generated :: [Check]
generated =
  [ caseFor atoms ops
  | size <- [1..3]
  , atoms <- replicateM size [A, B, Nullable, Group]
  , ops <- replicateM (size - 1) [Union, Intersection]
  ]

contextCases :: [Check]
contextCases =
  [ program ("property " ++ T.unpack typ) expected ("<?php class C { public " <> typ <> " $x; }")
  | (typ, expected) <- [("int", True), ("callable", False), ("?callable", False),
                        ("A|callable", False), ("(A&B)|callable", False), ("static", True)]
  ] ++
  [ program ("constant " ++ T.unpack typ) expected ("<?php class C { const " <> typ <> " X = 1; }")
  | (typ, expected) <- [("int", True), ("callable", False), ("A|callable", False), ("static", True)]
  ]

staticModifierCase :: Check
staticModifierCase = Check "static is a modifier, not a property type" $ pure $
  case parseProgram "type.php" "<?php class C { public static $x; }" of
    Right (Program _ [StmtClass _ cls]) -> case classMembers cls of
      [MemberProperty property] | propStatic (propModifier property) && propType property == Nothing -> Nothing
      members -> Just ("unexpected members: " ++ show members)
    result -> Just ("unexpected parse: " ++ show result)

main :: IO ()
main = runChecks (generated ++ contextCases ++ [staticModifierCase])
