{-# LANGUAGE RecordWildCards #-}
import qualified Data.List as List
import Language.PHP.Parser.State (NamespaceForm(..), NamespaceProblem(..))
import qualified Language.PHP.Parser.State as New

data Old = Old
  { seen :: Bool, depth :: Int, first :: Bool, nonDeclare :: Bool
  , blocking :: Bool, declared :: Bool, form :: Maybe NamespaceForm, terminator :: Bool
  } deriving (Eq, Show)
data Event = Enter | Commit | Rollback | Script | NonDeclare | Blocking | Claim | Close
  | Namespace NamespaceForm deriving Show

type Frame = (Old, New.LexerState)
type Pair = (Old, New.LexerState, [Frame])

initial :: Pair
initial = (Old False 0 False False False False Nothing False, New.initialLexerState, [])

oldNamespace :: NamespaceForm -> Old -> Either NamespaceProblem Old
oldNamespace f o@Old{..}
  | depth == 1 && not declared && blocking = Left NamespaceTooLate
  | Just previous <- form, previous /= f = Left NamespaceFormsMixed
  | otherwise = Right o { declared = declared || depth == 1, form = Just f }

observeOld :: Old -> (Bool, Bool, Bool, Either NamespaceProblem (), Either NamespaceProblem ())
observeOld o@Old{..} =
  (first, depth == 1 && not nonDeclare, depth == 0 && not seen,
   () <$ oldNamespace BracketedNamespace o, () <$ oldNamespace UnbracketedNamespace o)

observeNew :: New.LexerState -> (Bool, Bool, Bool, Either NamespaceProblem (), Either NamespaceProblem ())
observeNew n =
  (New.atScriptStart n, New.inDeclarePrologue n, New.atScriptStart (snd (New.beginStatement n)),
   () <$ New.enterNamespace BracketedNamespace n, () <$ New.enterNamespace UnbracketedNamespace n)

step :: Event -> Pair -> Maybe Pair
step e (o@Old{..}, n, frames) = case e of
  Enter -> Just (o { depth = depth + 1, first = depth == 0 && not seen }, snd (New.beginStatement n), (o,n):frames)
  Commit -> case frames of
    [] -> Nothing
    (before, previous):rest -> Just
      (o { depth = depthOld before, first = firstOld before, seen = seenOld before || depthOld before == 0 },
       New.leaveStatement (fst (New.beginStatement previous)) n, rest)
  Rollback -> case frames of
    [] -> Nothing
    (before, previous):rest -> Just (before, previous, rest)
  Script -> Just (o { seen = True }, New.markScriptStatement n, frames)
  NonDeclare -> Just (o { nonDeclare = nonDeclare || depth == 0 }, New.markNonDeclareContent n, frames)
  Blocking -> Just (o { blocking = blocking || depth == 0 }, New.markNamespaceBlockingContent n, frames)
  Claim -> Just (o { terminator = True }, New.noteCloseTagTerminator n, frames)
  Close -> Just (o { seen = True, terminator = False, nonDeclare = nonDeclare || (not terminator && depth == 0) }, New.consumeCloseTag n, frames)
  Namespace f -> case (oldNamespace f o, New.enterNamespace f n) of
    (Right nextOld, Right nextNew) -> Just (nextOld, nextNew, frames)
    (Left a, Left b) | a == b -> Just (o, n, frames)
    bad -> error (show e ++ " transition mismatch " ++ show bad)
  where
    depthOld = Main.depth
    firstOld = Main.first
    seenOld = Main.seen

events :: [Event]
events = [Enter, Commit, Rollback, Script, NonDeclare, Blocking, Claim, Close,
          Namespace BracketedNamespace, Namespace UnbracketedNamespace]

walk :: Int -> [Event] -> Pair -> Int
walk remaining trace pair@(o,n,_) =
  if observeOld o /= observeNew n then error (show (reverse trace, o, n, observeOld o, observeNew n))
  else if remaining == 0 then 1 else 1 + List.foldl' (+) 0
    [ walk (remaining - 1) (event:trace) next | event <- events, Just next <- [step event pair] ]

main :: IO ()
main = putStrLn ("Checked " ++ show (walk 6 [] initial) ++ " state prefixes; failures 0.")
