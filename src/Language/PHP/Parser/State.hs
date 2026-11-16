module Language.PHP.Parser.State
  ( LexerState
  , NamespaceForm (..)
  , NamespaceProblem (..)
  , StatementEntry
  , initialLexerState
  , beginStatement
  , leaveStatement
  , markScriptStatement
  , atScriptStart
  , markNonDeclareContent
  , markNamespaceBlockingContent
  , enterNamespace
  , noteCloseTagTerminator
  , consumeCloseTag
  , inDeclarePrologue
  , tokenEndBeforeTrivia
  , finishTrailingTrivia
  , spanEndpoint
  , takeTrivia
  , recordTrivia
  , moveTrivia
  , addTrivia
  , triviaBySpan
  ) where

import qualified Data.Map.Strict as Map

import Language.PHP.AST (Trivia)
import Language.PHP.Span (SourcePos, Span)

data LexerState = LexerState
  { tokenBoundary :: !TokenBoundary
  , triviaState :: !TriviaState
  , statementSite :: !StatementSite
  , scriptHistory :: !ScriptHistory
  , closeTagUse :: !CloseTagUse
  } deriving (Eq, Show)

data TokenBoundary = NoTokenBoundary | TokenBoundary !SourcePos !SourcePos
  deriving (Eq, Show)

data TriviaState = TriviaState ![Trivia] !(Map.Map Span [Trivia])
  deriving (Eq, Show)

data StatementSite
  = BetweenStatements
  | TopLevelStatement !Firstness
  | NestedStatement !StatementSite
  deriving (Eq, Show)

data Firstness = FirstStatement | LaterStatement
  deriving (Eq, Show)

data ScriptProgress = ScriptUntouched | ScriptStarted
  deriving (Eq, Show)

data DeclarePosition = InDeclarePrologue | AfterDeclarePrologue
  deriving (Eq, Show)

data FirstNamespace
  = FirstNamespaceAllowed
  | FirstNamespaceBlocked
  | FirstNamespaceDeclared
  deriving (Eq, Show)

data NamespaceForm = BracketedNamespace | UnbracketedNamespace
  deriving (Eq, Show)

data ScriptHistory = ScriptHistory
  { scriptProgress :: !ScriptProgress
  , declarePosition :: !DeclarePosition
  , firstNamespace :: !FirstNamespace
  , namespaceForm :: !(Maybe NamespaceForm)
  } deriving (Eq, Show)

data CloseTagUse = StandaloneCloseTag | StatementTerminatorTag
  deriving (Eq, Show)

data NamespaceProblem = NamespaceTooLate | NamespaceFormsMixed
  deriving (Eq, Show)

data StatementEntry = StatementEntry !StatementSite !ScriptProgress

initialLexerState :: LexerState
initialLexerState = LexerState NoTokenBoundary (TriviaState [] Map.empty)
  BetweenStatements (ScriptHistory ScriptUntouched InDeclarePrologue FirstNamespaceAllowed Nothing)
  StandaloneCloseTag

beginStatement :: LexerState -> (StatementEntry, LexerState)
beginStatement st =
  let site = statementSite st
      progress = scriptProgress (scriptHistory st)
      entered = case site of
        BetweenStatements -> TopLevelStatement $ case progress of
          ScriptUntouched -> FirstStatement
          ScriptStarted -> LaterStatement
        _ -> NestedStatement site
  in (StatementEntry site progress, st { statementSite = entered })

leaveStatement :: StatementEntry -> LexerState -> LexerState
leaveStatement (StatementEntry site progress) st = st
  { statementSite = site
  , scriptHistory = (scriptHistory st)
      { scriptProgress = case site of
          BetweenStatements -> ScriptStarted
          _ -> progress
      }
  }

markScriptStatement :: LexerState -> LexerState
markScriptStatement st = st
  { scriptHistory = (scriptHistory st) { scriptProgress = ScriptStarted } }

atScriptStart :: LexerState -> Bool
atScriptStart st = case statementSite st of
  TopLevelStatement FirstStatement -> True
  _ -> False

markNonDeclareContent :: LexerState -> LexerState
markNonDeclareContent st = case statementSite st of
  BetweenStatements -> st
    { scriptHistory = (scriptHistory st) { declarePosition = AfterDeclarePrologue } }
  _ -> st

markNamespaceBlockingContent :: LexerState -> LexerState
markNamespaceBlockingContent st = case (statementSite st, firstNamespace (scriptHistory st)) of
  (BetweenStatements, FirstNamespaceAllowed) -> st
    { scriptHistory = (scriptHistory st) { firstNamespace = FirstNamespaceBlocked } }
  _ -> st

enterNamespace :: NamespaceForm -> LexerState -> Either NamespaceProblem LexerState
enterNamespace form st
  | isTopLevelStatement (statementSite st)
  , firstNamespace history == FirstNamespaceBlocked = Left NamespaceTooLate
  | Just previous <- namespaceForm history
  , previous /= form = Left NamespaceFormsMixed
  | otherwise = Right st
      { scriptHistory = history
          { firstNamespace = if isTopLevelStatement (statementSite st)
              then FirstNamespaceDeclared
              else firstNamespace history
          , namespaceForm = Just form
          }
      }
  where
    history = scriptHistory st

isTopLevelStatement :: StatementSite -> Bool
isTopLevelStatement (TopLevelStatement _) = True
isTopLevelStatement _ = False

noteCloseTagTerminator :: LexerState -> LexerState
noteCloseTagTerminator st = st { closeTagUse = StatementTerminatorTag }

consumeCloseTag :: LexerState -> LexerState
consumeCloseTag st =
  let consumed = (markScriptStatement st) { closeTagUse = StandaloneCloseTag }
  in case closeTagUse st of
    StandaloneCloseTag -> markNonDeclareContent consumed
    StatementTerminatorTag -> consumed

inDeclarePrologue :: LexerState -> Bool
inDeclarePrologue st = isTopLevelStatement (statementSite st)
  && declarePosition (scriptHistory st) == InDeclarePrologue

tokenEndBeforeTrivia :: SourcePos -> LexerState -> SourcePos
tokenEndBeforeTrivia before st = case tokenBoundary st of
  TokenBoundary end triviaEnd | triviaEnd == before -> end
  _ -> before

finishTrailingTrivia :: SourcePos -> SourcePos -> LexerState -> LexerState
finishTrailingTrivia end after st = st { tokenBoundary = TokenBoundary end after }

spanEndpoint :: SourcePos -> SourcePos -> LexerState -> SourcePos
spanEndpoint start here st = case tokenBoundary st of
  TokenBoundary end triviaEnd
    | triviaEnd == here && start <= end && end <= here -> end
  _ -> here

takeTrivia :: LexerState -> ([Trivia], LexerState)
takeTrivia st = case triviaState st of
  TriviaState pending bySpan -> (pending, st { triviaState = TriviaState [] bySpan })

recordTrivia :: Span -> [Trivia] -> LexerState -> LexerState
recordTrivia sp triv st = case triviaState st of
  TriviaState pending bySpan -> st
    { triviaState = TriviaState pending (Map.insert sp triv bySpan) }

moveTrivia :: Span -> Span -> LexerState -> LexerState
moveTrivia from to st = case triviaState st of
  TriviaState pending bySpan -> case Map.lookup from bySpan of
    Nothing -> st
    Just triv -> st { triviaState = TriviaState pending
      (Map.insertWith (flip (++)) to triv (Map.delete from bySpan)) }

addTrivia :: Trivia -> LexerState -> LexerState
addTrivia t st = case triviaState st of
  TriviaState pending bySpan -> st { triviaState = TriviaState (pending ++ [t]) bySpan }

triviaBySpan :: LexerState -> Map.Map Span [Trivia]
triviaBySpan st = case triviaState st of
  TriviaState _ bySpan -> bySpan
