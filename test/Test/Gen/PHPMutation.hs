{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.Gen.PHPMutation
-- Description : Near-miss PHP programs, for the rejection half of the oracle
--
-- "Test.Gen.PHPSource" emits only valid programs. On a valid-only corpus every
-- differential property collapses into the same trivially-true assertion: the
-- interpreter accepts everything, and the library accepts everything. The
-- oracle can only say something new about /rejection/ if it is given programs
-- worth rejecting.
--
-- This module supplies them. A 'Mutation' is a small, targeted perturbation of
-- a catalogue-rendered snippet -- moving a return-only type into parameter
-- position, duplicating a declared member name, attaching a modifier the
-- grammar forbids, giving a variadic a default -- chosen so the result is
-- something a PHP programmer might plausibly write and PHP nonetheless rejects.
--
-- Mutations are derived from the feature catalogue rather than hand-written as
-- standalone programs: each names the feature it perturbs and rewrites that
-- feature's own rendered text, so the near-miss inherits whatever the catalogue
-- currently emits. 'mutatedApplied' reports whether the rewrite found its
-- anchor, which turns catalogue drift into a failing test rather than silently
-- skipped coverage.
module Test.Gen.PHPMutation
  ( -- * Mutations
    Mutation (..)
  , LibraryStance (..)
  , allMutations
  , mutationsUpTo

    -- * Mutated programs
  , MutatedProgram (..)
  , renderMutated
  , genMutatedProgram
  , genMutationOf
  , shrinkMutatedProgram
  ) where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Gen.PHPSource
import Test.QuickCheck

--------------------------------------------------------------------------------
-- Mutations
--------------------------------------------------------------------------------

-- | What the library currently does with this near-miss, pinned so that the
-- differential properties are two-sided.
--
-- A 'Caught' mutation asserts the library agrees with PHP. A 'KnownFalseAccept'
-- asserts it does /not/ -- so fixing the underlying bug fails this suite and
-- forces the stance to be updated in the same change. Neither direction can
-- drift unnoticed.
data LibraryStance
  = -- | The library rejects it too.
    Caught
  | -- | The library accepts it. The reason is quoted in the failure when the
    -- divergence disappears.
    KnownFalseAccept String
  deriving (Eq, Show)

data Mutation = Mutation
  { mutationName :: String
  , -- | The catalogue feature whose rendered text is perturbed.
    mutationFeature :: String
  , -- | What PHP says, recorded so a counterexample names the rule being broken.
    mutationPHPRule :: String
  , mutationStance :: LibraryStance
  , mutationRewrite :: Text -> Maybe Text
  }

-- | Mutations carry a rewrite function, so only the identifying fields are
-- shown. A counterexample that needs the perturbed source shows the
-- 'MutatedProgram' instead.
instance Show Mutation where
  show m = mutationName m ++ " (perturbs " ++ mutationFeature m ++ ")"

-- | Mutations whose target feature exists in PHP @v@'s grammar.
mutationsUpTo :: PHPVersion -> [Mutation]
mutationsUpTo v = filter available allMutations
  where
    available m = case featureByName (mutationFeature m) of
      Nothing -> False
      Just f -> featureSince f <= v

-- | Replace the first occurrence, or fail if the anchor is not there.
at :: Text -> Text -> Text -> Maybe Text
at anchor replacement src = case T.breakOn anchor src of
  (_, rest) | T.null rest -> Nothing
  (before, rest) -> Just (before <> replacement <> T.drop (T.length anchor) rest)

-- | The first parameter of the @mixedArgs@ function the @functions@ feature
-- renders. Every parameter-type mutation rewrites this one anchor.
firstParam :: Text
firstParam = "(mixed $a, iterable $b"

paramType :: String -> Text -> String -> LibraryStance -> Mutation
paramType name ty rule stance =
  Mutation
    { mutationName = name
    , mutationFeature = "functions"
    , mutationPHPRule = rule
    , mutationStance = stance
    , mutationRewrite = at firstParam ("(" <> ty <> " $a, iterable $b")
    }

allMutations :: [Mutation]
allMutations =
  [ -- Return-only types moved into parameter position.
    paramType
      "static-in-parameter-position"
      "static"
      "Cannot use the static modifier on a parameter"
      (KnownFalseAccept "the library accepts `static` as a parameter type")
  , paramType
      "static-union-in-parameter-position"
      "self|static|null"
      "syntax error, unexpected token \"static\""
      (KnownFalseAccept "the library accepts `self|static|null` as a parameter type")
  , paramType
      "never-in-parameter-position"
      "never"
      "never cannot be used as a parameter type"
      (KnownFalseAccept "the library accepts `never` as a parameter type")
  , paramType
      "void-in-parameter-position"
      "void"
      "void cannot be used as a parameter type"
      (KnownFalseAccept "the library accepts `void` as a parameter type")
  , paramType
      "nullable-mixed-parameter"
      "?mixed"
      "Type mixed cannot be marked as nullable since mixed already includes null"
      (KnownFalseAccept "the library accepts `?mixed`")
  , Mutation
      { mutationName = "variadic-parameter-with-default"
      , mutationFeature = "functions"
      , mutationPHPRule = "Variadic parameter cannot have a default value"
      , mutationStance = KnownFalseAccept "the library accepts a default on a variadic parameter"
      , mutationRewrite = at "int ...$rest)" "int ...$rest = 1)"
      }
  , -- Modifier combinations that are individually legal.
    Mutation
      { mutationName = "final-private-class-constant"
      , mutationFeature = "classes"
      , mutationPHPRule = "Private constant cannot be final as it is not visible to other classes"
      , mutationStance = KnownFalseAccept "the library accepts `final private const`"
      , mutationRewrite = at "    public const VERSION = '1.0';" "    final private const VERSION = '1.0';"
      }
  , Mutation
      { mutationName = "abstract-final-class"
      , mutationFeature = "classes"
      , mutationPHPRule = "Cannot use the final modifier on an abstract class"
      , mutationStance = Caught
      , mutationRewrite = at "abstract class Base" "abstract final class Base"
      }
  , Mutation
      { mutationName = "untyped-readonly-property"
      , mutationFeature = "classes"
      , mutationPHPRule = "Readonly property must have type"
      , mutationStance = Caught
      , mutationRewrite = at "    public readonly string $id;" "    public readonly $id;"
      }
  , Mutation
      { mutationName = "readonly-static-property"
      , mutationFeature = "classes"
      , mutationPHPRule = "Static property cannot be readonly"
      , mutationStance = Caught
      , mutationRewrite = at "    protected static int $count = 0;" "    public readonly static int $count;"
      }
  , -- Duplicate members within one class. These need a per-declaration member
    -- table, not a program-wide symbol table, so they are in contract.
    Mutation
      { mutationName = "duplicate-class-constant"
      , mutationFeature = "classes"
      , mutationPHPRule = "Cannot redefine class constant"
      , mutationStance = KnownFalseAccept "the library accepts a class constant declared twice"
      , mutationRewrite =
          at
            "    public const VERSION = '1.0';"
            "    public const VERSION = '1.0';\n    public const VERSION = '2.0';"
      }
  , Mutation
      { mutationName = "duplicate-property"
      , mutationFeature = "classes"
      , mutationPHPRule = "Cannot redeclare property"
      , mutationStance = KnownFalseAccept "the library accepts a property declared twice"
      , mutationRewrite =
          at
            "    protected static int $count = 0;"
            "    protected static int $count = 0;\n    protected static int $count = 1;"
      }
  , Mutation
      { mutationName = "duplicate-method"
      , mutationFeature = "classes"
      , mutationPHPRule = "Cannot redeclare method"
      , mutationStance = KnownFalseAccept "the library accepts a method declared twice"
      , mutationRewrite =
          at
            "    abstract protected function describe(): string;"
            ( T.intercalate
                "\n"
                [ "    abstract protected function describe(): string;"
                , ""
                , "    public function twice(): int {"
                , "        return 1;"
                , "    }"
                , ""
                , "    public function twice(): int {"
                , "        return 2;"
                , "    }"
                ]
            )
      }
  , -- Members where the declaration kind forbids them.
    Mutation
      { mutationName = "property-in-interface"
      , mutationFeature = "interfaces-and-traits"
      , mutationPHPRule = "Interfaces may only include hooked properties"
      , mutationStance = Caught
      , mutationRewrite =
          at
            "    public function describe(): string;"
            "    public int $plain;\n\n    public function describe(): string;"
      }
  , Mutation
      { mutationName = "property-in-enum"
      , mutationFeature = "enums"
      , mutationPHPRule = "Enum cannot include properties"
      , mutationStance = Caught
      , mutationRewrite = at "    case Draft;" "    public int $weight = 0;\n\n    case Draft;"
      }
  , Mutation
      { mutationName = "value-on-unbacked-enum-case"
      , mutationFeature = "enums"
      , mutationPHPRule = "Case of non-backed enum must not have a value"
      , mutationStance = Caught
      , mutationRewrite = at "    case Draft;" "    case Draft = 1;"
      }
  ]

--------------------------------------------------------------------------------
-- Mutated programs
--------------------------------------------------------------------------------

-- | A valid program with one mutated snippet appended.
--
-- The mutated snippet is always last, so shrinking can strip the surrounding
-- context down to nothing without removing the construct under test.
data MutatedProgram = MutatedProgram
  { mutatedMutation :: Mutation
  , -- | 'False' when the rewrite could not find its anchor, which means the
    -- catalogue has drifted away from the mutation.
    mutatedApplied :: Bool
  , mutatedProgram :: PHPProgram
  }

renderMutated :: MutatedProgram -> Text
renderMutated = renderProgram . mutatedProgram

instance Show MutatedProgram where
  show m =
    unlines
      [ "mutation: " <> mutationName (mutatedMutation m)
      , "perturbs: " <> mutationFeature (mutatedMutation m)
      , "PHP rule: " <> mutationPHPRule (mutatedMutation m)
      , "applied: " <> show (mutatedApplied m)
      , "context features: "
          <> intercalate ", " (map snippetFeature (programSnippets (mutatedProgram m)))
      , "--- source ---"
      , T.unpack (renderMutated m)
      , "--- end source ---"
      ]

-- | A random mutation available in PHP @v@, applied to a random PHP @v@ program.
genMutatedProgram :: PHPVersion -> Gen MutatedProgram
genMutatedProgram v = do
  m <- elements (mutationsUpTo v)
  genMutationOf v m

-- | The same, with the mutation fixed. Used to prove every mutation is
-- reachable and to pin each one's stance.
genMutationOf :: PHPVersion -> Mutation -> Gen MutatedProgram
genMutationOf v m = do
  base <- genProgram v
  target <- maybe (pure Nothing) (fmap Just . renderFeature mutantIndex) (featureByName (mutationFeature m))
  pure $ case target >>= \s -> (,) s <$> mutationRewrite m (snippetText s) of
    Nothing -> MutatedProgram m False base
    Just (s, mutated) -> MutatedProgram m True (appendSnippet s {snippetText = mutated} base)
  where
    -- Well clear of the indices genProgram hands out, so the mutated snippet's
    -- declarations cannot collide with the context's.
    mutantIndex = 9000

-- | Shrink away the surrounding context only. The mutated snippet is the point
-- of the test case, so it is never dropped.
shrinkMutatedProgram :: MutatedProgram -> [MutatedProgram]
shrinkMutatedProgram m =
  [ m {mutatedProgram = p'}
  | mutatedApplied m
  , p' <- mapMaybe keepsMutant (shrinkProgram (mutatedProgram m))
  ]
  where
    mutant = last (programSnippets (mutatedProgram m))
    keepsMutant p
      | not (null (programSnippets p))
      , last (programSnippets p) == mutant =
          Just p
      | otherwise = Nothing
