{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.Gen.PHPSource
-- Description : QuickCheck generators for syntactically valid PHP source text
--
-- Generators of PHP source programs, indexed by the PHP version whose grammar
-- the program needs. Each 'Feature' is a self-contained chunk of top-level PHP
-- tagged with the earliest version that accepts it, so @featuresUpTo v@ is
-- exactly the syntax a PHP @v@ runtime understands.
--
-- The generated text is the input to the high-level compatibility properties in
-- "Test.CompatibilitySpec": for every version the library claims to support,
-- programs drawn from that version's grammar must parse, round-trip, and print
-- to a fixed point.
module Test.Gen.PHPSource
  ( -- * Versions
    PHPVersion (..)
  , versionLabel
  , allVersions

    -- * Features
  , Feature (..)
  , allFeatures
  , featuresUpTo
  , featuresIntroducedIn
  , featureByName
  , renderFeature

    -- * Programs
  , Snippet (..)
  , PHPProgram (..)
  , renderProgram
  , genProgram
  , shrinkProgram
  , appendSnippet

    -- * Imports
  ) where

import Data.List (find, intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

-- | The PHP versions this library claims to parse. 8.2 is the floor, so any
-- syntax older than 8.2 is tagged 'PHP82'.
data PHPVersion = PHP82 | PHP83 | PHP84 | PHP85
  deriving (Eq, Ord, Show, Enum, Bounded)

versionLabel :: PHPVersion -> String
versionLabel = \case
  PHP82 -> "8.2"
  PHP83 -> "8.3"
  PHP84 -> "8.4"
  PHP85 -> "8.5"

allVersions :: [PHPVersion]
allVersions = [minBound .. maxBound]

-- | A named chunk of top-level PHP, tagged with the earliest version that
-- accepts it. The 'Int' makes the declared names unique so several snippets can
-- be concatenated into one program without colliding.
data Feature = Feature
  { featureName :: String
  , featureSince :: PHPVersion
  , featureBody :: Int -> Gen Text
  }

-- | Features a PHP @v@ runtime accepts: everything introduced at or before @v@.
featuresUpTo :: PHPVersion -> [Feature]
featuresUpTo v = filter ((<= v) . featureSince) allFeatures

-- | Features introduced exactly at @v@. Every generated program is anchored on
-- one of these so a version's property actually exercises that version.
featuresIntroducedIn :: PHPVersion -> [Feature]
featuresIntroducedIn v = filter ((== v) . featureSince) allFeatures

-- | Look a feature up by name. The mutation layer in "Test.Gen.PHPMutation"
-- names the feature it perturbs, so a renamed or deleted feature surfaces as a
-- failing mutation rather than as silently skipped coverage.
featureByName :: String -> Maybe Feature
featureByName n = find ((== n) . featureName) allFeatures

-- | Render one feature at the given uniquifying index.
renderFeature :: Int -> Feature -> Gen Snippet
renderFeature i f = Snippet (featureName f) (featureSince f) <$> featureBody f i

-- | One rendered feature.
data Snippet = Snippet
  { snippetFeature :: String
  , snippetSince :: PHPVersion
  , snippetText :: Text
  }
  deriving (Eq, Show)

-- | A whole generated file: an optional @declare@, an optional namespace and
-- imports, then the feature snippets.
data PHPProgram = PHPProgram
  { programVersion :: PHPVersion
  , programStrict :: Bool
  , programNamespace :: Maybe Text
  , programImports :: [Text]
  , programSnippets :: [Snippet]
  }
  deriving (Eq)

-- | Counterexamples print the feature list and the full source, so a failure
-- names the construct that broke as well as showing the program.
instance Show PHPProgram where
  show p =
    unlines
      [ "PHP " <> versionLabel (programVersion p) <> " program"
      , "features: " <> intercalate ", " (map snippetFeature (programSnippets p))
      , "--- source ---"
      , T.unpack (renderProgram p)
      , "--- end source ---"
      ]

renderProgram :: PHPProgram -> Text
renderProgram p =
  T.concat
    [ "<?php\n\n"
    , if programStrict p then "declare(strict_types=1);\n\n" else ""
    , maybe "" (\ns -> "namespace " <> ns <> ";\n\n") (programNamespace p)
    , if null (programImports p) then "" else T.unlines (programImports p) <> "\n"
    , T.intercalate "\n\n" (map snippetText (programSnippets p))
    , "\n"
    ]

-- | The @use@ statements a program may draw from, each paired with the names it
-- binds in the current namespace. Two statements binding the same name are a
-- compile error in PHP ("Cannot use ... because the name is already in use"),
-- which a parser with no import table cannot see, so the collision is removed
-- here rather than left for the differential oracle to trip over.
--
-- Function and constant imports live in separate symbol tables from class
-- imports, so their bound names are tagged to keep them from colliding with a
-- class import of the same spelling.
importCandidates :: [([Text], Text)]
importCandidates =
  [ (["Collection"], "use App\\Support\\Collection;")
  , (["A"], "use App\\Support\\Arr as A;")
  , (["Str", "Num"], "use App\\Support\\{Str, Num};")
  , (["Text", "function:slug", "const:VERSION"], "use App\\Support\\{Text, function slug, const VERSION};")
  , (["function:collect"], "use function App\\Support\\collect;")
  , (["const:MAX_DEPTH"], "use const App\\Support\\MAX_DEPTH;")
  ]

-- | Keep the first statement binding any given name and drop later collisions.
dedupeImports :: [([Text], Text)] -> [Text]
dedupeImports = go Set.empty
  where
    go :: Set Text -> [([Text], Text)] -> [Text]
    go _ [] = []
    go seen ((names, stmt) : rest)
      | any (`Set.member` seen) names = go seen rest
      | otherwise = stmt : go (foldr Set.insert seen names) rest

genProgram :: PHPVersion -> Gen PHPProgram
genProgram v = do
  strict <- arbitrary
  ns <- elements [Nothing, Just "App\\Generated", Just "Vendor\\App\\Generated"]
  imports <- dedupeImports <$> sublistOf importCandidates
  anchor <- elements (featuresIntroducedIn v)
  extraCount <- choose (0, 5)
  extras <- vectorOf extraCount (elements (featuresUpTo v))
  chosen <- shuffle (anchor : extras)
  snippets <- traverse (uncurry renderFeature) (zip [0 ..] chosen)
  pure (PHPProgram v strict ns imports snippets)

-- | Append a snippet to a program, keeping it last so shrinking can strip the
-- surrounding context without removing the snippet under test.
appendSnippet :: Snippet -> PHPProgram -> PHPProgram
appendSnippet s p = p {programSnippets = programSnippets p ++ [s]}

-- | Shrink by dropping the optional header parts and then whole snippets, so a
-- counterexample reduces to the smallest set of features that still fails.
shrinkProgram :: PHPProgram -> [PHPProgram]
shrinkProgram p =
  [p {programStrict = False} | programStrict p]
    ++ [p {programNamespace = Nothing} | Just _ <- [programNamespace p]]
    ++ [p {programImports = is} | is <- shrinkList (const []) (programImports p)]
    ++ [ p {programSnippets = ss}
       | ss <- shrinkList (const []) (programSnippets p)
       , not (null ss)
       ]

--------------------------------------------------------------------------------
-- Feature catalogue
--------------------------------------------------------------------------------

-- | Suffix that makes a snippet's declared names unique within a program.
sfx :: Int -> Text
sfx = T.pack . show

ls :: [Text] -> Text
ls = T.intercalate "\n"

-- | Compound assignment operators the parser is expected to accept.
compoundAssignOps :: [Text]
compoundAssignOps = ["+=", "-=", "*=", "**=", "/=", "%=", ".=", "&=", "|=", "^=", "<<=", ">>=", "??="]

-- | Visibility keywords accepted on class members.
visibilities :: [Text]
visibilities = ["public", "protected", "private"]

-- | Scalar type names usable as a parameter, return, property or constant type.
scalarTypes :: [Text]
scalarTypes = ["int", "string", "float", "bool", "array"]

-- | Every construct excluded below is recorded in @knownDivergences@
-- ("Test.Gen.PHPMutation") with both sides' decisions and its detectability, and
-- is exercised there. The exclusions stay here because a corpus containing them
-- would fail corpus health, which is the premise of every differential property.
allFeatures :: [Feature]
allFeatures =
  [ -- PHP 8.2 baseline: syntax the library's floor version already accepts.
    --
    -- @"$a[-1]"@ is rejected outright
    -- (<https://github.com/jonbaldie/php-parser/issues/235 #235>).
    Feature "echo-and-interpolation" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$name" <> n <> " = 'world';"
        , "$bag" <> n <> " = ['key' => 1, 'nested' => ['deep' => 2]];"
        , "echo \"hello {$name" <> n <> "}\";"
        , "echo \"value {$bag" <> n <> "['key']} and {$bag" <> n <> "['nested']['deep']}\";"
        , "echo \"unbraced $bag" <> n <> "[key]\";"
        , "echo 'single quoted', PHP_EOL;"
        ]
  , Feature "arithmetic-and-compound-assignment" PHP82 $ \i -> do
      let n = sfx i
      op <- elements compoundAssignOps
      pure $ ls
        [ "$acc" <> n <> " = 6 * 7 - (1 + 2) / 3 % 4;"
        , "$acc" <> n <> " " <> op <> " 2;"
        , "$acc" <> n <> " = $acc" <> n <> " ** 2;"
        , "$acc" <> n <> " = ($acc" <> n <> " << 1) | ($acc" <> n <> " >> 1) ^ ~$acc" <> n <> ";"
        , "$acc" <> n <> " = 'x' . $acc" <> n <> " + 1;"
        , "$acc" <> n <> "++;"
        , "--$acc" <> n <> ";"
        ]
  , -- @08@ and @09@ are absent: they are accepted today but rejected by PHP
    -- (<https://github.com/jonbaldie/php-parser/issues/241 #241>).
    Feature "numeric-literals" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$nums" <> n <> " = [0x1F, 0b1010, 0o17, 017, 1_000_000, 1.5e3, 0.5, 7];"
        , "$big" <> n <> " = 9_223_372_036_854_775_807;"
        ]
  , -- Bodies carry no escape sequences and the closer is indented no deeper
    -- than the body: escapes are re-emitted undecoded
    -- (<https://github.com/jonbaldie/php-parser/issues/236 #236>) and an
    -- over-indented closer is wrongly accepted
    -- (<https://github.com/jonbaldie/php-parser/issues/240 #240>).
    Feature "heredoc-and-nowdoc" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$who" <> n <> " = 'PHP';"
        , "$doc" <> n <> " = <<<TEXT"
        , "    interpolating heredoc for {$who" <> n <> "}"
        , "    second line"
        , "    TEXT;"
        , "$raw" <> n <> " = <<<'TEXT'"
        , "    nowdoc body, $notAVariable stays literal"
        , "    TEXT;"
        ]
  , Feature "control-flow" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$total" <> n <> " = 0;"
        , "for ($i" <> n <> " = 0; $i" <> n <> " < 10; $i" <> n <> "++) {"
        , "    if ($i" <> n <> " % 2 === 0) {"
        , "        $total" <> n <> " += $i" <> n <> ";"
        , "    } elseif ($i" <> n <> " === 7) {"
        , "        continue;"
        , "    } else {"
        , "        break;"
        , "    }"
        , "}"
        , "while ($total" <> n <> " > 100) {"
        , "    $total" <> n <> " = intdiv($total" <> n <> ", 2);"
        , "}"
        , "do {"
        , "    $total" <> n <> "++;"
        , "} while ($total" <> n <> " < 0);"
        , "switch ($total" <> n <> ") {"
        , "    case 0:"
        , "    case 1:"
        , "        echo 'low';"
        , "        break;"
        , "    default:"
        , "        echo 'high';"
        , "}"
        ]
  , Feature "alternative-syntax" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$rows" <> n <> " = [1, 2, 3];"
        , "if (count($rows" <> n <> ") > 0):"
        , "    echo 'some';"
        , "elseif (count($rows" <> n <> ") === 0):"
        , "    echo 'none';"
        , "else:"
        , "    echo 'other';"
        , "endif;"
        , "foreach ($rows" <> n <> " as $key" <> n <> " => $row" <> n <> "):"
        , "    echo $key" <> n <> ", $row" <> n <> ";"
        , "endforeach;"
        , "while (false):"
        , "    echo 'never';"
        , "endwhile;"
        ]
  , Feature "arrays-and-destructuring" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$rows" <> n <> " = [[1, 2], [3, 4]];"
        , "[$first" <> n <> ", $second" <> n <> "] = $rows" <> n <> "[0];"
        , "['a' => $alpha" <> n <> "] = ['a' => 1, 'b' => 2];"
        , "list($l" <> n <> ", list($m" <> n <> ", $r" <> n <> ")) = [1, [2, 3]];"
        , "$merged" <> n <> " = [...$rows" <> n <> "[0], ...$rows" <> n <> "[1], 5];"
        , "$assoc" <> n <> " = ['k' => 'v', ...['j' => 'w']];"
        , "foreach ($rows" <> n <> " as [$left" <> n <> ", $right" <> n <> "]) {"
        , "    echo $left" <> n <> " + $right" <> n <> ";"
        , "}"
        , "foreach ($rows" <> n <> " as &$byRef" <> n <> ") {"
        , "    $byRef" <> n <> " = null;"
        , "}"
        ]
  , -- Top-level functions have no class scope, so @self@ and @static@ are absent
    -- from every type position here: PHP rejects them at compile time with
    -- @Cannot use "static" when no class scope is active@. Their valid use is
    -- covered by the @classes@ feature, and their invalid use in parameter
    -- position by the mutation layer.
    Feature "functions" PHP82 $ \i -> do
      let n = sfx i
      ret <- elements (scalarTypes ++ ["int|float", "?string", "mixed", "iterable"])
      pty <- elements scalarTypes
      pure $ ls
        [ "function typed" <> n <> "(" <> pty <> " $a): " <> ret <> " {"
        , "    return typed" <> n <> "($a);"
        , "}"
        , "function compute" <> n <> "(int $a, ?string $b = null, int ...$rest): int|float {"
        , "    return $a + count($rest);"
        , "}"
        , "function &pick" <> n <> "(array &$xs): int {"
        , "    return $xs[0];"
        , "}"
        , "function nothing" <> n <> "(): void {"
        , "}"
        , "function halt" <> n <> "(): never {"
        , "    throw new \\RuntimeException('stop');"
        , "}"
        , "function mixedArgs" <> n <> "(mixed $a, iterable $b, callable $c, ?array $d = null): mixed {"
        , "    return $d;"
        , "}"
        ]
  , Feature "closures-and-arrow-functions" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$factor" <> n <> " = 3;"
        , "$scale" <> n <> " = function (int $x) use ($factor" <> n <> "): int {"
        , "    return $x * $factor" <> n <> ";"
        , "};"
        , "$bump" <> n <> " = function () use (&$factor" <> n <> "): void {"
        , "    $factor" <> n <> "++;"
        , "};"
        , "$short" <> n <> " = fn (int $x): int => $x * $factor" <> n <> ";"
        , "$stat" <> n <> " = static fn (int $x): int => $x;"
        , "$anon" <> n <> " = static function (): void {"
        , "};"
        ]
  , Feature "match-expression" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$score" <> n <> " = 42;"
        , "$label" <> n <> " = match (true) {"
        , "    $score" <> n <> " > 100, $score" <> n <> " > 1000 => 'huge',"
        , "    $score" <> n <> " > 10 => 'big',"
        , "    default => 'small',"
        , "};"
        , "$exact" <> n <> " = match ($score" <> n <> ") {"
        , "    42 => 'answer',"
        , "    default => 'other',"
        , "};"
        ]
  , Feature "enums" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "enum Suit" <> n <> ": string implements \\JsonSerializable {"
        , "    case Hearts = 'H';"
        , "    case Spades = 'S';"
        , ""
        , "    const Wild = self::Hearts;"
        , ""
        , "    public function color(): string {"
        , "        return match ($this) {"
        , "            Suit" <> n <> "::Hearts => 'red',"
        , "            Suit" <> n <> "::Spades => 'black',"
        , "        };"
        , "    }"
        , ""
        , "    public function jsonSerialize(): mixed {"
        , "        return $this->value;"
        , "    }"
        , "}"
        , "enum Status" <> n <> " {"
        , "    case Draft;"
        , "    case Live;"
        , "}"
        ]
  , Feature "classes" PHP82 $ \i -> do
      let n = sfx i
      vis <- elements visibilities
      pure $ ls
        [ "abstract class Base" <> n <> " {"
        , "    " <> vis <> " string $slot = '';"
        , "    public const VERSION = '1.0';"
        , "    protected static int $count = 0;"
        , "    public readonly string $id;"
        , "    private ?Base" <> n <> " $parent = null;"
        , ""
        , "    public function __construct(string $id) {"
        , "        $this->id = $id;"
        , "        static::$count++;"
        , "    }"
        , ""
        , "    abstract protected function describe(): string;"
        , ""
        , "    final public static function made(): int {"
        , "        return self::$count;"
        , "    }"
        , ""
        , "    public function __toString(): string {"
        , "        return $this->describe();"
        , "    }"
        , "}"
        , "final class Impl" <> n <> " extends Base" <> n <> " {"
        , "    public function __construct(string $id, private readonly array $tags = []) {"
        , "        parent::__construct($id);"
        , "    }"
        , ""
        , "    protected function describe(): string {"
        , "        return self::VERSION . ':' . $this->id;"
        , "    }"
        , ""
        , -- `static` and `self|static|null` are legal in return position but not
          -- in parameter position, where the library wrongly accepts them (the
          -- @KnownFalseAccept@ pins in @Test.Gen.PHPMutation@). The corpus
          -- therefore cannot carry them as parameter types, so it carries them
          -- here rather than losing the coverage altogether.
          "    public function itself(): static {"
        , "        return $this;"
        , "    }"
        , ""
        , "    public function sibling(): self|static|null {"
        , "        return $this->itself();"
        , "    }"
        , "}"
        ]
  , Feature "interfaces-and-traits" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "interface Describable" <> n <> " extends \\Stringable {"
        , "    public function describe(): string;"
        , "}"
        , "trait Greets" <> n <> " {"
        , "    public function greet(): string {"
        , "        return 'hi';"
        , "    }"
        , ""
        , "    abstract public function name(): string;"
        , "}"
        , "trait Waves" <> n <> " {"
        , "    public function greet(): string {"
        , "        return 'wave';"
        , "    }"
        , "}"
        , "class Person" <> n <> " implements Describable" <> n <> " {"
        , "    use Greets" <> n <> ", Waves" <> n <> " {"
        , "        Greets" <> n <> "::greet insteadof Waves" <> n <> ";"
        , "        Waves" <> n <> "::greet as wave;"
        , "        Greets" <> n <> "::greet as protected politely;"
        , "    }"
        , ""
        , "    public function name(): string {"
        , "        return 'anon';"
        , "    }"
        , ""
        , "    public function describe(): string {"
        , "        return $this->greet();"
        , "    }"
        , ""
        , "    public function __toString(): string {"
        , "        return $this->describe();"
        , "    }"
        , "}"
        ]
  , Feature "exceptions" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "try {"
        , "    throw new \\InvalidArgumentException('bad');"
        , "} catch (\\InvalidArgumentException | \\TypeError $e" <> n <> ") {"
        , "    echo $e" <> n <> "->getMessage();"
        , "} catch (\\Throwable) {"
        , "    echo 'unknown';"
        , "} finally {"
        , "    echo 'done';"
        , "}"
        , "$thrower" <> n <> " = fn (): never => throw new \\LogicException('expr');"
        ]
  , Feature "generators" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "function stream" <> n <> "(): \\Generator {"
        , "    $received" <> n <> " = yield 1;"
        , "    yield 'key' => 2;"
        , "    yield from [3, 4];"
        , "    return $received" <> n <> ";"
        , "}"
        ]
  , Feature "static-and-global-declarations" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "function counter" <> n <> "(): int {"
        , "    static $calls" <> n <> " = 0;"
        , "    global $registry" <> n <> ";"
        , "    $calls" <> n <> "++;"
        , "    return $calls" <> n <> ";"
        , "}"
        ]
  , Feature "isset-unset-empty" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$bag" <> n <> " = ['a' => 1];"
        , "if (isset($bag" <> n <> "['a'], $bag" <> n <> "['b']) && !empty($bag" <> n <> ")) {"
        , "    unset($bag" <> n <> "['a']);"
        , "}"
        , "$exists" <> n <> " = isset($undefined" <> n <> ") ? 'yes' : 'no';"
        ]
  , Feature "named-args-nullsafe-first-class-callables" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$len" <> n <> " = strlen(...);"
        , "$when" <> n <> " = new \\DateTimeImmutable('now');"
        , "$formatted" <> n <> " = $when" <> n <> "?->format(format: 'Y-m-d');"
        , "$bound" <> n <> " = $when" <> n <> "->format(...);"
        , "$static" <> n <> " = \\DateTimeImmutable::createFromFormat(...);"
        , "$chain" <> n <> " = $when" <> n <> "?->getTimezone()?->getName();"
        , "$spread" <> n <> " = max(...[1, 2, 3]);"
        , "$named" <> n <> " = str_pad(string: 'x', length: 3, pad_string: '-');"
        ]
  , Feature "attributes" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "#[\\Attribute(\\Attribute::TARGET_ALL)]"
        , "class Marker" <> n <> " {"
        , "    public function __construct(public string $note = '') {"
        , "    }"
        , "}"
        , "#[Marker" <> n <> "(note: 'class')]"
        , "final class Marked" <> n <> " {"
        , "    #[Marker" <> n <> "('const')]"
        , "    public const X = 1;"
        , ""
        , "    #[Marker" <> n <> "('prop')]"
        , "    public int $count = 0;"
        , ""
        , "    #[Marker" <> n <> "('method'), Marker" <> n <> "('second')]"
        , "    #[Marker" <> n <> "('stacked')]"
        , "    public function run(#[Marker" <> n <> "('param')] int $a): void {"
        , "    }"
        , "}"
        ]
  , Feature "ternary-coalesce-spaceship" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$cfg" <> n <> " = ['debug' => null];"
        , "$debug" <> n <> " = $cfg" <> n <> "['debug'] ?? false;"
        , "$mode" <> n <> " = $debug" <> n <> " ? 'dev' : 'prod';"
        , "$short" <> n <> " = $debug" <> n <> " ?: 'prod';"
        , "$order" <> n <> " = 1 <=> 2;"
        , "$chain" <> n <> " = $cfg" <> n <> "['a'] ?? $cfg" <> n <> "['b'] ?? 'fallback';"
        , "$cfg" <> n <> "['debug'] ??= true;"
        ]
  , Feature "instanceof-clone-casts" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$obj" <> n <> " = new \\stdClass();"
        , "$isStd" <> n <> " = $obj" <> n <> " instanceof \\stdClass;"
        , "$copy" <> n <> " = clone $obj" <> n <> ";"
        , "$asArray" <> n <> " = (array) $obj" <> n <> ";"
        , "$asInt" <> n <> " = (int) '42';"
        , "$asBool" <> n <> " = (bool) 1;"
        , "$asFloat" <> n <> " = (float) '1.5';"
        , "$asString" <> n <> " = (string) 42;"
        , "$silenced" <> n <> " = @$undefined" <> n <> ";"
        ]
  , Feature "new-in-initializers" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "class Service" <> n <> " {"
        , "    public function __construct(private \\ArrayObject $items = new \\ArrayObject()) {"
        , "    }"
        , "}"
        , "function withDefault" <> n <> "(\\ArrayObject $items = new \\ArrayObject()): int {"
        , "    return count($items);"
        , "}"
        ]
  , Feature "goto-and-labels" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$tick" <> n <> " = 0;"
        , "start" <> n <> ":"
        , "$tick" <> n <> "++;"
        , "if ($tick" <> n <> " < 3) {"
        , "    goto start" <> n <> ";"
        , "}"
        ]
  , Feature "82-dnf-types" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "interface Counts" <> n <> " {"
        , "}"
        , "interface Walks" <> n <> " {"
        , "}"
        , "function process" <> n <> "((Counts" <> n <> "&Walks" <> n <> ")|null $input): (Counts" <> n <> "&Walks" <> n <> ")|false {"
        , "    return $input ?? false;"
        , "}"
        , "class Holder" <> n <> " {"
        , "    public (Counts" <> n <> "&Walks" <> n <> ")|null $value = null;"
        , ""
        , "    public function set((Counts" <> n <> "&Walks" <> n <> ")|null $value): void {"
        , "        $this->value = $value;"
        , "    }"
        , "}"
        ]
  , Feature "82-readonly-classes" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "readonly class Point" <> n <> " {"
        , "    public function __construct(public int $x = 0, public int $y = 0) {"
        , "    }"
        , "}"
        , "final readonly class Vector" <> n <> " extends Point" <> n <> " {"
        , "    public function __construct(public float $length = 0.0) {"
        , "        parent::__construct();"
        , "    }"
        , "}"
        ]
  , Feature "82-trait-constants" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "trait HasVersion" <> n <> " {"
        , "    const VERSION = '1.0';"
        , "    public const EDITION = 'community';"
        , "    final const BUILD = 7;"
        , "    protected const INTERNAL = [1, 2];"
        , "}"
        ]
  , Feature "82-standalone-types" PHP82 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "function alwaysNull" <> n <> "(): null {"
        , "    return null;"
        , "}"
        , "function alwaysTrue" <> n <> "(mixed $value): true {"
        , "    return true;"
        , "}"
        , "function alwaysFalse" <> n <> "(mixed $value): false {"
        , "    return false;"
        , "}"
        , "class Flag" <> n <> " {"
        , "    public false $off = false;"
        , "    public true $on = true;"
        , "}"
        ]
  , -- PHP 8.3
    Feature "83-typed-class-constants" PHP83 $ \i -> do
      let n = sfx i
      vis <- elements visibilities
      cty <- elements scalarTypes
      pure $ ls
        [ "class Config" <> n <> " {"
        , "    " <> vis <> " const " <> cty <> "|null VARIED = null;"
        , "    public const string APP_ENV = 'prod';"
        , "    final public const int MAX_LIMIT = 100;"
        , "    protected const array DEFAULTS = [];"
        , "    private const ?string OPTIONAL = null;"
        , "}"
        , "interface HasLimit" <> n <> " {"
        , "    const int LIMIT = 10;"
        , "}"
        , "trait Limited" <> n <> " {"
        , "    const float RATIO = 0.5;"
        , "}"
        , "enum Level" <> n <> ": int {"
        , "    const string LABEL = 'level';"
        , "    case Low = 1;"
        , "}"
        ]
  , Feature "83-dynamic-class-constant-fetch" PHP83 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "class Settings" <> n <> " {"
        , "    public const APP_ENV = 'prod';"
        , "    public const APP_KEY = 'secret';"
        , "}"
        , "$which" <> n <> " = 'APP_ENV';"
        , "$prefix" <> n <> " = 'APP';"
        , "$direct" <> n <> " = Settings" <> n <> "::{$which" <> n <> "};"
        , "$computed" <> n <> " = Settings" <> n <> "::{$prefix" <> n <> " . '_KEY'};"
        ]
  , Feature "83-anonymous-readonly-classes" PHP83 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "interface Pings" <> n <> " {"
        , "    public function ping(): string;"
        , "}"
        , "$simple" <> n <> " = new readonly class {"
        , "    public function ping(): string {"
        , "        return 'pong';"
        , "    }"
        , "};"
        , "$configured" <> n <> " = new readonly class(3) implements Pings" <> n <> " {"
        , "    public function __construct(public int $depth = 1) {"
        , "    }"
        , ""
        , "    public function ping(): string {"
        , "        return 'pong' . $this->depth;"
        , "    }"
        , "};"
        ]
  , -- PHP 8.4
    Feature "84-property-hooks" PHP84 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "class Temperature" <> n <> " {"
        , "    private float $raw = 0.0;"
        , ""
        , "    public float $celsius {"
        , "        get => $this->raw;"
        , "        set(float $value) {"
        , "            $this->raw = $value;"
        , "        }"
        , "    }"
        , ""
        , "    public string $label {"
        , "        get {"
        , "            return sprintf('%.1f', $this->raw);"
        , "        }"
        , "    }"
        , ""
        , "    public int $rounded {"
        , "        set => (int) $value;"
        , "    }"
        , "}"
        , "interface HasName" <> n <> " {"
        , "    public string $name { get; set; }"
        , "}"
        , "abstract class Named" <> n <> " {"
        , "    abstract public string $name { get; }"
        , "}"
        ]
  , Feature "84-asymmetric-visibility" PHP84 $ \i -> do
      let n = sfx i
      (getVis, setVis) <- elements
        [("public", "private"), ("public", "protected"), ("protected", "private")]
      pure $ ls
        [ "class Account" <> n <> " {"
        , "    " <> getVis <> " " <> setVis <> "(set) string $varied = '';"
        , "    public private(set) string $id = '';"
        , "    public protected(set) int $version = 0;"
        , "    protected private(set) array $audit = [];"
        , ""
        , "    public function __construct(public private(set) readonly string $owner = '') {"
        , "    }"
        , "}"
        ]
  , Feature "84-new-without-parentheses" PHP84 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "class Service" <> n <> " {"
        , "    public const KEY = 'k';"
        , "    public array $items = [];"
        , ""
        , "    public function process(): string {"
        , "        return 'ok';"
        , "    }"
        , "}"
        , "$result" <> n <> " = new Service" <> n <> "()->process();"
        , "$key" <> n <> " = new Service" <> n <> "()::KEY;"
        , "$items" <> n <> " = new Service" <> n <> "()->items;"
        ]
  , -- PHP 8.5
    Feature "85-pipe-operator" PHP85 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$trimmed" <> n <> " = '  Hello  ' |> 'trim' |> 'strtolower';"
        , "$summed" <> n <> " = [1, 2, 3] |> array_sum(...);"
        , "$doubled" <> n <> " = 21 |> (fn (int $x): int => $x * 2);"
        , "$piped" <> n <> " = ['a', 'b'] |> array_reverse(...) |> (fn (array $xs): string => implode(',', $xs));"
        ]
  , Feature "85-clone-with" PHP85 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "$base" <> n <> " = new \\stdClass();"
        , "$plain" <> n <> " = clone $base" <> n <> ";"
        , "$positional" <> n <> " = clone($base" <> n <> ", ['status' => 'archived']);"
        , "$named" <> n <> " = clone($base" <> n <> ", with: ['updated' => true]);"
        ]
  , Feature "85-static-asymmetric-visibility" PHP85 $ \i -> do
      let n = sfx i
      pure $ ls
        [ "class Registry" <> n <> " {"
        , "    public private(set) static array $entries = [];"
        , "    public protected(set) static ?string $current = null;"
        , "}"
        ]
  ]
