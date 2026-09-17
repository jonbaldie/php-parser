# The differential PHP oracle

The suite normally checks the library against its own documented invariants: a
program parses, printing and reparsing preserves the AST, printing reaches a
fixed point. None of that can tell you whether the library agrees with *PHP*.

The oracle group can. It puts the same source in front of a real `php` binary
and compares decisions. `php -l` is the query, and it is stronger than its name
suggests: it performs a full compile without executing, so it reports
declaration-local semantic rules as well as grammar errors.

The group lives in `test/Test/OracleSpec.hs`; the interpreter itself is hidden
behind `Test.Oracle.PHP`.

## Running it locally

**Nothing is required.** With no interpreter on `PATH` every differential
property passes trivially, and every group that needed an interpreter says so in
its own name:

```
PHP interpreter oracle (differential) [SKIPPED: no PHP interpreter found -- see docs/testing/php-oracle.md]
  ...
    PHP 8.2 [SKIPPED: no PHP 8.2 binary found (set PHP82_BIN, or put one of php8.2 php82 php on PATH)]
```

Discovery therefore runs *before* the test tree is built, so that the names can
carry the reason. That is the normal local outcome, and it is why a fresh
checkout does not need PHP installed to run `cabal test` — but a skipped oracle
can never be mistaken for a passing one.

To actually exercise it, put one or more interpreters where the oracle can find
them. For each version it tries, in order:

1. the per-version override — `PHP82_BIN`, `PHP83_BIN`, `PHP84_BIN`, `PHP85_BIN`;
2. `php8.2`, `php82`, then a bare `php`, on `PATH`.

Discovery **verifies rather than trusts**: a binary called `php8.3` that reports
8.4 is rejected for the 8.3 slot. A mislabelled binary cannot make a version
group pass vacuously.

```sh
# whatever you have
cabal test all --test-options='--pattern "/PHP interpreter oracle/"'

# pointing at a specific build
PHP85_BIN=/opt/homebrew/opt/php/bin/php \
  cabal test all --test-options='--pattern "/PHP interpreter oracle/"'
```

Set `PHP_ORACLE_REQUIRED=1` to turn a missing interpreter into a failure instead
of a skip. CI sets it, so the oracle job cannot pass by quietly testing nothing.

### Installing all four interpreters

macOS, via Homebrew. Core Homebrew's `php` is the current release (8.5); the
older versions come from the `shivammathur/php` tap:

```sh
brew install php
brew tap shivammathur/php
brew install shivammathur/php/php@8.2 \
             shivammathur/php/php@8.3 \
             shivammathur/php/php@8.4
```

Only one of those can own the `php` name at a time, so point the overrides at
each prefix rather than trying to link four binaries into one `PATH`:

```sh
export PHP82_BIN=$(brew --prefix php@8.2)/bin/php
export PHP83_BIN=$(brew --prefix php@8.3)/bin/php
export PHP84_BIN=$(brew --prefix php@8.4)/bin/php
export PHP85_BIN=$(brew --prefix php)/bin/php
```

Debian and Ubuntu, via the `ondrej/php` PPA, which installs `php8.2` ... `php8.5`
side by side under those names — no overrides needed:

```sh
sudo add-apt-repository ppa:ondrej/php
sudo apt-get update
sudo apt-get install php8.2-cli php8.3-cli php8.4-cli php8.5-cli
```

CI takes a third route: `shivammathur/setup-php` once per version, which leaves
the same versioned binaries behind. See the `php-oracle` job in
`.github/workflows/ci.yml`.

## What the group checks

Four properties, in two directions.

| Property | Corpus | Claim |
| --- | --- | --- |
| Corpus health | valid, per version | a PHP *v* interpreter accepts every program the generator calls valid for *v* |
| No false rejects | valid, per version | PHP accepts it ⟹ the library parses it |
| No false accepts | mutated, version-agnostic | the library parses it ⟹ *some* supported version accepts it |
| Printer output is PHP | valid, per version | printing a parsed program yields source the interpreter still accepts |

Corpus health is the one that stops the others being decoration. Without it, a
generator that quietly narrowed to `<?php` would make every other property pass.

**No false accepts is version-agnostic on purpose.** `parseProgram` has no target
version — it parses the union of 8.2 through 8.5 — so it is only wrong to accept
something *no* supported version accepts. That property therefore runs only when
all four interpreters are present; "no version accepts this" cannot be
established from a subset.

## Why there is a mutation layer

On a corpus of valid programs only, all four properties collapse into the same
assertion — everything is accepted by everybody — and the rejection half of the
contract goes untested.

`Test.Gen.PHPMutation` supplies programs worth rejecting. A mutation is a small,
targeted perturbation of a snippet the feature catalogue already renders: a
return-only type moved into parameter position, a duplicated member name, a
modifier the grammar forbids, a default on a variadic. Each is something a PHP
programmer might plausibly write, and each is something PHP rejects.

Mutations are derived from the catalogue rather than written as standalone
programs, so a near-miss inherits whatever the catalogue currently emits. If the
catalogue drifts and a rewrite stops finding its anchor, `mutatedApplied`
reports it and the suite fails, rather than silently generating valid programs.

## The known-divergence table

Expected disagreements live in one place — `Test.Gen.PHPMutation` — and in one
test group, in two halves.

### Catalogue mutations

Every mutation records what the library *currently* does with it:

- `Caught` — the library rejects it, like PHP.
- `KnownFalseAccept` — the library accepts it, and here is why that is known.

Both are asserted. A `Caught` mutation that stops being caught is a regression.
A `KnownFalseAccept` that starts being caught is a **bug fix**, and it also fails
this suite — deliberately, so the fix and the updated pin land in the same
change. Neither direction can drift unnoticed.

The current false accepts are all parameter-position and duplicate-member rules:
`static`, `self|static|null`, `never`, `void` and `?mixed` as parameter types; a
default on a variadic; `final private const`; and a class constant, property or
method declared twice in one class.

### Constructs excluded from the generator

`Test.Gen.PHPSource` leaves seven constructs out of the valid corpus, because
including them would fail corpus health — which is the premise of every other
property. They are not merely commented out: `knownDivergences` carries each one
as data, with the issue it belongs to, a self-contained program, **both sides'
decisions**, and whether a lint oracle can see the difference at all.

| Issue | Construct | PHP | Library | Gated by this oracle? |
| --- | --- | --- | --- | --- |
| #235 | `"$a[-1]"` | accepts | rejects | **yes** — false reject |
| #237 | `**=`, `<<=`, `>>=` | accepts | rejects | **yes** — false reject |
| #240 | heredoc closer indented deeper than its body | rejects | accepts | **yes** — false accept |
| #241 | `08`, `09` | rejects | accepts | **yes** — false accept |
| #232 | `.` against `+`/`-` precedence | accepts | accepts | no |
| #233 | unbraced `"$a[key]"` | accepts | accepts | no |
| #236 | escape sequences in a heredoc body | accepts | accepts | no |

Every decision in that table was measured against a real interpreter and the
library, not assumed. The measurement corrected the spec, which had recorded
#237 as invisible to a verdict oracle; it is not — PHP accepts `$a **= 2;` and
the library rejects it, so the oracle gates it.

The last three rows are cases where **both sides accept** and only the meaning
differs. No exit status can distinguish them, so their entries are records
rather than gates, and their test names say `[recorded only: no exit status can
see this]`. Catching them needs an execution oracle, which is #245.

Both halves of every entry are checked. The library's half needs no interpreter
and always runs; the interpreter's half skips visibly when none is present. When
one of these bugs is fixed, the entry fails, which forces the table and the
generator's exclusion to be updated in the same change as the fix.

## The contract boundary

`php -l` rejects in three tiers:

1. **grammar** — in contract;
2. **rules decidable from a single declaration** — in contract; this is what the
   declaration-validation work implemented;
3. **rules needing a program-wide symbol table** — out of contract, because this
   is a parser and maintains none.

Only tier 3 is excluded, by `outOfContractRules` in `Test.Oracle.PHP`. That list
is the most dangerous artifact in the suite: anything on it disappears from the
test's attention. It is kept in one place, kept short, and every entry carries
its justification. Adding an entry is a design change, not a fix.

Duplicate members *within* one class are deliberately absent from it: they need a
per-declaration member table, not a program-wide one, so they are in contract —
and, as the table above records, currently accepted.

## Cost, and the budget

One subprocess per generated program, memoised on the source text. A single
`php -n -d display_errors=1 -l` over stdin measures a median of 43 ms on the
machine below, so the cost is process spawn, not parsing.

**The budget is 10 minutes of wall clock for the whole oracle group at default
settings with all four interpreters present.** Exceeding it is a defect in this
suite, not an acceptable cost: the group has to stay runnable in a CI job and,
for anyone who installs the interpreters, in a local loop.

Measured against that budget on an M-series laptop, GHC 9.12.1, default settings
(50 QuickCheck tests per property):

| Interpreters resolved | Oracle group | Whole suite |
| --- | --- | --- |
| 1 (8.5) | 82 s | 91 s (469 tests) |
| 0 (everything skips) | 0.19 s | — |

The per-version properties are the bulk of the cost and scale with the number of
interpreters resolved, which is where the four-interpreter budget comes from:
four times the measured interpreter-bound work, plus the no-false-accepts
property, which only runs with all four and lints each program four times. CI is
where the four-interpreter figure is actually observed; the `php-oracle` job
prints its own duration. If it lands above 10 minutes, cut the work rather than
raising the number.

This is also why the group is a separate CI job rather than part of the GHC
matrix — the PHP matrix stays orthogonal to the compiler matrix instead of
multiplying against it. `--quickcheck-tests=N` raises the count when you want a
deep fuzz, and deliberately leaves the budget behind.
