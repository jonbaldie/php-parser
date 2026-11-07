# Exploratory testing setup blocker, 2026-11-07

## Starting state

The selected repository was `jonbaldie/php-parser`. The worktree was based on
`origin/main` at `0ffd609b383336e2bda3e33512726b14a056e7b8`, version `0.1.15.0`.
The run requested a lockfile-based, frozen or locked dependency install, with
mutable dependencies and build outputs inside the worktree.

No GHC or Cabal executable was available on `PATH`. A search under
`/home/ubuntu` and `/opt` to depth five found no executable named `ghc` or
`cabal`. The tracked tree contained no `cabal.project.freeze`, Stack lockfile, or
other file ending in `.lock`. `php-parser.cabal` specifies version ranges, not a
locked dependency set. The repository's CI runs `cabal update` and an unlocked
`cabal build`. That command would not meet this run's install constraint.
No dependencies were installed and no library build was attempted.

The maintainer or operator can unblock a comparable run by providing a supported
Haskell toolchain and a project dependency lockfile with a frozen install
procedure, or by authorizing a run without the lockfile requirement. A compiler
alone would not resolve the missing lockfile constraint.

## Journeys

None of the three planned public API journeys could be executed. They remain
unexplored, not passed or failed.

1. Parse a PHP program with `parseProgram`, print it with `prettyPrint`, and
   reparse the output. Repeat with a program that includes comments.
2. Parse an expression with `parseExpression` and inspect its span. Repeat with
   malformed input to inspect the diagnostic and a corrected expression.
3. Parse a program and query its expressions or variables with the public fold
   API. Repeat with a nested expression and compare the returned values.

All three need a runnable library and a consumer program. The run did not infer
library behavior from source inspection or from the PHP interpreter.

## Environment checks

The host has PHP 8.5.4. These commands tested the interpreter, not the library:

```text
$ php -n -r 'echo PHP_VERSION, PHP_EOL;'
8.5.4
$ printf '<?php echo "hello";\n' | php -n -l
No syntax errors detected in Standard input code
$ printf '<?php echo ;\n' | php -n -l
Parse error: syntax error, unexpected token ";" in Standard input code on line 1
Errors parsing Standard input code
```

The first fixture was accepted and the second rejected. Neither result
establishes a parser finding. PHP 8.2 through 8.4 were not exercised.

## Findings and limitations

No product bug was confirmed or filed. There were no library observations to
replay. The setup limitation is not evidence of a defect in the parser. Existing
issues include [#320](https://github.com/jonbaldie/php-parser/issues/320) for
comment ownership and [#303](https://github.com/jonbaldie/php-parser/issues/303)
for lazy text entry points; neither was re-tested in this run.

No service, browser, container, image, volume, or background process was started
for the pass. The worktree was registered for removal after the report was
published. The checkout was otherwise left unchanged.
