---
name: verify-roundtrip
description: Check bounded PHP expression parser and pretty-printer round-trips. Use when changing expression syntax, operator precedence, literals, types, or expression printing.
---

# Verify expression round-trips

For every generated source expression that `parseExpression` accepts, printing its annotation-free AST and reparsing the result must produce the same AST. Otherwise, a formatter or AST consumer can change a program's meaning without a parse error.

## Run

From the repository root, run:

```bash
bash .agents/skills/verify-roundtrip/check.sh
```

The script builds the local library and runs the source generator against it. The check was run with GHC 9.12.1 and cabal-install 3.16.1.0. Install those versions and the dependencies in `php-parser.cabal` to reproduce that environment. Cabal dependency versions are not frozen by this skill.

The summary reports generated, accepted, rejected, and failing cases. When the checker runs, exit 0 means all accepted cases passed, exit 1 means a round-trip failure, and exit 2 means a required group had no accepted input or the options were invalid. Build failures also return nonzero; read the output to distinguish them.

For all counterexamples, add `--all-failures`. For one rejected input and the rejection count per group, add `--show-rejections`. For every rejected input, add `--all-rejections`. To run one group, add `--group 'binary/+'` or use another exact group name from the output. A group pass covers only that group. A rejected input is not a round-trip pass. Compare acceptance and rejection counts with the baseline below and investigate changes. The check requires each seed, binary operator, and context group to have an accepted input, but it does not require every generated pair to be accepted.

## Interpret a failure

The input in a failure is PHP source generated without `prettyPrintExpr`. The checker first parses that entire input, strips annotations, prints the AST, and reparses the printed source. A failure reports the input and printed text, plus either the parse error or both ASTs. Reproduce the source with `parseExpression` before changing the generator. If the source is accepted and the AST changes, treat it as a product finding. Do not filter it out merely to make the check pass.

At sync commit `51db30ad31f3a15081c92ec7d069d80e00c55524`, GHC 9.12.1 produced 13,859 cases. The parser accepted 13,566 and rejected 293. One accepted case failed. `yield (yield 42) => $y` prints as `yield yield 42 => $y`. The original AST yields `$y` with a nested yield as its key. The printed source instead nests the keyed yield in the outer yield's value. The check exits 1 until that finding is fixed.

## Scope and maintenance

`harness/ExhaustiveRoundTrip.hs` defines the finite source alphabet and the bounds. It checks every listed binary operator against each pair of listed atoms. It also puts each listed seed in every listed context, including both operand slots of every listed binary operator. It checks only combinations accepted by the parser. Some combinations are intentionally invalid, such as incrementing a literal. The generator is not an exhaustive enumeration of PHP, expression ASTs, identifiers, nesting depths, or precedence trees.

The comparison removes annotations, so spans and comments are outside the property. It does not check PHP runtime semantics, PHP's acceptance of input, statement or program round-trips, or whether the parser rejects invalid source. This is bounded testing of real Haskell code, not a proof for unbounded inputs.

The coverage record at sync commit `51db30ad31f3a15081c92ec7d069d80e00c55524` is:

- `src/Language/PHP/Parser.hs`, `Parser/Statement.hs`, `Parser/Expression.hs`, `Parser/Internal.hs`, `Parser/Type.hs`, `Parser/Name.hs`, and `Parser/Lexer.hs` provide `parseExpression` and its grammar.
- `src/Language/PHP/Pretty.hs` provides `prettyPrintExpr` and the expression, literal, and type renderers it calls.
- `src/Language/PHP/AST.hs`, `Fold.hs`, and `Span.hs` define the compared AST and annotation removal.
- `php-parser.cabal` defines the library dependencies and exposed modules.

When any covered code changes after the sync commit, inspect the diff and update the generator, expected coverage, or property as appropriate. Run the check and inspect every new failure and acceptance change. In a disposable worktree, run the check with `--group 'binary/+'` and confirm it passes. Change `prettyBinOp` for `OpAdd` from `"+"` to `"-"`, rerun that group, and confirm that an addition source produces an AST mismatch. Remove the mutation, rerun the group, and run the full check. Move the sync commit forward only after confirming that the check describes the covered code at that commit. Use `/maintain-formal-verification-skill` when the verification has fallen behind.
