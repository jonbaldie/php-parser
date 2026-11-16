# Parser state refactor review

Source reviewed: `948019495d320c95a7554344bcd32058a7e2f1ce`.
Baseline: `1c1586e3d46503a97a499b194a6dd7f2b359509b`.

[Protocol dispositions and reproduction commands](../design/parser-state-machines.md)
cover all 24 production and four supporting inventory entries. Existing grammar,
AST, context, cache and wizard representations are retained where already explicit.
The [decision trail](parser-state-decisions.tsv) is a sanitized snapshot of the
append-only working log; its evidence pointers identify repository artifacts.
Later hosted results and evidence attachments belong to [PR #420](https://github.com/jonbaldie/php-parser/pull/420).

## Local verification

- Warnings-as-errors build and Haddock generation passed with GHC 9.12.1.
- All 729 non-oracle tests and 71 oracle-group tests passed with replay seed 417.
  The local oracle exercised PHP 8.4; unavailable versions were skipped locally.
  Hosted CI requires real PHP 8.2, 8.3, 8.4 and 8.5.
- All 11 protocol runners passed. The session comparison covers 420,715 event
  prefixes through depth six. The 51 trivia checks now pass, correcting 32 failures
  reproduced before the lookahead fix.
- The final source matched the baseline exactly on 880 programs and 459 expressions:
  acceptance, annotated ASTs, printer output and diagnostics. This corpus includes
  comments but does not cover the specific formerly duplicated trivia positions.

These are finite checks, not formal proofs or a claim of complete PHP equivalence.
The dedicated trivia cases establish the intended comment correction separately.

## Independent review

GPT-6.1 Terra reviewed two competing designs and selected local domain types with
atomic namespace and close-tag transitions. It later reviewed the final source,
lookahead call sites, all inventory dispositions and the CI scripts. Its initial
review caught missing compiler extensions in the direct transition runner and an
incorrect description of the corpus as comment-free. Both were corrected.

The final source review found no material actionable defect and independently ran
the transition script successfully over 420,715 prefixes. It did not independently
rerun the full suite, oracle or behavior corpus. A separate Terra trail review
required final-source equivalence evidence and explicit compatibility disclosure;
the final-source comparison above passed after that review.

Terra satisfies the requested non-Astra review. It shares the GPT model family
with the implementer; a different-family trail reviewer was unavailable.

## Compatibility decision

Making the exposed low-level `LexerState` opaque removes its constructor, fields
and split namespace/close-tag helpers. Custom Lexer clients must migrate to the
semantic operations. No downstream compatibility audit was performed. The normal
parse and print signatures stay unchanged. This source break is disclosed in the
changelog and PR; it is a deliberate design tradeoff, not a compatibility guarantee.
