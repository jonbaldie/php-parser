## Agent skills

### Issue tracker

Issues and PRDs are tracked in GitHub Issues. See `docs/agents/issue-tracker.md`.

### Triage labels

Use the five default canonical triage labels. See `docs/agents/triage-labels.md`.

### Domain docs

Use the single-context domain-doc layout. See `docs/agents/domain.md`.

### Differential PHP oracle

The test suite can check the parser against a real `php` binary. Setup, what it
checks, and the known-divergence table are in `docs/testing/php-oracle.md`.

### Exploratory testing

Past exploratory passes and their findings are indexed in `docs/exploratory-testing/README.md`.

### Enforced rules

| Rule | Enforcement |
| --- | --- |
| Parser lookahead must restore lexer state. | `Language.PHP.Parser.Megaparsec` provides the safe operation; `bash scripts/check-parser-lookahead.sh` checks imports and historical failures in CI. |
| Query APIs share child structure; exact traversal and pruning differ only in their recursive callbacks. | `Fold.hs` routes both query engines through `exprChildren` and `stmtChildren`; the shared-query cases in `RecursionSchemesSpec` check historical binding and interpolation omissions. |
