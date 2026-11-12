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

## Cursor Cloud specific instructions

GHC 9.12.1 and Cabal 3.16.1.0 are on `PATH`. PHP 8.2 through 8.5 are installed as `php8.2`, `php8.3`, `php8.4`, and `php8.5`. This is a library; nothing has to be started before `cabal test`.
