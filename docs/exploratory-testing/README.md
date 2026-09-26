# Exploratory testing reports

Each report records an exploratory pass over the public API: the journeys attempted,
the confirmed findings (filed as GitHub issues), candidates rejected with evidence,
and the limitations of that pass. Reports are named `YYYY-MM-DD-<scope>.md`.

| Report | Scope | Library | Confirmed findings |
| ------ | ----- | ------- | ------------------ |
| [2026-09-16-declarations-and-clauses.md](2026-09-16-declarations-and-clauses.md) | Declarations and clauses | `0.1.5.0` | see report |
| [2026-09-16-grammar-traversals.md](2026-09-16-grammar-traversals.md) | Modern grammar and traversal schemes | `0.1.5.0` | [#186](https://github.com/jonbaldie/php-parser/issues/186)–[#191](https://github.com/jonbaldie/php-parser/issues/191) |
| [2026-09-16-types-attributes-destructuring.md](2026-09-16-types-attributes-destructuring.md) | Types, attributes, destructuring | `0.1.5.0` | see report |
| [2026-09-17-precedence-interpolation-heredocs.md](2026-09-17-precedence-interpolation-heredocs.md) | Precedence, interpolation, heredocs, trivia | `0.1.6.0` | [#232](https://github.com/jonbaldie/php-parser/issues/232)–[#241](https://github.com/jonbaldie/php-parser/issues/241) |
| [2026-09-21-cgpt-differential.md](2026-09-21-cgpt-differential.md) | Coverage-guided differential (file shell, declares, open tags) | `0.1.7.0` | [#271](https://github.com/jonbaldie/php-parser/issues/271)–[#283](https://github.com/jonbaldie/php-parser/issues/283) |
| [2026-09-26-consumer-journeys.md](2026-09-26-consumer-journeys.md) | Index, format-and-run, fragment repair | `0.1.8.0` | [#304](https://github.com/jonbaldie/php-parser/issues/304)–[#309](https://github.com/jonbaldie/php-parser/issues/309) |

Evidence (drivers, case corpora, replay transcripts) is kept locally under
`exploratory-evidence/<date>-<scope>/` and is not committed.
