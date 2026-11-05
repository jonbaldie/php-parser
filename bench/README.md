# Query allocation check

`QueryAllocation.hs` measures cumulative allocation while each query builds its
complete result. It constructs the left-deep expression before the measurement,
checks the result length, and excludes parsing. The number is not peak residency
or an end-to-end timing.

Build and run with GHC 9.8.4 and Cabal:

```sh
cabal build lib:php-parser
mkdir -p /tmp/php-parser-query-objects
cabal exec -- ghc -O1 -rtsopts -outputdir /tmp/php-parser-query-objects -o /tmp/query-allocation bench/QueryAllocation.hs
for query in variables expressions; do
  for size in 1000 2000 4000 8000; do
    /tmp/query-allocation "$query" "$size" +RTS -T -RTS
  done
done
```

Each line contains the query, operand count, result count, and allocated bytes.
To compare revisions, compile this same script against each revision and run the
binaries with the same GHC and flags. The measurement includes the query and
the result length check, not tree construction. The result length check forces
the complete list spine.

On Linux with GHC 9.8.4, `-O1`, and `+RTS -T`, five alternating runs per
revision produced the same allocation on every run. The baseline is commit
`283aeaca7bfdbc859d8acf425f20c92f999a51cc`.

| Query | Operands | Baseline bytes | Updated bytes |
| --- | ---: | ---: | ---: |
| variables | 1000 | 28110520 | 177696 |
| variables | 2000 | 144561304 | 353696 |
| variables | 4000 | 646060632 | 705696 |
| variables | 8000 | 2719714776 | 1409696 |
| expressions | 1000 | 56354456 | 601512 |
| expressions | 2000 | 289424024 | 1201512 |
| expressions | 4000 | 1292627608 | 2401512 |
| expressions | 8000 | 5440476824 | 4801512 |
