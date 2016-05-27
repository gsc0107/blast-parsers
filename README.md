# BLAST Parsers

Scala sample code for parsing various versions of [NCBI BLAST/P/N](https://en.wikipedia.org/wiki/BLAST) output formats.
Main package is `parsers.blast`, base trait is `BlastParser`.
BlastParserDemoApp and a sample file are included (arguments in brackets are optional):
```
sbt "run-main parsers.blast.BlastParserDemoApp [-nolegacy] [dir]"
```

