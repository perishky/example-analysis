#MDD DNAm score in ALSPAC

Test associations between a published
DNAm model of major depressive disorder
and depression in ALSPAC.

The main analysis script is `run-all.r`.

```
Rscript run-all.r
```

Assumes correct setting of directory paths (top of `run-all.r`), e.g.
```
project.dir <- "[base project directory]"
alspac.dir <- "[ALSPAC data directory]"
aries.dir <- "[ARIES data directory]"
```

Assumes installation of the following R packages:

|package|source|
|:-|:-|
|alspac|https://github.com/explodecomputer/alspac|
|meffonym|https://github.com/perishky/meffonym|
|eval.save|https://github.com/perishky/eval.save|
|pROC|CRAN|
|markdown|CRAN|
|knitr|CRAN|
|tableone|CRAN|






