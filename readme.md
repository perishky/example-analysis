# MDD DNAm score in ALSPAC

Test associations between a published
DNAm model of major depressive disorder
and depression in ALSPAC.

The main analysis script is `run-all.r`.

File paths are set in config.yml.

```
Rscript run-all.r [configuration]
```

Configuration refers the particular configuration in config.yml to use.

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






