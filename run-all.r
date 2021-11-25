project.dir <- "[base project directory]"
alspac.dir <- "[ALSPAC data directory]"
aries.dir <- "[ARIES data directory]"

dir.create(output.dir <- file.path(project.dir, "output"))
dir.create(object.dir <- file.path(project.dir, "objects"))

## library for loading alspac data
## https://github.com/explodecomputer/alspac
library(alspac)
alspac::setDataDir(alspac.dir)

## library for saving time-consuming evaluations 
## https://github.com/perishky/eval.save
library(eval.save)
eval.save.dir(object.dir)

## function for loading an RData file
## but loading into a list
## rather than into main memory and
## potentially overwriting something
source("load-list-function.r",echo=T)
## out: load.list()

## function for loading aries DNAm data
## (old code! use the R package instead:
##  https://github.com/MRCIEU/aries)
source("load-aries-function.r",echo=T)
## out: load.aries()

aries.15 <- load.aries(aries.dir, "450", "15up")
aries.24 <- load.aries(aries.dir, "epic", "F24")
aries.preg <- load.aries(aries.dir, "450", "antenatal")
aries.mid <- load.aries(aries.dir, "450", "FOM")

## load ALSPAC data
source("alspac.r")
## out: alspac.table

## load DNAm model of MDD
mdd.model <- read.table("mdd-model.txt", header=F, sep=" ", row.names=1)
colnames(mdd.model) <- "effect"

## library for calculating model scores in DNAm datasets
## https://github.com/perishky/meffonym
library(meffonym)

## calculate MDD methylation scores in ARIES
source("calculate-scores.r",echo=T)
## in: aries.15, aries.24, aries.preg, aries.mid, mdd.model
## out: scores


## libraries for generating markdown report of outputs
library(markdown)
library(knitr)
source("plot-functions.r")
## out: plots.boxplot()
source("knit-report-function.r")
## out: knit.report()


## library and function for generating 'Table 1' in markdown
## (not used in this report)
library(tableone)
source("kableone-function.r")
## out: kableone()

options(markdown.HTML.options=union(c('toc', 'base64_images'), getOption("markdown.HTML.options")))
options(markdown.HTML.stylesheet=file.path(getwd(), "style.css"))
options(markdown.HTML.header=file.path(getwd(), "collapsible.html"))

## generate the report and save in markdown and html
knit.report("report.rmd", file.path(output.dir, "report.html"))

