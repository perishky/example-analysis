## Create a markdown table for TableOne output
##
## Example use where tables are generated for multiple analyses.
## 
## ```{r,results="asis"}
## library(tableone)
## for (name in names(analyses)) {
##     ret <- CreateTableOne(
##                  vars=analyses[[name]]$covariatenames, 
##                  strata="gender",
##                  data=analyses[[name]]$data,
##                  factorVars=analyses[[name]]$categoricalvarnames)
##     cat("## Analysis: ", name, "\n")
##     print(kableone(ret, caption=name))
## }
## ```
kableone <- function(x, ...) {
    capture.output(x <- print(x))
    knitr::kable(x, ...)
}

