set.cond <- function(x,condition,value) {
    x[which(condition)] <- value
    x
}
map <- function(x, ...) {
    key <- list(...)
    x[which(!x %in% names(key))] <- NA
    xp <- unlist(key)[match(x, names(key))]
    names(xp) <- names(x)
    print(table(original=x, mapped=xp, useNA="ifany"))
    xp
}
or.na <- function(...) {
    x <- sapply(list(...), function(x) x)
    is.na <- rowSums(!is.na(x)) == 0
    ret <- rowSums(x, na.rm=T) > 0
    ret[is.na] <- NA
    ret
}


alspac.vars <- c(
    "fh6892", ## dawba at 15.5 (1=yes, 2=no)
    "fh0011a", ## age of the child (months)
    "FJCI1001", ## cis-r at 17 (1=yes, 0=no)
    "fj003a", ## age of the child (months)
    "fkdq1000", ## cis-r mild depression at 24 (0=no, 1=yes)
    "fkdq1010", ## cis-r moderate depression at 24
    "fkar0010", ## age of the child (months)
    ## severe depression numbers too low
    "c600", ## EPDS at 32w gestation
    "c994", ## age of mother
    "c991", ## weeks gestation
    "t3255", ## EPDS at 18 years post-pregnancy
    "t9991a")## age of study child (months)

## fathers/partners epds not close enough to methylation time point
alspac.table <- eval.save({
    var.info <- findVars(alspac.vars)
    var.info <- filterVars(var.info, c600=list(obj="^c_"), t3255=list(obj="^t_"))
    alspac.table <- extractVars(var.info)
    colnames(alspac.table) <- tolower(colnames(alspac.table))
    alspac.table
}, "alspac")

stopifnot(all(tolower(alspac.vars) %in% colnames(alspac.table)))

alspac.table$dawba.15 <- map(alspac.table$fh6892, "1"=1, "2"=0)
alspac.table$dawba.15.age <- set.cond(alspac.table$fh0011a/12, alspac.table$fh0011a < 0, NA)
alspac.table$cisr.17 <- map(alspac.table$fjci1001, "0"=0, "1"=1)
alspac.table$cisr.17.age <- set.cond(alspac.table$fj003a/12, alspac.table$fj003a < 0, NA)
alspac.table$cisr.mild.24 <- map(alspac.table$fkdq1000, "0"=0, "1"=1)
alspac.table$cisr.mild.24.age <- set.cond(alspac.table$fkar0010/12,alspac.table$fkar0010 < 0, NA)
alspac.table$cisr.mod.24 <- map(alspac.table$fkdq1010, "0"=0, "1"=1)
alspac.table$cisr.mod.24.age <- set.cond(alspac.table$fkar0010/12,alspac.table$fkar0010 < 0, NA)
alspac.table$epds.preg <- set.cond(alspac.table$c600, alspac.table$c600 < 0, NA) >= 13
alspac.table$epds.preg.age <- set.cond(alspac.table$c994, alspac.table$c994 < 0, NA)
alspac.table$epds.mid <- set.cond(alspac.table$t3255, alspac.table$t3255 < 0, NA) >= 13
alspac.table$epds.mid.age <- set.cond(alspac.table$c994 + alspac.table$t9991a/12 + 2, alspac.table$c994 < 0 | alspac.table$t9991a < 0, NA)


