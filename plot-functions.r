plots.boxplot <- function(
    formula, data, main, xlab, ylab, sub,
    col=c("red", "skyblue","green"),
    show=F, plot.points=nrow(data) < 100, ...) {
    
    if (!plot.points) {        
        ret <- do.call(
            boxplot,
            list(formula=as.formula(formula),
                 data=data, col=col, ...))
        
    }
    else {
        ret <- do.call(
            boxplot,
            list(formula=as.formula(formula),
                 data=data, ...))
        
        group.name <- rownames(attr(terms(formula), "factors"))[2]
        variable.name <- rownames(attr(terms(formula), "factors"))[1]
        group <- match(as.character(data[,group.name]), ret$names)
        x.coord <- group + runif(nrow(data), min=-0.25, max=0.25)
        y.coord <- data[,variable.name]
        
        for (g in unique(group)) {
            idx <- which(group == g)
            points(x=x.coord[idx], y=y.coord[idx], col=col[g], pch=20, cex=2)
        }
        ret$x <- x.coord
        ret$y <- y.coord
    }
    if (missing(sub)) {
        fit <- do.call(lm, list(formula=as.formula(formula), data=data))
        r <- summary(fit)$adj.r.squared
        p <- coef(summary(fit))
        p <- p[nrow(p),ncol(p)]
        sub <- paste("adjusted R^2 = ", round(r, digits=2),
                     ", p = ", format(p, digits=5), sep="")
    }
    
    title(main=main, xlab=xlab, ylab=ylab, sub=sub)
    
    if (show)
        for (i in 1:ncol(ret$stats))
            text(x=rep(i,nrow(ret$stats)), y=ret$stats[,i], labels=round(ret$stats[,i], digits=2), pos=3)

    ret
}

scatter.thinning <- function(x,y,resolution=100,max.per.cell=100) {
    x.cell <- floor((resolution-1)*(x - min(x,na.rm=T))/diff(range(x,na.rm=T))) + 1
    y.cell <- floor((resolution-1)*(y - min(y,na.rm=T))/diff(range(y,na.rm=T))) + 1
    z.cell <- x.cell * resolution + y.cell
    frequency.table <- table(z.cell)
    frequency <- rep(0,max(z.cell))
    frequency[as.integer(names(frequency.table))] <- frequency.table
    f.cell <- frequency[z.cell]
    
    big.cells <- length(which(frequency > max.per.cell))
    sort(c(which(f.cell <= max.per.cell),
           sample(which(f.cell > max.per.cell),
                  size=big.cells * max.per.cell, replace=F)),
         decreasing=F)
}

plots.scatterplot <- function(x, y, sub, col.line="red", confidence=0.95, robust=F, smooth=F, labels=NULL, ...) {
    if (missing(sub)) {
        p.robust <- NA
        if (robust) {
            fit <- summary(rlm(y ~ x))            
            coef <- coef(fit)["x",]
            p.robust <- unname(2*pt(abs(coef[["t value"]]), fit$df[2], lower.tail=FALSE))
        }
        fit <- lm(y ~ x)
        r <- summary(fit)$adj.r.squared
        r <- ifelse(r < 0, 0, r)
        p <- tail(coef(summary(fit))["x",], n=1)

        sub <- paste("n = ", length(y),
                     ", CI = ", confidence,
                     ", adjusted R^2 = ", round(r, digits=5),
                     ", p = ", format(p, digits=5),
                     ifelse(robust, paste(", robust p = ", format(p.robust, digits=5), sep=""), ""),
                     sep="")
    }
    if (!smooth) 
        plot(x, y, sub=sub, pch=20, ...)
    else
        smoothScatter(x,y, sub=sub, ...)
    plots.regression.line(x,y,col.line,confidence,robust)

    if (!is.null(labels)) {
        stopifnot(length(labels) == length(x))
        text(x,y,labels=labels,pos=3)
    }
}

plots.regression.line <- function(x, y, col.line="red", confidence=0.95, smooth=F, robust=F, ...) {
    if (robust) fit <- rlm(y ~ x)
    else fit <- lm(y ~ x)
    abline(fit, col=col.line, ...)
    newx <- seq(par("usr")[1], par("usr")[2], length.out=100)
    p <- predict(fit, newdata=data.frame(x=newx),
                 interval="confidence", level=confidence, type="response")
    lines(newx, p[,"lwr"], col=col.line, lty=2, ...)
    lines(newx, p[,"upr"], col=col.line, lty=2, ...)
    if (smooth) {
        idx <- which(!is.na(y) & !is.na(x))
        idx <- idx[order(x[idx])]
        fit <- loess(y[idx] ~ x[idx])
        lines(x[idx], predict(fit), col=col.line, lty="dotted", ...)
    }
}
