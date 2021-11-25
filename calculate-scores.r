meffonym.add.model("mdd", c("intercept",rownames(mdd.model)), c(0,mdd.model$effect), "MDD")

scores <- sapply(paste("aries", c("15","24","preg","mid"), sep="."), function(ds) { 
    meffonym.score(get(ds)$methylation, "mdd")$score
},simplify=F)
