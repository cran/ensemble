summary.ensemble <- function(x, ntree, ...)
{
    if (!inherits(x, "ensemble"))
        stop("Not legitimate ensemble object")

    cat("\n this ensemble contains ", x$num, " decision trees \n")
    cat("\n sampling method: ", x$sampling, "\n")

    if (x$type == "training") {
        cat("\n weights and error rate of each classifier \n")
	if (x$sampling == "bagging")
            print(cbind(x$weights, "error rate" = x$class.err))
	else
	    print(cbind(x$weights, "error rate" = x$class.err, "ens error"
	        = x$ens.err))
        cat("\n confusion matrix for training data \n")
        print(x$conf.mtx)
        if (missing(ntree))
	    ntree <- 1
	else
            ntree <- ntree[ntree <= x$num]
        for (i in 1:length(ntree)) {
            cat("\n tree#", ntree[i], "  weights", x$weights[ntree[i], 2], "\n")
            summary(x$trees[[ntree[i]]], ...)
        }
    }
    else {
	cat("\n summary of classification result \n")
        print(summary(x$label, ...))
	cat("\n summary of total votes \n")
	summary(x$vote, ...)
    }
}

