print.ensemble <- function(x, ntree, ...)
{
    if (!inherits(x, "ensemble"))
        stop("Not legitimate ensemble object")
	
    cat("\n this ensemble contains ", x$num, " decision trees")
    cat("\n sampling method: ", x$sampling, "\n")
    if (x$type == "prediction") {
        cat("\n classification result: ", "\n")
	print(x$label)
    }	
    else {
        cat("\n classfication trees \n")
	if (missing(ntree)) 
	    ntree <- 1
	else
	    ntree <- ntree[ntree <= x$num]
	for (i in 1:length(ntree)) {
	    cat("\n tree#", ntree[i], "  weights", x$weights[ntree[i], 2],
	    "  misclassification rate", x$class.err[ntree[i]], "\n")
            print(x$trees[ntree[i]], ...)
        }
    }
}
