plot.ensemble <- function(x, ntree, ...)
{
    if (!inherits(x, "ensemble"))
        stop("Not legitimate ensemble object")

    if (x$type == "training") {
        if (missing(ntree))
            ntree <- 1:min(4, x$num)
        else
            ntree <- ntree[ntree <= x$num]
	.k <- length(ntree) %/% 4
	.m <- length(ntree) %% 4
	if (.k != 0) {
            for (i in 0:(.k-1)) {
	        split.screen(figs = c(2, 2))
	        for (j in 1:4) {
	            screen(j)
		    plot(x$trees[[i*4 + j]], ...)
		}
	    }
	}
	if (.m != 0) {
	    split.screen(figs = c(2, 2))
	    for (l in 1:.m) {
	        screen(l)
		plot(x$trees[[4*.k + l]], ...)
	    }	
	}
    }
}
	
	
