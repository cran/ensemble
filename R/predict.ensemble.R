predict.ensemble <- function(object, newdata)
 {
     if (!inherits(object, "ensemble"))
         stop("Not legitimate ensemble object")

     method <- object$sampling
     number <- object$num
     level <- levels(object)
     if (method == "bagging")
         weights <- rep(1 / number, number)
     else
         weights <- object$weights[, 2]
     total.vote <- matrix(0, nrow(newdata), length(level))
     dimnames(total.vote) <- list(d1 = NULL, d2 = level)
     class.mtx <- array(0, c(nrow(newdata), length(level), number)) 
     dimnames(class.mtx) <- list(d1 = NULL, d2 = level, d3 = 1:number)
     res <- list(type = "prediction", sampling = method, number = number, 
         weights = weights)
     
     for (i in 1:number) {
 	 class.mtx[,,i] <- predict(object$trees[[i]], newdata)
	 total.vote <- total.vote + class.mtx[,,i] * weights[i]
     }
     lcolmax <- function(x)
         level[match(max(x), x)]
     label <- factor(apply(total.vote, 1, lcolmax), levels = level)
     res$label <- label
     res$class.matrix <- class.mtx
     levels(res) <- level
     class(res) <- "ensemble"
     res
}
