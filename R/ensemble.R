ensemble <- function(formula, data = NULL, ..., subset, na.action 
    = na.rpart, s.method = c("bagging", "M1.AdaBoost", "M2.AdaBoost"), 
    number = 15)
{ 
    # extract model frame
    m <- match.call(expand = FALSE)
    if (is.matrix(eval(m$data, sys.frame(sys.parent()))))
        m$data <- as.data.frame(data)
    m$... <- m$number <- m$s.method <- NULL
    m[[1]] <- as.name("model.frame")
    m <- eval(m, sys.frame(sys.parent()))
    Y <- model.extract(m, "response")
    s.method <- match.arg(s.method)
    
    ens.prob <- matrix(0, nrow(m), (number+1))
    ens.prob[,1] <- rep(1/nrow(m), nrow(m))
    ens.tree <- ens.pred <- vector("list", number)
    ens.weights <- ens.err <- class.err <- numeric(number)
    res <- list(type = "training", sampling = s.method) 
    flag <- flag1 <- 0
    
    # constructing ensembles
    for (i in 1:number) {
	j <- 1
	while (j <= 5) {
	    if (s.method == "bagging") 
	        s.index <- sample(nrow(m), nrow(m), replace = T)
	    else { 
		if (sum(is.na(ens.prob[,i])) > 0) 
		    browser()
                s.index <- sample(nrow(m), nrow(m), replace = T, prob = 
	            ens.prob[,i])
	    }
            s.data <- m[s.index, ]
            ens.tree[[i]] <- ens.rpart(formula, data = s.data, subset = 
	        1:nrow(m), ...)
	    if (!is.na(ens.tree[[i]])) break
	    if (j > 5) {
	       flag <- 1
	       break
	    }   
	    j <- j + 1
	}    
        ens.pred[[i]] <- predict(ens.tree[[i]])
        lcolmax <- function(x)
            levels(Y)[match(max(x), x)]
	l.err <- rep(FALSE, nrow(m))
	l.err[s.index] <- apply(ens.pred[[i]], 1, lcolmax) != Y[s.index]
	class.err[i] <- sum(l.err)/ nrow(m) 
	if (s.method != "bagging" && class.err[i] < 0.01) {
	    flag <- 1
	    cat("\n sampling method: ", s.method)
	    cat("\n tree number: ", i)
	    cat("\n misclassification rate too small \n")
	    break
	}
	if (s.method != "bagging") {
            if (s.method == "M1.AdaBoost") {
	        ens.err[i] <- sum(l.err * ens.prob[, i])
 	        if (ens.err[i] > 0.5) {
	            flag <- 1
		    cat("\n error rate too high")
 	            break
                }    
	        err.term <- 1 - l.err
	    }
	    else { 
	        class.mtx <- matrix(0, nrow(m), ncol(ens.pred[[i]]))
		class.mtx[cbind(1:nrow(m), as.numeric(Y))] <- 1
		class.mtx[s.index, ] <- ens.pred[[i]]
		.err <- (1 + apply(class.mtx, 1, sum) - 2 *   
		    class.mtx[cbind(1:nrow(m), as.numeric(Y))]) / 2 
	        ens.err[i] <- sum(ens.prob[i] * .err)
	        err.term <- (1 - apply(class.mtx, 1, sum) + 2 *
	            class.mtx[cbind(1:nrow(m), as.numeric(Y))]) / 2
	    } 
	    beta.l <- ens.err[i] / (1 - ens.err[i])
	    ens.weights[i] <- log(1/beta.l)
            ens.prob[, (i+1)] <- ens.prob[, i] * beta.l^err.term
	    ens.prob[,i+1][is.na(ens.prob[,i+1])] <- 0
	    ens.prob[, (i+1)] <- ens.prob[, (i+1)] / sum(ens.prob[, (i+1)])
        }
    }	
    if (flag == 1 && i <= 3) { 
	cat(" too small number of trees. ensemble aborted \n")
        return(NA)
    }	
    res$num <- min((i - flag), number)
    res$class.err <- class.err[1:(i - flag)]
    if (s.method != "bagging") {
        res$ens.err <- ens.err[1:(i - flag)]
        res$prob <- ens.prob[, 1:(i - flag)]
        res$weights <- cbind(classifier = 1:(i - flag), weights =
	    ens.weights[1:(i - flag)] / sum(ens.weights[1:(i - flag)]))
    }
    else {
        res$weights <- cbind(classifier = 1:(i - flag), weights = 
	    rep(1/(i - flag), i - flag))
    }	
    res$trees <- ens.tree[1:(i - flag)]  # ens.tree[[1:(i - flag)]] wrong
    class(res) <- c("ensemble", class(res))
    attr(res, "levels") <- levels(Y)
    p.label <- predict(res, m)$label
    res$label <- data.frame(label = Y, pred = p.label)
    res$conf.mtx <- table(res$label[c("label", "pred")])
    res$model <- m
    res
 }
