\name{ensemble}
\alias{ensemble}
\title{train ensembles of classifiers}
\description{
  Ensembles of classifiers is to combine the decisions of individual
  classifiers (typically by weighted or unweighted voting) to classify
  new examples. There are many ways to construct ensembles. This
  function uses training set subsampling.
}
\usage{
ensemble(formula, data=NULL, ..., subset, na.action=na.rpart, s.method=
  c("bagging", "M1.AdaBoost", "M2.AdaBoost"), number=15)
}
\arguments{
  \item{formula}{a formula, as in the lm function}
  \item{data}{a data frame in which to interpret the variables in the formula}
  \item{\dots}{other arguments. may include the arguments of \code{\link{rpart}}}
  \item{subset}{optional expression saying that only a subset of the
      rows of the data should be used in the fit}
  \item{na.action}{the default action delets all observations for which
      y is missing, but keep those in which one or more predictors are missing}
  \item{s.method}{training set subsampling method. one of bagging,
      M1.AdaBoost, M2.AdaBoost(should be AdaBoost.M*, change order for
      easier matching). default is bagging}
  \item{number}{maximum number of classifiers in Ensemble. default 15}
}
\details{
  Manipulating training dataset is one way to construct ensembles
  Three subsampling methods are implemented. The first is "boottrap
  aggregation", or "bagging", while the rest are different versions
  the boosting algorithm "AdaBoost". "AdaBoost.M1" is suitable for
  classifiers with error rate < 0.5, while "AdaBoost.M2" is specially
  adapted to "weak" classifiers with error rate often exceeding 0.5.
  For more details can refer to several papers in reference. 
}
\value{
  an object of class \code{\link{ensemble}}
  \item{type}{constant "training", type of the object}
  \item{s.method}{trainingset subsampling method}
  \item{num}{number of trees in the ensemble}
  \item{class.err}{misclassification rate for each tree}
  \item{ens.err}{weighted error rate for each tree for sampling methods
      other than "bagging"}
  \item{prob}{sampling probability vector for each tree for sampling
      methods other than "bagging"}
  \item{weights}{voting weights for each tree}
  \item{trees}{list of trees ("rpart" object \code{\link{rpart.object}})} 
  \item{label}{classified labels for the training data}
  \item{conf.mtx}{confusion matrix for the training data}
  \item{model}{model frame}
}
\references{
  Dietterich TG,  Machine-learning research - Four current directions,
  AI Magazine, 18: (4) 97-136, 1997
  AdaBoost website:
  http://www.research.att.com/~schapire/boost.html
}
\author{ Zhilong Yuan  zhilongyuan@students.wisc.edu}
\note{underdevelopment. not responsible for any loss caused by this package}
\seealso{\code{\link{rpart}}, \code{\link{predict.ensemble}},
    \code{\link{print.ensemble}}, \code{\link{summary.ensemble}}, 
    \code{\link{plot.ensemble}}
}

\examples{
 library(rpart)
 data(fgl)
 res.ens <- ensemble(type ~ ., data=fgl)
 res.ens
 plot(res.ens)
 summary(res.ens)
 res.pred <- predict.ensemble(res.ens, fgl)
 res.pred
}
\keyword{tree}
\keywork{classif}

