\name{predict.ensemble}
\alias{predict.ensemble}
\title{Return predicted values}
\description{
  Return an ensemble object of predicted values
}
\usage{
predict.ensemble(object, newdata)
}
\arguments{
  \item{object}{fitted model object of class ensemble}
  \item{newdata}{data frame containing the values at which predictions are 
     required. The predictors referred to in the right side of formula(object) 
     must be present by name in newdata. If missing, the fitted values are 
     returned.}
}
\details{
  This function is a method for the generic function predict for class rpart. 
  It can be invoked by calling predict for an object of the appropriate class, 
  or directly by calling predict.ensembler regardless of the class of the
  object.
}
\value{
    an object of class \code{\link{ensemble}}
    \item{type}{constant "prediction", type of the object}
    \item{s.method}{trainingset subsampling method}
    \item{num}{number of trees in the ensemble}
    \item{weights}{voting weights for each tree}
    \item{label}{classified labels for the training data}
    \item{class.mtx}{an array of classification result}
}
\seealso{ \code{\link{ensemble}}}
\keyword{tree}
\keywork{classif}
