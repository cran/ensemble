\name{summary.ensemble}
\alias{summary.ensemble}
\title{summary of an ensemble object}
\description{
Returns a detailed listing of a fitted ensemble object. 
}
\usage{
summary.ensemble(x, ntree, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{an ensemble object}
  \item{ntree}{an vector specifying a subset of the trees to summary}
  \item{\dots}{other argument specifying the parameter to summary \code{
      \link{rpart}} objects}
}
\details{
  This function is a method for the generic function summary for class 
  "ensemble". It can be invoked by calling summary for an object of the 
  appropriate class, or directly by calling summary.ensemble regardless of 
  the class of the object. 
}
\seealso{\code{\link{ensemble}}, \code{\link{predict.ensemble}}} 
\keyword{tree}
