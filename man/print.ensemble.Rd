\name{print.ensemble}
\alias{print.ensemble}
\title{print an ensemble object}
\description{
  This function prints an ensemble object. It is a method for the generic 
  function print of class ensemble.
}
\usage{
print.ensemble(x, ntree, ...)
}
\arguments{
  \item{x}{an ensemble object}
  \item{ntree}{an vector specifying a subset of the trees to print}
  \item{\dots}{other argument specifying the parameter to print \code{\link{rpart}} objects}
}
\details{
   This function is a method for the generic function print for class 
   "ensemble". It can be invoked by calling print for an object of the 
   appropriate class, or directly by calling print.ensemble regardless of the
   class of the object. 
}
\seealso{\code{\link{ensemble}}, \code{\link{predict.ensemble}}}
\keyword{tree}
