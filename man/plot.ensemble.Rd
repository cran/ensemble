\name{plot.ensemble}
\alias{plot.ensemble}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{plot an ensemble object}
\description{
Plots an ensemble object on the current graphics device(under development).
}
\usage{
plot.ensemble(x, ntree, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{an ensemble object}
  \item{ntree}{an vector specifying a subset of the trees to plot}
  \item{\dots}{other argument specifying the parameter to plot \code{\link{rpart}} objects}
}
\details{
  This function is a method for the generic function plot, for objects of 
  class ensemble.  
}
\seealso{\code{\link{plot.rpart}}, \code{\link{ensemble}}}
\keyword{tree}
