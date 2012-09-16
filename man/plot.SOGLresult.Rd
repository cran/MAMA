\name{plot.SOGLresult}
\alias{plot.SOGLresult}
\title{
Function to plot an object of class SOGLresult
}
\description{Function draws three plots presented for results of meta-analysis by Similarity of Ordered Gene List method
}
\usage{
\method{plot}{SOGLresult}(x, which, ...)
}
\arguments{
  \item{x}{SOGLresult object, provided by function \code{performSOGL}}
  \item{which}{Character indicator which plot has to be drawn, see Details}
  \item{...}{arguments to \code{plot} function}
}
\details{
If \code{which="alpha selection"} {Considered alphas and corresponding pAUC are plotted. Red vertical line signs selected alpha.}
If \code{which="density"}{Estimated density of random (in black) and subsampled score (in red)}
If \code{which="empirical CI"}{Empirical confidence intervals and observed number of overlapping genes}
}
\references{
Yang, X., Bentink, S., Scheid, S. Spang, R., Similarities of ordered gene lists, 2005
}
\author{Ivana Ihnatova}

\keyword{ hplot }