\name{meta.test}
\alias{meta.test}
\title{
Function to compute T-statistic and p-value in meta-analysis
}
\description{
Function \code{meta.test} returns a list with two slots: data frame of test statistics and data frame of p-values. In each of the matrices rows correspond to genes and columns to data sets.
}
\usage{
meta.test(..., class.col, data.names)
}

\arguments{
  \item{\dots}{
Data sets as ExpressionSet objects.
}
  \item{class.col}{
A numeric vector indicating which column of pData() has to be used as class labels.
}
  \item{data.names}{
A character vector with names of the data sets.
}
}

\value{
A list with two slots:
  \item{test}{A data frame of T-statistics in which rows correspond to genes and columns to data sets}
  \item{p}{A data frame of p-values (for T-statistics in \code{test} slot in which rows correspond to genes and columns to data sets}
}

\author{
Ivana Ihnatova
}


\examples{
\dontrun{
m<-meta.test(denmark, australia, japan, class.col=c(1,1,2), data.names=c("denmark","australia","japan"))
}}
\keyword{ htest }

