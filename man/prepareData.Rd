\name{prepareData}
\alias{prepareData}
\title{
Function to prepare data 
}
\description{
Function prepares data as part of \code{RandomScore} function
}
\usage{
prepareData(j, data, varname, p, type)
}
\arguments{
  \item{j}{Permutation}
  \item{data}{MetaArray object - original dataset}
  \item{varname}{ String indicating which column of clinical data matrices 
  should be used to compute test statistic. Same column is used in all datasets.}
  \item{p}{Permutation of class labels or subsample}
  \item{type}{\code{1} for permutation of class labels, \code{2} for subsamples}
}

\value{ MetaArray object  }

\author{
Ivana Ihnatova
}


\keyword{ manip }
