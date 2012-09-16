\name{RandomScore}
\alias{RandomScore}
\title{
Function to do compute random and subsampled similarity score
}
\description{
Function computes random and subsampled similarity score in order to select appropriate tunning parameter alpha for weights in preliminary simmilarity score
}
\usage{
RandomScore(data, varname, B, alpha, test, which = c("random", "empirical", "subsample"), two.sided = TRUE)
}
\arguments{
  \item{data}{MetaArray object}
  \item{varname}{String indicating which column of clinical data should be used as class labels, same on all data set}
  \item{B}{Number of permutation}
  \item{alpha}{Vector of alphas considered (can be returned by \code{computeAlpha}) or selected manually}
  \item{test}{\code{"FCH"} for fold change (function \code{fold.change}) 
  or \code{"T"} for T-test (function \code{meta.test})}
  \item{which}{\code{"random"} for random score (permutation of class labels), \code{"subsample"} for subsampled score, \code{"empirical"} for empirical confidence intervals of overalaping genes, vector of several is also possible}  
  \item{two.sided}{if \code{TRUE} both top and bottom of the \code{ordering} considered, if \code{FALSE} only top positions are considered }
}
\value{
A list 
  \item{random}{Random similarity score}
  \item{empirical}{Empirical confidence intervals for overlaping genes}
  \item{subsample}{Subsampled score}
}

\author{
Ivana Ihnatova
}


\keyword{ univar }
