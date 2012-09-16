\name{preparePermutations}
\alias{preparePermutations}
\title{
Function to prepare permutation and subsamples
}
\description{ Function prepares permutations of class labels and subsamples from expression data 
}
\usage{
preparePermutations(id, B, sample.ratio = 0.8)
}
\arguments{
  \item{id}{Binary vector (0's and 1's) of class labels to be permuted and subsampled}
  \item{B}{number of premutations}
  \item{sample.ratio}{ratio for subsampling, default \code{0.8} means 80\% of samples from each group is selected}
}

\value{
A list
\item{yperm}{Permutation - vectors of 0's and 1's shuffeled}
\item{ysubs}{Subsamples - numeric vectors indicating which samples should be selected}
}
\author{
Ivana Ihnatova, Claudio Lottaz
}


\keyword{ manip }
