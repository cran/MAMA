\name{selectAlpha}
\alias{selectAlpha}
\title{
Function to select the most optimal alpha parameter
}
\description{
Function selects the most optimal value of alpha parameter according to pAUC. 
For each of possible alphas the pAUC is computed as a measure of the separabilty of two distributions of similarity score: random and subsampled (prepared by function \code{RandomScore}.
Alpha with maximal pAUC is selected. 
}
\usage{
selectAlpha(alpha, subsample, random)
}
\arguments{
  \item{alpha}{Vector of possible alphas}
  \item{subsample}{Similarity scores after subsampling}
  \item{random}{Similarity scores after permuting class labels }
}

\value{
A list:
  \item{alpha}{selected value of alpha}
  \item{pAUC}{pAUC for all alphas achivied}
}

\author{Ivana Ihnatova}



\seealso{
 \code{\link{RandomScore}}
}

\keyword{ univar }
