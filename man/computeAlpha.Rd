\name{computeAlpha}
\alias{computeAlpha}
\title{
Function to do compute tunning parameter alpha
}
\description{Function computes vector of possible alphas in Similarity of Ordered Gene List method. See Details.}
\usage{
computeAlpha(n = NULL, min.weight = 1e-05, ngenes)
}
\arguments{
  \item{n}{ Number of genes to be considered in the comparison , if \code{NULL} a pre-defined vector is used}
  \item{min.weight}{ Minimal weight to be counted }
  \item{ngenes}{Number of genes in the dataset}
}
\details{
Alphas are calculated so that at certain position (\code{n}), the exponential weights reach \code{min.weight}. If one is interessted in comparing ordered gene lists up to certain position, alpha appropriate for this position can be calculated. 
}
\value{ Numeric vector of possible alphas }
\references{
Yang, X., Bentink, S., Scheid, S. Spang, R., Similarities of ordered gene lists, 2005
}
\author{Ivana Ihnatova}

\examples{
#using default n
A<-computeAlpha(ngenes=1000)

#or with user-selected n
A<-computeAlpha(n=seq(from=25, to=300, by= 25),ngenes=1000)

}
\keyword{ univar }
