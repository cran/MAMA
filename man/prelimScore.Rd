\name{prelimScore}
\alias{prelimScore}
\title{Function compute preliminary Similarity Score for Ordered Gene Lists}
\description{
Function computes preliminary Similarity Score as defined in Yang, 2005. 
}
\usage{
prelimScore(ordering, alpha, min.weight = 1e-05, two.sided = TRUE)
}
\arguments{
  \item{ordering}{Data frame, where columns refer to ordered gene list from one study}
  \item{alpha}{Numeric parameter used in weights \code{exp(-alpha*n)}}
  \item{min.weight}{minimal weight to be counted}
  \item{two.sided}{if \code{TRUE} both top and bottom of the \code{ordering} considered, if \code{FALSE} only top positions are considered}
}

\value{Similarity Score}
\references{
Yang, X., Bentink, S., Scheid, S. Spang, R., Similarities of ordered gene lists, 2005
}
\author{
Ivana Ihnatova
}

\examples{ 
genes<-paste("Gene", 1:100)
O<-cbind(sample(genes), sample(genes), sample(genes))
prelimScore(O, 0.1)
}
\keyword{ univar }
