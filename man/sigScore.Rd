\name{sigScore}
\alias{sigScore}
\title{
Function to calculate signifficance of similarity score
}
\description{
Function calculates empirical signifficance of similarity score by means of random permutation of ordered gene lists, computing the similarity scores and comparing them to the value observed in original ordering of the genes.
}
\usage{
sigScore(ranking, alpha, B, min.weight = 1e-05, two.sided = TRUE)
}
\arguments{
  \item{ranking}{Ordered gene lists as data.frame or matrix, each column refer to one study}
  \item{alpha}{Selected alpha parameter for Similarity score}
  \item{B}{Number of permutation}
  \item{min.weight}{Minimal weight for similarity score calculation}
  \item{two.sided}{if \code{TRUE} both top and bottom of the \code{ordering} considered, if \code{FALSE} only top positions are considered}
}

\value{Signifficance of similarity score in form of p-value}

\author{Ivana Ihnatova}




\examples{
\dontrun{
genes<-paste("Gene", 1:100)
O<-cbind(sample(genes), sample(genes), sample(genes))
sigScore(O, 0.0001, 100)
}}
\keyword{ htest }

