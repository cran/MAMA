\name{selectGenes}
\alias{selectGenes}
\title{
Function to select genes that account for Similarity score
}
\description{
Genes that account for e.g 95\% of Similarity score are returned. 
}
\usage{
selectGenes(ordering, alpha, percent, min.weight = 1e-05, two.sided = TRUE)
}

\arguments{
  \item{ordering}{Ordered gene lists as data.frame or matrix, each column refer to one study}
  \item{alpha}{Selected alpha parameter for Similarity score}
  \item{percent}{Percentage (Numeric between \code{0} and \code{1}) of the score}
  \item{min.weight}{minimal weight to be counted}
  \item{two.sided}{if \code{TRUE} both top and bottom of the \code{ordering} considered, if \code{FALSE} only top positions are considered}
}

\value{Vector of genes}

\author{Ivana Ihnatova}




\examples{
genes<-paste("Gene", 1:1000)

O<-cbind(c(sample(genes[1:200]),sample(genes[201:1000])),
         c(sample(genes[1:200]),sample(genes[201:1000])),
         c(sample(genes[1:200]),sample(genes[201:1000])) 
)
alph<-computeAlpha(100,ngenes=1000)
selectGenes(O, alph, 0.95)
}
\keyword{ univar }

