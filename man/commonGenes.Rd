\name{commonGenes}
\alias{commonGenes}
\title{
Function to compute number of common genes in ordered gene lists
}
\description{ Function computes number of common genes up to each position (from \code{1} to \code{n})}
\usage{
commonGenes(ord, n)
}
\arguments{
  \item{ord}{Data frame, where columns refer to ordered gene list from one study}
  \item{n}{The last position to be concered}

}
\value{Numeric vector, number of common genes up to each position
}
\references{
Yang, X., Bentink, S., Scheid, S. Spang, R., Similarities of ordered gene lists, 2005
}
\author{
Ivana Ihnatova
}
\note{Created as part of implementation of the Similarity of Ordered Gene Lists method}



\examples{
genes<-paste("Gene", 1:100)
O<-cbind(sample(genes), sample(genes), sample(genes))
commonGenes(O,100)
}
\keyword{ univar }
