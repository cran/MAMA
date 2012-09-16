\name{MAP.genes}
\Rdversion{1.1}
\alias{MAP.genes}
\title{
Function to do assign probesets IDs to patterns
}
\description{
Function makes a list of vectors of probeset IDs. One vector contains probesets with one observed pattern.
}
\usage{
MAP.genes(resx, value.dis, files = TRUE)
}

\arguments{
  \item{resx}{
data.frame, rows refer to patterns, columns to pattern description - see in examples
}
  \item{value.dis}{
Matrix of observed patterns: binary matrix, columns refer to studies, rows to genes,
}

  \item{files}{
logical, when TRUE, files with probeset IDs are written too
}
}


\value{
list, each slot is vector of gene names 

}


\author{
Ivana Ihnatova
}
\examples{
#> t(resx)
#                        111 101    110    011
#n.sig[which(n.sig > 1)]   3   2  2.000  2.000
#n.strong                 32 127 20.000  6.000
#n.soft                   32 159 52.000 38.000
#p.soft                    0   0  0.000  0.000
#p.strong                  0   0  0.000  0.002
#permu.soft                0   0  0.000  0.000
#permu.strong              0   0  0.001  0.008
}

\keyword{ manip }

