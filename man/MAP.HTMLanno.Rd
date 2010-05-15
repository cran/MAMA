\name{MAP.HTMLanno}
\Rdversion{1.1}
\alias{MAP.HTMLanno}

\title{
Function to do make annotation of vectors of probeset ID
}
\description{
Function creates HTML document with annotation of probeset ID
}
\usage{
MAP.HTMLanno(resx, probs, package)
}

\arguments{
  \item{resx}{
data.frame, rows refer to patterns, columns to pattern description - see in examples
}
  \item{probs}{
list of vectors of probeset ID
}
  \item{package}{
name of R-packge with annotation of microarray platform used
}
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

