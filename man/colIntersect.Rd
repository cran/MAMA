\name{colIntersect}
\alias{colIntersect}
\title{
Function to find intersect in columns of a data.frame
}
\description{
Function returns intersect of all coulmns of a data.frame.
}
\usage{
colIntersect(x)
}
\arguments{
  \item{x}{A data.frame}
}
\details{
Intersect is found recursively. In means that at first the intersect of the first and the second column is computed. Later this intersect is compared to third column in order to obtain common values etc.
}
\value{Vector of common values}
\author{Ivana Ihnatova}

\seealso{
 \code{\link{intersect}}, ~~~
}
\examples{
genes<-paste("Gene", 1:100)
O<-cbind(sample(genes), sample(genes), sample(genes))
colIntersect(O[1:50,])
}
\keyword{ manip }

