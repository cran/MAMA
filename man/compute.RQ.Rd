\name{compute.RQ}
\alias{compute.RQ}
\title{Function to compute R and Q statistics as defined in - see References}
\description{
  Function computes R (average rank across studies) and Q (sum of the squared deviations of each study's rank for the gene from the mean of the ranks for that gene)
}
\usage{
compute.RQ(RAN)
}
\arguments{
  \item{RAN}{matrix with rank of genes as produced by \code{rank.genes}, with rows coresponding to genes and columns coresponding to studies}
}
\value{
  matrix with first column of R statistic and second of Q statistic
  }
\references{ Zintzaras, E., Ioannidis, J.P.A 2008 Meta-analysis for ranked discovery datasets: Theoretical framework and empirical demonstration for microarrays, Computational Biology and Chemistry 32, 39-47}
\author{Ivana Ihantova}
\seealso{\code{\link{rank.genes}},\code{\link{MCtest}}}
\examples{
RANK<-cbind(sample(100), sample(100), sample(100))
RQ<-compute.RQ(RANK)
head(RQ)
}
\keyword{ nonparametric}