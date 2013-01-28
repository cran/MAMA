\name{METRADISC}
\alias{METRADISC.res}
\alias{METRADISC}
\title{
Wrapper function for METRADISC method 
}
\description{
This is a wrapper function for meta-analytical method called METRADISC that perform all steps of the analysis and return all the outputs in one line.
}
\usage{
METRADISC(data, varname, nperm = 1000)
}
\arguments{
  \item{data}{MetaArray object}
  \item{varname}{
Character String - name of one column in clinical data matrices to be used as class labels
}
  \item{nperm}{Number of permutations for Monte Carlo permutation test, at least 1000 is suggested}
}
\value{
An object of class \code{METRADISC.res} containing
  \item{ranks }{Ranks of the genes in each dataset}
  \item{RQ}{Average rank (R) and measure of heterogeneity (Q) for each gene}
  \item{MCtest}{Four p-values (for low and high R and low and high Q) for each gene as provided after Monte Carlo permutation test}
}
\references{
Zintzaras, E., Ioannidis, J.P.A 2008 Meta-analysis for ranked discovery datasets: Theoretical framework and empirical demonstration for microarrays, Computational Biology and Chemistry 32, 39-47
}
\author{
Ivana Ihnatova
}
\examples{
data(ColonData)
m <- METRADISC(ColonData, "MSI", 5)
}
\keyword{ univar }
