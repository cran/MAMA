\name{zScores}
\alias{zScores}
\alias{zScorePermuted}
\alias{zScoreFDR}
\alias{multExpFDR}
\title{
Function for Meta-analysis of gene expression data
}
\description{
Functions for computing zScores for FEM and REM and computing FDR. This are modification of functions found in GeneMeta package.
}
\usage{
zScores(esets, classes, useREM=TRUE, CombineExp=1:length(esets))
zScorePermuted(esets, classes, useREM=TRUE, CombineExp=1:length(esets))
zScoreFDR(esets, classes, useREM=TRUE, nperm=1000, CombineExp=1:length(esets))
multExpFDR(theScores, thePermScores, type="pos")
}
\arguments{
  \item{esets}{A \code{list} of \code{matrices}, one expression set per experiment. All experiments must have the same variables(genes).}
  \item{classes}{A \code{list} of class memberships, one per experiment. Each \code{list} can only contain 2 levels.}
  \item{useREM}{A \code{logical} value indicating whether or not to use a REM, \code{TRUE}, or a FEM, \code{FALSE}, for combining the z scores.}
  \item{theScores}{A \code{vector} of scores (e.g. t-statistics or z scores)}
  \item{thePermScores}{A \code{vector} of permuted scores (e.g. t-statistics or z scores)}
  \item{type}{\code{"pos"}, \code{"neg"} or \code{"two.sided"}}
  \item{nperm}{number of permutations to calculate the FDR}
  \item{CombineExp}{
A \code{vector} of \code{integer}- which experiments should be combined-default:all experiments
}
}
\details{
The function \code{zScores} implements the approach of Choi et al. for MetaArray. The function \code{zScorePermuted} applies zScore to a single permutation of the class labels. The function \code{zScoreFDR} computes a FDR for each gene, both for each single experiment and for the combined experiment. The FDR is calculated as described in Choi et al. Up to now ties in the zscores are not taken into account in the calculation. The function might produce incorrect results in that case. The function also computes zScores, both for the combines experiment and for each single experiment. 
}
\value{
A \code{matrix} with one row for each probe(set) and the following columns:
\item{zSco_Ex_}{For each single experiment the standardized mean difference, Effect_Ex_, divided by the estimated standard deviation, the square root of the EffectVar_Ex_ column.}
\item{MUvals}{The combined standardized mean difference (using a FEM or REM)}
\item{MUsds}{The standard deviation of the MUvals.}
\item{zSco}{The z statistic - the MUvals divided by their standard deviations, MUsds.}
\item{Qvals}{Cochran's Q statistic for each gene.}
\item{df}{The degree of freedom for the Chi-square distribution. This is equal to the number of combined experiments minus one.}
\item{Qpvalues}{The probability that a Chi-square random variable, with df degrees of freedom) has a higher value than the value from the Q statistic.}
\item{Chisq}{The probability that a Chi-square random variate (with 1 degree of freedom) has a higher value than the value of zSco^2.}
\item{Effect_Ex_}{The standardized mean difference for each single experiment.}
\item{EffectVar_Ex_}{The variance of the standardized mean difference for each single experiment.}
Note that the three column names that end in an underscore are replicated, once for each experiment that is being analyzed. 
}
\references{
Choi et al, Combining multiple microarray studies and modeling interstudy variation. Bioinformatics, 2003, i84-i90.
}
\author{
M. Ruschhaupt (original function), I. Ihnatova (modification)
}
\examples{
data(ColonData)
esets <- GEDM(ColonData)
classes <- selectClass(ColonData, "satelite", "binary")
theScores <- zScores(esets, classes, useREM = FALSE)
}

\keyword{manip}

