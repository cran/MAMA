\name{ES.GeneMeta}
\alias{ES.GeneMeta}
\alias{ES.GeneMeta.res}
\title{
Wrapper function for combining the effect size as implemented in GeneMeta package}
\description{
This is a wrapper function for meta-analytical method implemented in GeneMeta package
}
\usage{
ES.GeneMeta(data, varname, useREM = TRUE, CombineExp = 1:length(esets), nperm = 1000)
}

\arguments{
  \item{data}{MetaArray object}
  \item{varname}{Character String - name of one column in clinical data matrices to be used as class labels}
  \item{useREM}{Logical - indicating whethet Random Effect Model (REM) shuld be used, if \code{FALSE} then Fixed Effect Model is applied
}
  \item{CombineExp}{A numeric vector - which experiments should be combined, all experiments are set as default}
  \item{nperm}{Number of permutations to calculate FDR}
  }
\value{
An object of class \code{ES.GeneMeta.res}
  \item{theScores}{Ouput from function \code{zScores}}
  \item{ScoresFDR}{Output from function \code{zScoreFDR}}
}
\references{ Choi et al, Combining multiple microarray studies and modeling interstudy variation. Bioinformatics, 2003, i84-i90.
}
\author{
Ivana Ihnatova
}

\seealso{
\code{\link{zScores}}, \code{\link{zScoreFDR}}
}
\examples{
data(ColonData)
es<- ES.GeneMeta(ColonData, "satelite", nperm = 10)
}
\keyword{ univar }

