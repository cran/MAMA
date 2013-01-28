\name{RankProduct}
\alias{RankProduct.res}
\alias{RankProduct}
\title{
Wrapper function for RankProduct method
}
\description{
This is a wrapper function for perfoming meta-analysis using Rank Product method.
}
\usage{
RankProduct(data, varname, num.perm = 100, logged = TRUE, na.rm = FALSE, gene.names = NULL, plot = FALSE, rand = NULL, cutoff = 0.05)
}

\arguments{
  \item{data}{MetaArray object}
  \item{varname}{Character String - name of one column in clinical data matrices to be used as class labels, factors are turned into a numeric vector by \code{as.numeric()-1)}}
  \item{num.perm}{Number of permutations}
  \item{logged}{Logical - indicating whether data are on log-scale}
  \item{na.rm}{	Logical - if \code{FALSE} (default), the NA value will not be used in computing rank. If \code{TRUE} the missing values will be replaced by the genewise mean of the non-missing values. Gene will all value missing will be assigned "NA"}
  \item{gene.names}{Character vector - gene names to be be attached to the estimated percentage of false prediction (pfp)}
  \item{plot}{Logical - if \code{TRUE} a plot of the estimated pfp verse the rank of each gene is drawn}
  \item{rand}{Numeric - a seed for random number generator}
  \item{cutoff}{Numeric - p-value for selection of significant genes}
}

\value{
Object of class \code{RankProduct.res} containing outputs from functions: \code{RPadvance} and \code{topGene}. 'Class1' refers to the first level of the used class labels, 'Class2' to the second one. 

}
\references{
Breitling, R., Armengaud, P., Amtmann, A., and Herzyk, P.(2004) Rank Products: A simple, yet powerful, new method to detect differentially regulated genes in replicated microarray experiments, FEBS Letter, 57383-92 
}
\author{
Ivana Ihnatova
}


\examples{
\dontrun{
data(ColonData)
rp<-RankProduct(ColonData, "MSI", num.perm=10)
}}
\keyword{ univar}

