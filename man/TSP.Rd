\name{TSP}
\alias{TSP}
\alias{tsp.res}
\title{
Wrapper function for top-scoring pair 
}
\description{
This is a wrapper function for top-scoring pair method.
}
\usage{
TSP(data, varname, B = 50)
}
\arguments{
  \item{data}{Object of class MetaArray}
  \item{varname}{Character String - name of one column in clinical data matrices to be used as class labels}
  \item{B}{Number of permutations for empirical signifficance}
}
\value{
An object of class \code{tsp.res}. It is a list
  \item{tsp}{Top scoring pair of genes}
  \item{sig}{Significance of top score. It is a list, where \code{p} referes to the p-value and \code{nullscores} to the top scores achieved in permutations}

}
\references{
D. Geman, C. d'Avignon, D. Naiman and R. Winslow, "Classifying gene expression profiles from pairwise mRNA comparisons," Statist. Appl. in Genetics and Molecular Biology, 3, 2004.


A.C. Tan, D.Q. Naiman, L. Xu, R.L. Winslow, D. Geman, "Simple decision rules for classifying human cancers from gene expression  profiles," Bioinformatics, 21: 3896-3904, 2005.

}
\author{
Ivana Ihnatova
}

\seealso{
 \code{\link{tspcalc}}, \code{\link{tspsig}}, \code{\link{tspplot}}
}
\examples{
data(ColonData)
TSP(ColonData, "satelite", 10)
}
\keyword{ univar }
