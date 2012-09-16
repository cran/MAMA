\name{MAP.Matches}
\alias{MAP.Matches.res}
\alias{MAP.Matches}
\title{
Wrapper function for MAP-Matches method
}
\description{
This is a wrapper function for MAP-Matches method. 
}
\usage{
MAP.Matches(data, varname, t.cutoff = "98.00\%", multiple = TRUE, perm = c("both", "columns", "labels")[1], nperm = 1000, test = c("t", "t.equalvar")[1], sig.col, sig.cutoff = 0.05)
}
\arguments{
  \item{data}{Object of class \code{MetaArray}}
  \item{varname}{Character String - name of one column in clinical data matrices to be used as class labels}
  \item{t.cutoff}{Character String - quantile of T statistics to be selected, e.g. "95.00\%" selects the top 5 percent of absolute values}
  \item{multiple}{Logical - when \code{TRUE} only paterrns with multiple '1' are used}
  \item{perm}{Character String - if \code{"labels"} only class labels are permuted for statistical analysis (empirical significance), if \code{"columns"} only genes in each dataset are selected randomly, if \code{"both"} both class labels and genes are permuted and two p-values returned}
  \item{nperm}{Numeric - number of permutations}
  \item{test}{Character String - if \code{"t"} then unequal variance t-test is used, if \code{"t.equalvar"} equal variance t-test is used}
  \item{sig.col}{Character String - which p-value is used for selection of significant patterns. Possible values are: \code{"p.col.strong"}, \code{"p.col.weak"}, \code{"p.lab.strong"}, \code{"p.lab.weak"} , "col" refers to column permutations, "lab" to class labels, "weak" to soft match and "strong" to strong match}
  \item{sig.cutoff}{Numeric - p-value for selection of sigificant patterns}
}
\value{
Object of class \code{MAP.Matches.res} containing
  \item{tests }{Data.frame of test statistics}
  \item{bin.matrix}{Binary matrix from \code{tests}, \code{1} means the test statistics was higer than threshold}
  \item{sumarization}{Sumarization of \code{bin.matrix}: number of selected genes in each dataset, genes with at least one \code{1} in pattern, probability of observing strong or soft match in the data  }
  \item{MAP }{Data frame describing observed patterns: number of strong \code{n.strong} and soft \code{n.soft} matches and number of genes involved \code{n.sig}}
  \item{stat.analysis}{Results of statistical analysis} 
  \item{genes}{List of genes observed with each pattern} 
}
\references{Yang, X., Bentink, S. and Spang, R. 2005, Detecting Common Gene Expression
Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices,
Vol.7:3, pp. 247-251}
\author{
Ivana Ihnatova
}
\examples{
data(ColonData)
MAP.Matches(ColonData, "satelite", nperm = 100, sig.col="p.lab.strong")

}
\keyword{ univar }

