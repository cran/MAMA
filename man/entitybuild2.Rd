\name{entitybuild2}
\Rdversion{1.1}
\alias{entitybuild2}
\title{
Function to calculate test statistic for microarray data
}
\description{Calculates test statistic for microarray data}
\usage{
entitybuild2(expr.mat, ALLtype = NULL, type, dataset = NULL, minSampleNum = 3, method = "t", random = FALSE)
}
\arguments{
  \item{expr.mat}{Expression matrix, with rows corresponding to genes and columns to samples}
  \item{ALLtype}{Factor vector of phenodata}
  \item{type}{Vector of levels of phenodata}
  \item{dataset}{Name of the dataset}
  \item{minSampleNum}{Minimal number of samples required for test statistic}
  \item{method}{Type of test as in \code{mt.teststat}}
  \item{random}{Logical, if TRUE samples are assinged to groups randomly}
}
\value{
Vector of test statistics. 
}
\references{
Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices, vol.7:3
}
\author{
Codes provided by Xinan Yang <xnyang@seu.edu.cn>
}
\keyword{univar}

