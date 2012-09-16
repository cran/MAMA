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
  \item{ALLtype}{Vector of class labels, must be \code{factor}}
  \item{type}{Levels of class labels}
  \item{dataset}{Name of the dataset}
  \item{minSampleNum}{Minimal number of samples required for test statistic}
  \item{method}{Type of test as in \code{mt.teststat} (one of \code{fc}, \code{t}, \code{z})}
  \item{random}{Logical, if TRUE samples are assinged to groups randomly}
}     
\value{
Vector of test statistics. 
}
\references{
Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices, vol.7:3
}
\author{
Code provided by Xinan Yang <xnyang@seu.edu.cn> has been modified by Ivana Ihnatova
}
\examples{
data(Singhdata)
group<-as.factor(Singhdata$classes[[1]])
entitybuild2(Singhdata$esets[[1]], ALLtype=group, type=levels(group))
}
\keyword{univar}

