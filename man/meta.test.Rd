\name{meta.test}
\alias{meta.test}
\title{
Function to compute T-statistic and p-value in meta-analysis
}
\description{
Function \code{meta.test} returns a list with two slots: data frame of test statistics and data frame of p-values. In each of the matrices rows correspond to genes and columns to data sets.
}
\usage{
meta.test(x, varname, stat = "t")
}

\arguments{
 \item{x}{MetaArray object}
  \item{varname}{A String indicating which column of clinical data matrices should be used as class labels. Column of such name must be present in all datasets. It must not be a binary vector (0's and 1's)}
  \item{stat}{A character String indicating the type of test statistic to be computed as used in \code{mt.teststat} function}
}

\value{
A list with two slots:
  \item{test}{A data frame of statistics in which rows correspond to genes and columns to data sets}
  \item{p}{A data frame of p-values (only if \code{test="t"} returned) in which rows correspond to genes and columns to data sets}
}

\author{
Ivana Ihnatova
}


\examples{
data(Singhdata)

cl1<-as.data.frame(Singhdata$classes[[1]]+1)
names(cl1)<-"classlab"
cl2<-as.data.frame(Singhdata$classes[[2]]+1)
names(cl2)<-"classlab"
cl3<-as.data.frame(Singhdata$classes[[3]]+1)
names(cl3)<-"classlab"
rownames(Singhdata$esets[[1]])<-Singhdata$geneNames
rownames(Singhdata$esets[[2]])<-Singhdata$geneNames
rownames(Singhdata$esets[[3]])<-Singhdata$geneNames

data<-new("MetaArray", GEDM=list(Singhdata$esets[[1]], Singhdata$esets[[2]], Singhdata$esets[[3]]),
clinical=list(cl1, cl2, cl3), datanames=c("dataset1", "dataset2", "dataset3"))

m<-meta.test(data,"classlab")
}
\keyword{ htest }

