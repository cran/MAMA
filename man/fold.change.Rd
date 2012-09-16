\name{fold.change}
\Rdversion{1.1}
\alias{fold.change}

\title{
Function to do compute fold change between two groups
}
\description{
Function computes fold change between two groups of log2-transformed data
}
\usage{
fold.change(x, varname)
}

\arguments{
  \item{x}{ MetaArray object}
  \item{varname}{Character String specifying which column of clinical data matrices should be used as class labels. Column of this name must be present in each clinical data matrix.}
}
\value{
Data frame of fold changes, each column refer to one study and row to genes.
}
\author{
Ivana Ihnatova
}
\examples{
#data preparation
data(Singhdata)
cl1<-as.data.frame(Singhdata$classes[[1]])
names(cl1)<-"classlab"
cl2<-as.data.frame(Singhdata$classes[[2]])
names(cl2)<-"classlab"
cl3<-as.data.frame(Singhdata$classes[[3]])
names(cl3)<-"classlab"

rownames(Singhdata$esets[[1]])<-Singhdata$geneNames
rownames(Singhdata$esets[[2]])<-Singhdata$geneNames
rownames(Singhdata$esets[[3]])<-Singhdata$geneNames

dataset<-new("MetaArray", GEDM=list(Singhdata$esets[[1]], Singhdata$esets[[2]], Singhdata$esets[[3]]),
clinical=list(cl1, cl2, cl3), datanames=c("dataset1", "dataset2", "dataset3"))

#fold change
fch<-fold.change(dataset, "classlab")
head(fch)
}
\keyword{ nonparametric }

