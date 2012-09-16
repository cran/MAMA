\name{mergedata}
\alias{mergedata}
\title{Function to merge data from MetaArray object}
\description{
Function merges the data stored in MetaArray object. It binds expression 
data matrices into one gene expression data matrix. It creates one binary vector 
of class labels of the samples and one numeric vector of orgin of the samples.
}
\usage{
mergedata(x, varname)
}
\arguments{
  \item{x}{MetaArray objec}
  \item{varname}{character String specifying the column of clinical data to be used in vector of class labels of the samples}
}

\value{
  A list with three slots
  \item{dat }{Gene expression data matrix, rows refer to genes/probes and columns to samples}
  \item{cl }{Binary vector of class labels of the samples}
  \item{origin }{Numeric vector describing the origin of the samples. Same number refers to samples from one study}
}

\author{ Ivana Ihnatova }

\examples{
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

data<-new("MetaArray", GEDM=list(Singhdata$esets[[1]], Singhdata$esets[[2]], Singhdata$esets[[3]]),
clinical=list(cl1, cl2, cl3), datanames=c("dataset1", "dataset2", "dataset3"))

merged.data<-mergedata(data,"classlab")
summary(merged.data)
}
\keyword{ manip }

