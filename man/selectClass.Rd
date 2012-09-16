\name{selectClass}
\alias{selectClass}
\title{ Function to select class labels from MetaArray object }
\description{
Function selects one column from each clinical data matrix and binds them into a list object 
}
\usage{
selectClass(x, varname, type)
}
\arguments{
  \item{x}{ MetaArray object }
  \item{varname}{ Character String specifying which column of clinical data should be selected }
  \item{type}{if \code{factor} then factor vector is returned , if \code{binary} then vector of 1's and 0' is returned as class labels}
}

\value{
  A list where each slot refers to one clinical data matrix (study) and contains selected class labels of the samples.
}

\author{ Ivana Ihnatova }
\note{ Such a class labels extraction is necessary for some methods of meta-analysis of microarray }

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

dataset<-new("MetaArray", GEDM=list(Singhdata$esets[[1]], Singhdata$esets[[2]], Singhdata$esets[[3]]),
clinical=list(cl1, cl2, cl3), datanames=c("dataset1", "dataset2", "dataset3"))

selectClass(dataset, "classlab", "factor")
selectClass(dataset, "classlab", "binary")
}


\keyword{ manip }

