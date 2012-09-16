\name{computeOrdering}
\alias{computeOrdering}
\title{
Function to compute ordered gene lists
}
\description{
Function computes test statistic for each gene in each dataset of MetaArray object 
and orders them form the most up-regulated (possitive statisics) to the most down-regulated (negative statistics). }
\usage{computeOrdering(data, varname, test)}
\arguments{
  \item{data}{MetaArray object}
  \item{varname}{A string indicating which column of clinical data matrices 
  should be used to compute test statistic. Same column is used in all datasets.}
  \item{test}{\code{"FCH"} for fold change (function \code{fold.change}) 
  or \code{"T"} for T-test (function \code{meta.test}) }
}

\value{ A data frame, each column refers to ordered gene list from one study}
\author{Ivana Ihnatova}
\seealso{
\code{\link{fold.change}}, \code{\link{meta.test}}
}
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

ord<-computeOrdering(data, "classlab", "FCH")
}

\keyword{ htest }