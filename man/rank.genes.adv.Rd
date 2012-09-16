\name{rank.genes.adv}
\alias{rank.genes.adv}
\title{
Function to rank genes
}
\description{
A wrapper function to \code{rank.genes}. This function provides a data frame of ranks, where rows correspond to genes and columns to data sets.
}
\usage{
rank.genes.adv(testp)
}
\arguments{
  \item{testp}{
A list with two slots: \code{test} and \code{p}, where \code{test} is data frame of T-statistics and \code{p} is data frame of p-values. In both rows refer to genes and columns to data sets. It is output from \code{meta.test} function. 
}
}

\value{
A data frame of ranks, where rows correspond to genes and columns to data sets.
}
\author{
Ivana Ihnatova
}

\seealso{
\code{\link{meta.test}}, \code{\link{rank.genes}}
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
rank.genes.adv(m)
}
\keyword{ manip}

