\name{gene.select.FC}
\alias{gene.select.FC}
\title{ Function to select genes according to fold change}
\description{Function selects genes with fold change (in absolute value) above input cutoff }
\usage{
gene.select.FC(fch, cutoff)
}
\arguments{
  \item{fch}{ Data frame of fold change with columns corresponding to microarray experiments and rows to genes }
  \item{cutoff}{ Cutoff for selection}
}
\value{ List - each slot refers to one column of input data frame and it is a vector of genes names with fold change above selected threshold
  }
\author{Ivana Ihantova}

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
#gene selection
genes.selected<-gene.select.FC(fch, 1)
}
\keyword{manip }

