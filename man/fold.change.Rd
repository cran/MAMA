\name{fold.change}
\Rdversion{1.1}
\alias{fold.change}

\title{
function to do compute fold change between two groups
}
\description{
Function computes fold change between two groups of log2-transformed data
}
\usage{
fold.change(data, group)
}

\arguments{
  \item{data}{
microarray dataset - ExpressionSet object
}
  \item{group}{
number of column of pData indicating group labels
}
}
\value{
vector of fold change
}
\author{
Ivana Ihnatova
}
\examples{
data(Singhdata)
exprs<-as.matrix(Singhdata$esets[[1]])
rownames(exprs)<-Singhdata$geneNames
metadata=data.frame(labelDescription=c("Normal/Tumour"), row.names=c("group"))
pData<-data.frame(group=Singhdata$classes[[1]]+1)
rownames(pData)<-colnames(exprs)
phenoData<-new("AnnotatedDataFrame", data=pData,varMetadata=metadata)
data<-new("ExpressionSet", exprs=exprs, phenoData=phenoData)
fc<-fold.change(data,1)
}
\keyword{ nonparametric }

