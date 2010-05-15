\name{dataGEM}
\Rdversion{1.1}
\alias{dataGEM}
\title{
Data format converter
}
\description{
Function to convert data from ExpressionSet to list of expression data matrix, group labels, probes names.
}
\usage{
dataGEM(data, group)
}

\arguments{
  \item{data}{
microarray data as object class \code{ExpressionSet}    }
  \item{group}{
number of column of pData with group labels
}
}

\value{
 \item{expr}{gene expression data matrix}
 \item{class}{vector with group labels}
 \item{keys}{vector of probe names}
}

\author{
Ivana Ihnatova
}


\seealso{
 \code{\link{dataSOGL}}, \code{\link{dataZ}}
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
dataG<-dataGEM(data,1)
}
\keyword{ manip }
