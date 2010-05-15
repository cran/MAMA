\name{dataSOGL}
\Rdversion{1.1}
\alias{dataSOGL}
\title{
Data format converter
}
\description{
Function to convert ExpressionSet to form suitable for SOGL method
}
\usage{
dataSOGL(data, group, groupname, annotation)
}
\arguments{
  \item{data}{
ExpressionSet
}
  \item{group}{
number of column with group identification
}
  \item{groupname}{
character - name of grouping variable
}
  \item{annotation}{
character - identification of microarray platform
}
}
\value{
ExpressionSet object
}
\author{
Ivana Ihnatova
}
\seealso{
 \code{\link{dataZ}}, \code{\link{dataGEM}}
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
dataS<-dataSOGL(data,1,"tumor", "hgu133plus2")
}
\keyword{ manip }