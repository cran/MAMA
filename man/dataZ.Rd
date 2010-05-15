\name{dataZ}
\Rdversion{1.1}
\alias{dataZ}

\title{
Data format converter
}
\description{
Function to convert data from ExpressionSet to form suitable for function \code{Zscore}
}
\usage{
dataZ(data, group, nsamp, varname)
}

\arguments{
  \item{data}{
ExpressionSet
}
  \item{group}{
number of column of pData with group identification
}
  \item{nsamp}{
number of samples in a group, same in each dataset and group
}
 \item{varname}{
group label
} 
}
\value{
ExpressionSet with reduced number of samples and groups labeled 0,1
}
\author{
Ivana Ihnatova
}


\seealso{
\code{\link{dataGEM}}, \code{\link{dataSOGL}}
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
dataZ<-dataZ(data,1,5,"tumour")
}
\keyword{ manip }

