\name{test.group.shuffle}
\Rdversion{1.1}
\alias{test.group.shuffle}

\title{
Function to do compute test statistic iterativelly
}
\description{
Function computes test statistic with random assignment of group labels to samples in each iteration. It binds results to one matrix. Finaly it multiplies values of test statistics by -1. It saves a file necessary in MAP-Matches method.  
}
\usage{
test.group.shuffle(data, dataname, var=1, minSampleNum = 3, method = "t",B=1000)
}

\arguments{
  \item{data}{
Gene expresion data of class \code{ExpressionSet}
}
  \item{dataname}{
Name of the data set
}
  \item{var}{
Number specifying column of \code{pData} definig groups
}
\item{minSampleNum}{Minimal number of samples required for test statistic}
  \item{method}{Type of test as in \code{mt.teststat}}
  \item{B}{Number of iterations}
}

\value{matrix of test statistics (with random group assignment and multiplied by -1)
}
\references{
Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices, vol.7:3
}
\author{
Ivana Ihnatova
}

\seealso{
 \code{\link{entitybuild2}}
}
\examples{
require(multtest)
data(Singhdata)
exprs<-as.matrix(Singhdata$esets[[1]])
rownames(exprs)<-Singhdata$geneNames
metadata=data.frame(labelDescription=c("Normal/Tumour"), row.names=c("group"))
pData<-data.frame(group=as.factor(Singhdata$classes[[1]]+1))
rownames(pData)<-colnames(exprs)
phenoData<-new("AnnotatedDataFrame", data=pData,varMetadata=metadata)
data<-new("ExpressionSet", exprs=exprs, phenoData=phenoData)

dat.shuf<-test.group.shuffle(data=data, dataname="sample", B=10)
}
\keyword{ univar }

