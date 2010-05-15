\name{mergedata}
\alias{mergedata}
\title{
Function to do merge ExpressionSet objects
}
\description{
Function takes ExpressionSet object and return merged data matrix, class labels and origin labels
}
\usage{
mergedata(..., class.col)
}
\arguments{
  \item{\dots}{
ExpressionSet objects
}
  \item{class.col}{
vector of columns of pData to be used as class labels
}
}

\value{
 \item{dat }{Gene expression data matrix (binned columns of individual \code{exprs()} data}
 \item{cl }{Vector of class lables (same numeric value for samples from one class}
 \item{origin}{Vector of origin labels (same numeric value for samples from one data set)  }
}

\author{
Ivana Ihnatova
}
     

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(...,class.col)
{
args<-list(...)
N<-length(args)
merge<-list()
for (i in c(1:N))
{
if (i==1) {
merge$dat=exprs(args[[i]])
merge$cl=as.numeric(pData(args[[i]])[,class.col[i]])
merge$origin=c(rep(i,length(as.numeric(pData(args[[i]])[,class.col[i]]))))
  } else {
merge$dat=cbind(merge$dat, exprs(args[[i]]))
merge$cl=c(merge$cl,as.numeric(pData(args[[i]])[,class.col[i]]))
merge$origin=c(merge$origin,rep(i,length(as.numeric(pData(args[[i]])[,class.col[i]]))))
  }
  }
return(merge)
  }
}

\keyword{ manip }

