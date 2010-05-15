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

## The function is currently defined as
function(testp)
{
n<-ncol(testp$test)
for (i in 1:n)
{
 if (i==1) {
 r<-rank.genes(testp$test[,i],testp$p[,i])
 } else {
 r<-cbind(r,rank.genes(testp$test[,i],testp$p[,i])) 
 }
  }
rownames(r)<-rownames(testp$test)
colnames(r)<-colnames(testp$p)
return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}

