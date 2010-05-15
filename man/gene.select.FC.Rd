\name{gene.select.FC}
\alias{gene.select.FC}
\title{ Function to select genes according to fold change}
\description{Function selects genes with fold change (in absolute value) above input cutoff }
\usage{
gene.select.FC(data, cutoff)
}
\arguments{
  \item{data}{ Data frame of fold change with columns corresponding to microarray experiments and rows to genes }
  \item{cutoff}{ Cutoff for selection}
}
\value{ List - each slot refers to one column of input data frame and it is a vector of genes names with fold change above selected threshold
  }
\author{Ivana Ihantova}
%\note{This is the first part of VennMapper algorithm. }
\examples{
## The function is currently defined as
function(data, cutoff)
{
lists<-list()
for (i in 1:ncol(data)) lists[[i]]<-rownames(data)[abs(data[,i])>=cutoff]
names(lists)<-colnames(data)
return(lists)
  }
}
\keyword{manip }

