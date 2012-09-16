\name{metagene}
\Rdversion{1.1}
\alias{metagene}
\title{
Function to do extract row from list of data.frames
}
\description{
Function extracts one row (specified by number or name) from all data.frames of input list
}
\usage{
metagene(x, results)
}
\arguments{
  \item{x}{
number or name of row to be extracted
}
  \item{results}{
list of data frame (for example outputs of methods of meta-analysis where rows refer to genes or probesets)
}
}
\value{
list, one slot refer to one data.frame
}
\author{
Ivana Ihnatova
}
\examples{
A<-data.frame(x=rep(c(1,2,3),2),y=rep(c("a","b","c"),2))
B<-data.frame(x=rep(c(9,8,7),2),y=rep(c("x","y","z"),2))
res<-list(A=A,B=B)
metagene(2,res)
               }
\keyword{ manip }

