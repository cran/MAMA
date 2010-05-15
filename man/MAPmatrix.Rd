\name{MAPmatrix}
\alias{MAPmatrix}
\title{
Function to summarize binary matrix
}
\description{
Function \code{MAPmatrix} summarizes a binary matrix. It treats each row as Meta-Analysis Pattern and looks for count of observed soft and strong matches. 
}
\usage{
MAPmatrix(value.dis)
}
\arguments{
 \item{value.dis}{
A binary matrix with rows refering to genes and columns to microarray studies.
}
}

\value{
A matrix with rows corresponding to MAP patterns and four columns: unique patterns that are being observed in the data (\code{uniqe.pat}), number of observed soft matches with the pattern (\code{n.soft}), number of observed strong matches (\code{n.strong}) and number of $1$'s in the pattern \code{n.sig})
}
\references{
Yang, X., Bentink, S. a Spang, R., \emph{Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities}, Biomedical Microdevices, Vol.7:3, 2005
}
\author{
Ivana Ihnatova
}

\examples{

## The function is currently defined as
function(value.dis)
{
res<-ratio(value.dis)
unique.pat <- unique(results$X.string)
n.soft <- patternMatch(value.dis,unique.pat)
n.strong <- patternMatch.strong(value.dis,unique.pat)
unique.X <- patternmatrix(unique.pat,ncol(value.dis))
n.sig <- apply(unique.X,1,sum)
mat<-data.frame(unique.pat, n.soft, n.strong,n.sig)
return(mat)
  }
}

\keyword{ nonparametric }
