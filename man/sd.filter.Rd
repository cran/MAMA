\name{sd.filter}
\Rdversion{1.1}
\alias{sd.filter}
\title{Microarray probes filtering}
\description{Function to filter microarray probes according to standard deviation}
\usage{
sd.filter(data, cutoff = 0.5)
}
\arguments{
  \item{data}{expression matrix with probes in rows and samples in columns}
  \item{cutoff}{cutoff value for filtering}
}
\value{Expression matrix, probes with SD below cutoff are filtered out.}
\author{Ivana Ihnatova}
\examples{
## The function is currently defined as
function(data, cutoff=0.5)
{
sd<-apply(data,1,sd)
plot(sort(sd), ylab="Standard deviation")
abline(h=0.5,col="red")
data<-data[sd>cutoff,]
return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
