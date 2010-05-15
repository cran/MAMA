\name{cv.filter}
\Rdversion{1.1}
\alias{cv.filter}
\title{Microarray probes filtering}
\description{Function to filter microarray probes according to coefficient of variation}
\usage{
cv.filter(data, cutoff = 0.05)
}

\arguments{
  \item{data}{expression matrix with probes in rows and samples in columns}
  \item{cutoff}{cutoff value for filtering}
}

\value{Expression matrix, probes with CV below cutoff are filtered out.}

\author{Ivana Ihnatova}



\examples{
## The function is currently defined as
function(data, cutoff=0.05)
{
cv<-apply(data,1,function(x) sd(x)/mean(x))
co<-cutoff
plot(sort(cv), ylab="Coefficient of variation")
abline(h=co,col="red")
data<-data[sd>cutoff,]
return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}

