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
test.group.shuffle(x,  varname, minSampleNum = 3, method = "t",B=1000)
}

\arguments{
  \item{x}{
MetaArray object
}
  \item{varname}{
String indicating the column of clinical data matrices definig groups
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

\keyword{ univar }

