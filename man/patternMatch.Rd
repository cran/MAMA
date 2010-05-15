\name{patternMatch}
\Rdversion{1.1}
\alias{patternMatch}
\title{
Function to count soft pattern matches
}
\description{Funtion counts number of observed soft matches in meta-analysis 
}
\usage{
patternMatch(X.discret, unique.pat)
}
\arguments{
  \item{X.discret}{Binary matrix, with rows corresponding to genes, columns to studies and 1 to selected (significant) genes in studies}
  \item{unique.pat}{Vector of binary strings - patterns}
}

\value{Numeric vector of number of soft pattern matches for each pattern.
}
\references{
Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices, vol.7:3
}
\author{
Codes provided by Xinan Yang <xnyang@seu.edu.cn>
}

\examples{

## The function is currently defined as
A<-matrix(c(1,0,0,1,0,1,0,1,1,0,1,0,1,0,1), ncol=3, nrow=10)
uni<-c("011","101","110","111")
patternMatch(A,uni)
}
\keyword{ nonparametric}

