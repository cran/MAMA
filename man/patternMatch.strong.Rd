\name{patternMatch.strong}
\Rdversion{1.1}
\alias{patternMatch.strong}
\title{
Function to count strong pattern matches
}
\description{
Funtion counts number of observed strong matches in meta-analysis 
}
\usage{
patternMatch.strong(X.discret, unique.pat)
}
\arguments{
  \item{X.discret}{Binary matrix, with rows corresponding to genes, columns to studies and 1 to selected (significant) genes in studies}
  \item{unique.pat}{Vector of binary strings - patterns}
}
\value{Numeric vector of number of strong pattern matches for each pattern.
}

\references{
Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices, vol.7:3
}
\author{
Codes provided by Xinan Yang <xnyang@seu.edu.cn>
}

\examples{
A<-matrix(c(1,0,0,1,0,1,0,1,1,0,1,0,1,0,1), ncol=3, nrow=10)
uni<-c("011","101","110","111")
patternMatch.strong(A,uni)
}
\keyword{nonparametric}

