\name{MAPsig1}
\Rdversion{1.1}
\alias{MAPsig1}

\title{
Pattern signifficance
}
\description{
Function computes significance of observed number of strong and soft matches by 
randomly choosing differentially expressed genes in each study.
}
\usage{
MAPsig1(unique.pat, value.dis, iter = 1000)
}
\arguments{
  \item{unique.pat}{
unique meta-analysis patterns
}
  \item{value.dis}{
binary matrix from T-statistics
}
  \item{iter}{
number of iteration
}
}

\value{
data.frame with p-values for number of oberved soft and strong matches
}
\references{
Yang, X., Bentink, S. and Spang, R. 2005, Detecting Common Gene Expression
Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices,
Vol.7:3, pp. 247-251
}
\author{
Ivana Ihnatova
}


\keyword{ nonparametric }

