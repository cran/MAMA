\name{MAPsig2}
\Rdversion{1.1}
\alias{MAPsig2}

\title{
Pattern signifficance
}
\description{
Function computes significance of observed number of strong and soft matches by 
randomly assigning group labels in each study.
}
\usage{
MAPsig2(dataset,value.dis, unique.pat, B = 1000)
}

\arguments{
  \item{dataset}{
vector with datasets names - function loads files \code{paste("random_stat_",dataset[i],".Rdata"}
}
 \item{value.dis}{
binary matrix from T-statistics
}
  \item{unique.pat}{unique meta-analysis patterns
}
  \item{B}{
    number of iterations
}
}

\value{
data.frame with p-values for number of oberved soft and strong matches}
\references{
Yang, X., Bentink, S. and Spang, R. 2005, Detecting Common Gene Expression
Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices,
Vol.7:3, pp. 247-251
}
\author{
Ivana Ihnatova
}

\keyword{ nonparametric }

