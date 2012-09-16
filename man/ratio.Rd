\name{ratio}
\Rdversion{1.1}
\alias{ratio}
\title{Function to calculate the ratio of co-significant: expected/observed
}
\description{Function to calculates the ratio of co-significant: expected/observed of strong and soft pattern matches
}
\usage{
ratio(X.discret)
}
\arguments{
  \item{X.discret}{Matrix of 0 and 1. Rows correspond to genes, columns to studies (comparisons). 1 means that T statistic for the gene in the study was higher than selected threshold, otherwise 0 is used. }
}
\details{
Calculation is part of MAP-Matches methods. See References for details\\}
\value{
A list which contains
 \item{n}{Number of selected genes in each comparison}
 \item{X.string}{Patterns in observed in data}
 \item{p.strong}{Co-significance of strong pattern match, probability of observing strong pattern match in data?}
 \item{p.soft}{Co-significance of soft pattern match, probability of observing strong pattern match in data?}
}
\references{
Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices, vol.7:3
}
\author{
Codes provided by Xinan Yang <xnyang@seu.edu.cn>
}
\examples{
A<-matrix(c(1,0,0,1,0,1,0,1,1,0,1,0,1,0,1), ncol=3, nrow=10)
ratio(A)
}
\keyword{ nonparametric }

