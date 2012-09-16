\name{MCtest}
\alias{MCtest}
\title{ Monte Carlo permutation test }
\description{ 
This function performs Monte Carlo permutation test to asses the statistical significance of R and Q statistics. 
 }
\usage{
MCtest(RAN, RQ, nper = 100)
}
\arguments{
  \item{RAN}{matrix of ranks to be permuted, columns refer to studies, rows refer to genes  }
  \item{RQ}{ observed values of R (average rank) and Q (heterogenity) - as produced by \code{compute.RQ}}
  \item{nper}{ number of permutations}
}

\value{
  Returns a matrix with four columns. First (Second) column represents significance level of high (low) avarage rank. Third (fourth) represents significance level of high (low) heterogenity. 
  }
\references{ Zintzaras, E., Ioannidis, J.P.A 2008 Meta-analysis for ranked discovery datasets: Theoretical framework and empirical demonstration for microarrays, Computational Biology and Chemistry 32, 39-47}
\author{ Ivana Ihnatova}

\seealso{ \code{\link{rank.genes}},\code{\link{compute.RQ}} }
\examples{
RANK<-cbind(sample(100), sample(100), sample(100))
RQ<-compute.RQ(RANK)
head(RQ)       
MCtest(RANK, RQ, nper=100)
}

\keyword{ nonparametric }

