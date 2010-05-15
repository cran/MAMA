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
\dontrun{
data(Singhdata)

#compute T-statistics and P-value
p1<-apply(Singhdata$esets[[1]],1,function(x) {t=t.test(x~Singhdata$classes[[1]], alternative="two.sided"); return(t$p.value) })
p2<-apply(Singhdata$esets[[2]],1,function(x) {t=t.test(x~Singhdata$classes[[2]], alternative="two.sided"); return(t$p.value) })
p3<-apply(Singhdata$esets[[3]],1,function(x) {t=t.test(x~Singhdata$classes[[3]], alternative="two.sided"); return(t$p.value) })
T1<-apply(Singhdata$esets[[1]],1,function(x) {t=t.test(x~Singhdata$classes[[1]], alternative="two.sided"); return(t$statistic) })
T2<-apply(Singhdata$esets[[2]],1,function(x) {t=t.test(x~Singhdata$classes[[2]], alternative="two.sided"); return(t$statistic) })
T3<-apply(Singhdata$esets[[3]],1,function(x) {t=t.test(x~Singhdata$classes[[3]], alternative="two.sided"); return(t$statistic) })

# Rank genes
rank1<-rank.genes(T1,p1)
rank2<-rank.genes(T2,p2)
rank3<-rank.genes(T3,p3)
# Compute R^star and Q^star 
RANK<-cbind(rank1,rank2, rank3)
RQ<-compute.RQ(RANK)
head(RQ)
# MonteCarlo test
MC<-MCtest(RANK,RQ, nper=2)

}        }

\keyword{ nonparametric }

