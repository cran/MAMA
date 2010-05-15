\name{compute.RQ}
\alias{compute.RQ}
\title{Function to compute R and Q statistics as defined in - see References}
\description{
  Function computes R (average rank across studies) and Q (sum of the squared deviations of each study's rank for the gene from the mean of the ranks for that gene)
}
\usage{
compute.RQ(RAN)
}
\arguments{
  \item{RAN}{matrix with rank of genes as produced by \code{rank.genes}, with rows coresponding to genes and columns coresponding to studies}
}
\value{
  matrix with first column of R statistic and second of Q statistic
  }
\references{ Zintzaras, E., Ioannidis, J.P.A 2008 Meta-analysis for ranked discovery datasets: Theoretical framework and empirical demonstration for microarrays, Computational Biology and Chemistry 32, 39-47}
\author{Ivana Ihantova}
\seealso{\code{\link{rank.genes}},\code{\link{MCtest}}}
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
}}
\keyword{ nonparametric}