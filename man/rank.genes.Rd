\name{rank.genes}
\alias{rank.genes}

\title{ Rank genes }
\description{
   Assigna ranks to gene names according to p-value and sign of test statistics} 
\usage{
rank.genes(T, p)
}

\arguments{
  \item{T}{ vector of test statistics with gene names in \code{names}}
  \item{p}{ vector of p-values with gene names in \code{names} }
}
\value{
  Data frame with ranks of gene names
}
\references{  Zintzaras, E., Ioannidis, J.P.A 2008 Meta-analysis for ranked discovery datasets: Theoretical framework and empirical demonstration for microarrays, Computational Biology and Chemistry 32, 39-47 }
\author{Ivana Ihnatova }
\seealso{  \code{\link{compute.RQ}} }
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
} }

\keyword{ arith }

