\name{plotpattern}
\alias{plotpattern}

\title{
Function to do plot signifficance of Meta-Analysis Patterns
}
\description{
Function plots signifficance of Meta-Analysis Patterns
}
\usage{
plotpattern(intx, method)
}
\arguments{
  \item{intx}{
A data frame with rows refering to Meta-Analysis Patterns and columns (from 5th to 8th) to signifficance of observed  pattern matches
}
  \item{method}{
Either number 1 or 2, otherwise no plot is provided. If 1 a line plot is made. If 2 a form of scatterplot is drawn. 
}
}
\references{Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices}
\author{
Ivana Ihnatova
}
\examples{

## The function is currently defined as
function(intx, method)
{
if (method==1) {
 matplot(t(intx)[5:8,],type="l",ylab="pvalue",xlab="pattern ID", 
 lwd=3, lty=c(1,2,3,5))
 legend(x="topleft",colnames(resx)[5:8],lty=c(1,2,3,5),
 col=1:4,lwd=2)
 abline(h=0.05,col="red")}
if (method==2) 
{
lay=matrix(c(1:5), ncol=1)
layout(lay)
#layout.show(3)
for (i in 5:7) {
plot(1,type="n", axes=F, xlab="", ylab=names(intx[i]),xlim=c(0,max(intx[,5:8])))
for (j in 1:nrow(intx)) {
    points(x=intx[j,i],y=1, pch = 19, cex=2, col = j )
    abline(h=1,col="black")
    }
  }
  plot(1,type="n", yaxt="n", frame.plot=FALSE,xlab="", ylab=names(intx[i]),xlim=c(0,max(intx[,5:8])))
  for (j in 1:nrow(intx)) { points(x=intx[j,8],y=1, pch = 19, cex=2, col = j )}
  abline(h=1,col="black")
#legend
plot(1, type="n", axes=F, xlab="", ylab="")
legend(x="center", legend=c(as.character(intx[,1])), col=1:nrow(intx), pch=19, horiz=TRUE, bty="n",title="Pattern")
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }

