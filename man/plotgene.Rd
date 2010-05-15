\name{plotgene}
\alias{plotgene}
\title{
Function to visuaze change in expression of one gene
}
\description{
Various methods for meta-analysis provide different outputs. Function takes an output from function \code{metagene} as input and draws a plot. 
}
\usage{
plotgene(gene, datalabels, type)
}
\arguments{
  \item{gene}{
A list, output from function \code{metagene}
}
  \item{datalabels}{
A character vector, names of the data sets and for meta-analysis results 
}
  \item{type}{
A numeric vector indicating which function the slots of \code{gene} come from
}
}
\details{
For \code{type} please use:
\itemize{
\item{1}{for functions: \code{pvalcombination}, \code{pvalcombination.paired}, \code{EScombination} or \code{EScombination.paired}}
\item{2}{for function \code{zScores}}
\item{3}{for function \code{ScoresFDR}}
\item{4}{for function \code{OrderedList}}
\item{5}{for function \code{topGene}}
\item{6}{for function \code{z.stat}}
\item{7}{for function \code{MAP.genes}}
\item{8}{for function \code{MC}}
\item{9}{for function \code{compute.RQ}}
}

}

\author{
Ivana Ihnatova
}
\examples{

## The function is currently defined as
function(gene, datalabels, type)
{
#pvalt, ESt, theScores, ScoresFDR$two.sided, x.z, RankRes, z.stat, probs, MC, RQ

gen=gene
laymat<-matrix(c(1,2,2,3,3),ncol=1)
lay<-layout(laymat)
#layout.show(lay)
dum1<-logical()
for (i in which(type==1)) 
dum1<-c(dum1,gene[[i]][1:(length(gene[[i]])-1)])
for (i in which(type==4|type==7)) 
dum1<-c(dum1,gene[[i]])
#dum.nam<-c("SOGL.z", "SOGL.fc", "SOGL.t", paste("MAP",names(gen$MAP)))
dum.nam=names(dum1)
par(mar=c(2,6,4,10))
pos<-1:(length(names(dum1)))
image(pos,1,as.matrix(dum1),
col=c("grey75", "grey25"), xlab="", ylab="", 
axes=F, xlim = 0.5 + c(0, length(names(dum1))))
axis(3, at=pos, labels = dum.nam, las = 2, line = -0.5, tick = 0, 
        cex.axis = 0.7)

x<-numeric()
labels<-character()
sta<-character()
for (i in which(type==6)) {
 x<-c(x, gene[[i]][2]); 
 labels=c(labels,"z.stat")
 sta<-c(sta,paste("z.stat:",round(gene[[i]][1],2)))
 }
for (i in which(type==5)) {
 x<-c(x, gene[[i]][4]); 
 labels=c(labels,"RankProd")
 sta<-c(sta,paste("RankProd.down:",round(gene[[i]][2],2)))
 }
for (i in which(type==8)) {
 x<-c(x, gene[[i]])   ; 
 labels=c(labels,names(gene[[i]]))
 }
for (i in which(type==9))
sta<-c(sta,paste("Average Rank:",round(gene[[i]][1],2)), paste("Average Rank:",round(gene[[i]][1],2)),
 paste("Heterogenity:",round(gene[[i]][2],2)), paste("Heterogenity:",round(gene[[i]][2],2))
)
x<-unlist(x)
par(mar=c(5,6,2,10))
plot(x=x,y=1:length(x),pch=19,xlab="p-value", ylab="",ylim=0.5+c(0,length(x)), 
 xaxt="s", yaxt="n",frame.plot=TRUE)
axis(2, at=c(1:length(x)), labels =labels, las = 2, line = -0.5, tick = 0, 
        cex.axis = 0.8)
axis(1, at=1, labels = , las = 2, line = -0.5, tick = 0, 
        cex.axis = 0.7)
abline(v=0.05,lty=3)
axis(4, at=c(1:length(x)), 
 labels = sta, las = 2, line = -0.5, tick = 0, cex.axis = 0.8)

par(mar=c(5,6,2,10))
for (i in which(type==2))
{
me<-gene[[i]][c(11:13,5)]
va<-gene[[i]][c(14:16,6)]
plot(x=me,y=1:4,pch=19,xlab="Effect Size", ylab="",ylim=0.5+c(0,4),
 xlim=c(min(me-va), max(me+va)), xaxt="s", yaxt="n",frame.plot=TRUE)
for (i in 1:4) lines(x=c(me[i]-va[i],me[i]+va[i]),y=c(i,i))
axis(2, at=c(1:4), 
 labels = datalabels, las = 2, line = -0.5, tick = 0, cex.axis = 0.8)
  }
for (i in which(type==3))
{
fd<-paste("FDR.twosided:",round(gene[[i]][c(2,4,6,8)],3))
axis(4, at=c(1:4), labels=fd, las = 2, line = -0.5, tick = 0, cex.axis = 0.8)
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
