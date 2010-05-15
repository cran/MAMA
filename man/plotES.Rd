\name{plotES}
\alias{plotES}
\title{
Function to do plots in combination of effect size method
}
\description{
Function plots several characteristics examined in meta-analysis with combination effect size method. 
}
\usage{
plotES(theScores,ScoresFDR,num.studies, legend.names, colors, which)
}
\arguments{
\item{theScores}{Output from function \code{zScores}}
\item{ScoresFDR}{Output from function \code{zScoreFDR}}
  \item{num.studies}{
number of studies involved in meta-analysis
}
  \item{legend.names}{
vector of names of studies, the first one should be "Combined Set"
}
  \item{colors}{
vector of colors used in plots, its length must be 1 + number of studies
}
\item{which}{subset from {1,2,3}: 1 for plot of the fraction of the genes that have a higher effect size than the threshold for the combined Z-score, but not for any of the data set specific Z-scores, 2 for plot of the number of genes and the corresponding FDR for the two sided situation and 3 for plot of the number of genes that are below a given threshold for the FDR}
}

\author{
Ivana Ihnatova
}
    
\seealso{
\code{\link{zScores}}, \code{\link{zScoreFDR}}
}
\examples{

## The function is currently defined as
function(theScores, ScoresFDR, num.studies, legend.names, colors)
{
# par(mfrow=c(1,3))
 IDRplot(theScores, Combine = 1:num.studies, colPos = colors[2], 
 colNeg = colors[3])
# savePlot("ES2 IDRplot.png", type="png")
 FDRwholeSettwo <- sort(ScoresFDR$two.sided[, "FDR"])
 experimentstwo <- list()
 for (j in 1:num.studies) 
 {experimentstwo[[j]] <- sort(ScoresFDR$two.sided[, 
 paste("FDR_Ex_", j, sep = "")])}
 theNewC<-colors[2:length(colors)]
 plot(FDRwholeSettwo, pch = ".", col = colors[1], ylab = "FDR", 
 xlab = "Number of genes")
 for (j in 1:num.studies) points(experimentstwo[[j]], pch = ".", 
 col = theNewC[j])
 legend(x="bottomright", legend=legend.names, 
 col=c(colors[1], theNewC[1:num.studies]), pch=19)
# savePlot("ES3 FDR.png", type="png")
 CountPlot(ScoresFDR, Score = "FDR", kindof = "two.sided", 
 cols = colors, main = "two sided FDR", 
 xlab = "FDR threshold", ylab = "Number of genes")
 legend(x=0.07,y=600, legend=legend.names, col=colors, pch="*")
# savePlot("ES4 countplot.png",type="png")
  }
}

\keyword{ hplot }
