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


\keyword{ hplot }
