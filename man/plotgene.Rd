\name{plotgene2}
\alias{plotgene2}
\alias{plotgene}
\title{
Function to visuaze change in expression of one gene
}
\description{
Various methods for meta-analysis provide different outputs. Function takes an output from function \code{metagene} as input and draws a plot. 
}
\usage{
plotgene2(gene, datalabels, type)
plotgene(gene, datalabels, type, col=c("green", "red"),cex=c(0.7),sig=0.05)
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
 \item{col}{colors for unsignificant/significant}
 \item{cex}{Font size for labels in unsignificant/significant part of the chart}
 \item{sig}{Significance threshold for p-values graph} 
}
\details{
Function \code{plotgene2} is based on traditional graphics, wheras function \code{plotgene} on grid.

For \code{type} please use:
\itemize{
\item{0}{ for functions: \code{pvalcombination}, \code{pvalcombination.paired}, \code{EScombination} or \code{EScombination.paired}, \code{metaMA}}
\item{1}{ for function \code{ES.GeneMeta}}
\item{2}{ for function \code{zScores}}
\item{3}{ for function \code{ScoresFDR}}
\item{4}{ for function \code{performSOGL}}
\item{5}{ for function \code{topGene} or \code{RankProduct}}
\item{6}{ for function \code{posterior.mean} }
\item{7}{ for function \code{MAP.genes} or \code{MAP.Matches}}
\item{8}{ for function \code{MC}}
\item{9}{ for function \code{compute.RQ}}
\item{10}{ for funtion \code{METRADISC}}
}

}

\author{
Ivana Ihnatova
}

\keyword{ hplot }
