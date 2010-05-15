\name{plotgene}
\Rdversion{1.1}
\alias{plotgene}
\title{
Function to plot various characteristics of gene expression
}
\description{
Funtions plots part of \code{metagene} output as heatmap, scatter plot and forest plot
}
\usage{
plotgene(gene, datalabels = c("denmark", "australia", "japan", "combined"))
}

\arguments{
  \item{gene}{
gene expression characteristics - output from \code{metagene}
}
  \item{datalabels}{
names of datasets for forest plot
}
}

\author{
Ivana Ihnatova
}

\examples{
\dontrun{
gene<-metagene(1,results) 
plot(gene)
}
}
\keyword{ hplot }

