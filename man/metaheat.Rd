\name{metaheat}
\alias{metaheat}

\title{ Display Data as Heatmap}
\description{
This function displays a matrix as a heatmap. It is based on function \code{heatmap_2} in the \code{Heatplus} package. 
}
\usage{
metaheat(x, Rowv = NA, Colv = NA, distfun = dist, hclustfun = hclust, na.rm = TRUE, do.dendro = c(TRUE, TRUE), legend = 0, legfrac = 8, col = heat.colors(12), ...)
}

\arguments{
  \item{x}{the numerical data matrix to be displayed}
  \item{Rowv}{either a dendrogram or a vector of reordering indexes for the rows, setting to \code{NA} suppresses re-ordering of rows }
  \item{Colv}{either a dendrogram or a vector of reordering indexes for the columns, setting to \code{NA} suppresses re-ordering of columns  }
  \item{distfun}{ function to compute the distances between rows and columns. Defaults to \code{dist}}
  \item{hclustfun}{function used to cluster rows and columns. Defaults to \code{hclust} }
  \item{na.rm}{logical indicating whther to remove NAs }
  \item{do.dendro}{logical vector of length two, indicating (in this order) whether to draw the row and column dendrograms }
  \item{legend}{integer between 1 and 4, indicating on which side of the plot the legend should be drawn: 1=bottom, 2=left, 3=above, 4=right }
  \item{legfrac}{fraction of the plot that is taken up by the legend; larger values correspond to smaller legends }
  \item{col}{the color scheme }
  \item{\dots}{ extra arguments to \code{image}}
}
\author{ Ivana Ihnatova}
\examples{
lists<-list(Method1=c("Gene_A", "Gene_V","Gene_S","Gene_C","Gene_U","Gene_D","Gene_E","Gene_G","Gene_W"), 
  Method2=c("Gene_D","Gene_W","Gene_G","Gene_E","Gene_H","Gene_X"),
  Method3=c("Gene_L","Gene_K","Gene_J","Gene_M","Gene_V","Gene_T","Gene_R","Gene_U"))
A<-make.matrix(lists)
metaheat(A, legend=1, col=c(3,4))
}
\keyword{ hplot}
