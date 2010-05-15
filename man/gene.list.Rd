\name{gene.list}
\alias{gene.list}
\title{Intersect of gene lists}
\description{ This function takes list of gene list as input and returns a matrix of gene names common in pairs of lists
}
\usage{
gene.list(lists)
}

\arguments{
  \item{lists}{ list of vectors. Each vector refers to a method and contains names of significant genes}
}
\details{ Simmilar to \code{conting.tab} and \code{Z}, but provides different output}
\value{A matrix of gene names common in two gene lists}
\author{ Ivana Ihnatova }
\seealso{ \code{\link{conting.tab}}, \code{\link{Z}} }
\examples{
lists<-list(Method1=c("Gene_A", "Gene_V","Gene_S","Gene_C","Gene_U","Gene_D","Gene_E","Gene_G","Gene_W"), 
  Method2=c("Gene_D","Gene_W","Gene_G","Gene_E","Gene_H","Gene_X"),
  Method3=c("Gene_L","Gene_K","Gene_J","Gene_M","Gene_V","Gene_T","Gene_R","Gene_U"))
gene.list(lists)
}
\keyword{ arith}

