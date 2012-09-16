\name{Z}
\Rdversion{1.1}
\alias{Z}
\title{
Function to compute Z-statistics in contingency table
}
\description{
Function to compute Z-statistics in contingency table as in VennMapper program.
}
\usage{
Z(lists, n.genes)
}
\arguments{
  \item{lists}{list of vectors. Each vector refers to a method and contains names of significant genes}
    \item{n.genes}{Number of genes in meta-analysis (number of genes on microarray slide)}
}
\details{
Simmilar to \code{conting.tab} and \code{gene.list}, but provides different output
}
\value{
Matrix of Z-statistics as defined in see references.
}
\references{
Smid, M., Dorssers, L. C. J. and Jenster, G. 2003, Venn Mapping: clustering of heterologous microarray data based on the number of co-occurring differentially expressed genes, Bioinformatics, vol. 19 no. 16 2003}
\author{Ivana Ihnatova}
\seealso{ \code{\link{conting.tab}}, \code{\link{gene.list}} }
\examples{
lists<-list(Method1=c("Gene_A", "Gene_V","Gene_S","Gene_C","Gene_U","Gene_D","Gene_E","Gene_G","Gene_W"), 
  Method2=c("Gene_D","Gene_W","Gene_G","Gene_E","Gene_H","Gene_X"),
  Method3=c("Gene_L","Gene_K","Gene_J","Gene_M","Gene_V","Gene_T","Gene_R","Gene_U"))
z<-Z(lists, n.genes=10000)
}
\keyword{ htest }