\name{conting.tab}
\Rdversion{1.1}
\alias{conting.tab}
\title{Contingency table from gene lists}
\description{Function to make a contingency table from gene lists as in VennMapper program.}
\usage{conting.tab(lists)}
\arguments{
  \item{lists}{list of vectors. Each vector refers to a method and contains names of significant genes}
}
\details{
Simmilar to \code{gene.list} and \code{Z}, but provides different output
}
\value{
Matrix with counts of matches in pairs of gene lists
}
\references{
Smid, M., Dorssers, L. C. J. and Jenster, G. 2003, Venn Mapping: clustering of heterologous microarray data based on the number of co-occurring differentially expressed genes, Bioinformatics, vol. 19 no. 16 2003}
\author{Ivana Ihnatova}
\seealso{
 \code{\link{Z}},\code{\link{gene.list} }
}
\examples{
lists<-list(Method1=c("Gene_A", "Gene_V","Gene_S","Gene_C","Gene_U","Gene_D","Gene_E","Gene_G","Gene_W"), 
  Method2=c("Gene_D","Gene_W","Gene_G","Gene_E","Gene_H","Gene_X"),
  Method3=c("Gene_L","Gene_K","Gene_J","Gene_M","Gene_V","Gene_T","Gene_R","Gene_U"))
conting.tab(lists)
}
\keyword{ manip}