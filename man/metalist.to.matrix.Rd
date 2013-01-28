\name{metalist.to.matrix}
\Rdversion{1.1}
\alias{metalist.to.matrix}

\title{
Function to do convert list to matrix
}
\description{
Function converts list (output from \code{pvalcombination}, \code{EScombination}, \code{metaMA}) to matrix. )
}
\usage{
metalist.to.matrix(list, genenames = NULL)
}
\arguments{
  \item{list}{
output from \code{pvalcombination}, \code{EScombination}}
  \item{genenames}{
vector of gene names in same order like in expression set for \code{pvalcombination}, can be \code{NULL} if the wrapper function \code{metaMA} was used. 
}
}
\value{
Matrix. Last columns contains test statistics (last slot from metalist). Other columns are binary vector indicating that the index of the gene was present in other slots of metalist. 
}
\author{
Ivana Ihnatova
}
\examples{
data(Singhdata)
pvalt<-pvalcombination(
  esets=Singhdata$esets, 
  classes=Singhdata$classes, 
  moderated = "t", BHth = 0.01)
xx<-metalist.to.matrix(pvalt,Singhdata$geneNames)
}

\keyword{ manip}

