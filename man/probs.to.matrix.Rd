\name{probs.to.matrix}
\Rdversion{1.1}
\alias{probs.to.matrix}
\title{
Function to convert list to matrix
}
\description{
Function converts list to binary matrix
}
\usage{
probs.to.matrix(probs, genenames)
}
\arguments{
  \item{probs}{
list of vectors of gene names/ character strings
}
  \item{genenames}{
vector of all gene names in analysis / all strings to be considered
}
}

\value{
matrix in which rows refer to genes (character strings) and columns to slots of input list
}

\author{
Ivana Ihnatova
}

\seealso{
 \code{\link{metalist.to.matrix}}
}
\examples{

lists<-list(Method1=c("Gene_A", "Gene_V","Gene_S","Gene_C","Gene_U","Gene_D","Gene_E","Gene_G","Gene_W"), 
  Method2=c("Gene_D","Gene_W","Gene_G","Gene_E","Gene_H","Gene_X"),
  Method3=c("Gene_L","Gene_K","Gene_J","Gene_M","Gene_V","Gene_T","Gene_R","Gene_U"))
genes<-c("Gene_A", "Gene_V", "Gene_S", "Gene_C", "Gene_U", "Gene_D", "Gene_E", "Gene_G",
 "Gene_W", "Gene_H", "Gene_X", "Gene_L", "Gene_K", "Gene_J", "Gene_M", "Gene_T",
 "Gene_R")
PM<-probs.to.matrix(lists,genes)
PM
   }     
\keyword{ manip }

