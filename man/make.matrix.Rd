\name{make.matrix}
\Rdversion{1.1}
\alias{make.matrix}
\title{Function to make matrix for heatmap to compare results of several methods}
\description{
\code{make.matrix} returns matrix of 1 and 0 with gene names as rows and methods as colums. 1 refers to the gene that was found as differentialy expressed by the method, othterwise 0 is used.}
\usage{
make.matrix(lists)
}
\arguments{
  \item{lists}{list of vectors. Each vector refers to a method and contains names of significant genes}
}
\value{
Binary matrix with gene names as rows and methods as colums. 
}
\author{Ivana Ihnatova}
\examples{
lists<-list(Method1=c("Gene_A", "Gene_V","Gene_S","Gene_C","Gene_U","Gene_D","Gene_E","Gene_G","Gene_W"), 
  Method2=c("Gene_D","Gene_W","Gene_G","Gene_E","Gene_H","Gene_X"),
  Method3=c("Gene_L","Gene_K","Gene_J","Gene_M","Gene_V","Gene_T","Gene_R","Gene_U"))
make.matrix(lists)
}
\keyword{dplot}


