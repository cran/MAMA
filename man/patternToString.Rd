\name{patternToString}
\Rdversion{1.1}
\alias{patternToString}
\title{Function to convert rows of a matrix to strings}
\description{
Function takes a matrix and converts rows of it to strings -  One string per row. 
}
\usage{
patternToString(X.discret)
}
\arguments{
  \item{X.discret}{Matrix}
}
\details{Originally part of MAP-Matches implementation
}
\value{
Matrix with same number of rows as input, but with rows converted to strings.
}
\references{
Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices, vol.7:3
}
\author{
Codes provided by Xinan Yang <xnyang@seu.edu.cn>
}

\examples{
A<-matrix(c(0,1,0,0,1,1), ncol=4, nrow=5)
patternToString(A)
#another example
uni<-c("0101","1100","0011","0100")
A<-patternmatrix(uni,3)
patternToString(A)
}
\keyword{ manip}

