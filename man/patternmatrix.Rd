\name{patternmatrix}
\Rdversion{1.1}
\alias{patternmatrix}

\title{
Function to split binary vectors to matrix.
}
\description{
Funtion takes vector of binary strings (0,1) and returns matrix with strings split.)
}
\usage{
patternmatrix(unipattern, n.entity)
}

\arguments{
  \item{unipattern}{Vector of binary strings}
  \item{n.entity}{Length of strings, number of studies in original application}
}
\details{Originally part of MAP-Matches implementation}
\value{
Binary matrix with rows corresponding to input strings.}
\references{
Yang, X., Bentink, S. a Spang, R. 2005, Detecting Common Gene Expression Patterns in Multiple Cancer Outcome Entities, Biomedical Microdevices, vol.7:3
}
\author{
Codes provided by Xinan Yang <xnyang@seu.edu.cn>
}
\examples{
uni<-c("0101","1100","0011","0100")
patternmatrix(unipattern=uni,n.entity=4)
}
\keyword{manip}

