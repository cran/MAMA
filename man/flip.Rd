\name{flip}
\alias{flip}
\title{
Function to flip data frames
}
\description{
Function reverses the order of rows. It is simmilar to function \code{rev}, but designed for rows of a data frame, matrix.
}
\usage{
flip(order)
}
\arguments{
  \item{order}{Data frame, Matrix}
}

\value{Same data frame or matrix with reversed rows}
\author{Ivana Ihnatova}


\seealso{
\code{\link{rev}}
}
\examples{
 A<-matrix(1:24, ncol=4);A
 flip(A)     }
\keyword{ manip }

