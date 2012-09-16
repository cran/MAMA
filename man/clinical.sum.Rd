\name{clinical.sum}
\alias{clinical.sum}
\title{
Function to calculate summaries of clinical data
}
\description{
Function calculates summaries of clinical data in object of class MetaArray. Absolute and relative frequencies of factors and descriptive statistic (minimum, median, mean, quartiles, maximum) are provided for continuous variables. Overall summaries for all datasets are also provided.
}
\usage{
clinical.sum(x)
}
\arguments{
  \item{x}{An object of class MetaArray}
}

\value{

 \item{absolute}{A list of absolute frequencies or desciptive statistics, one slot refers to one variable }
 \item{realative}{A list of relative frequencies, one slot refers to one variable}

}

\author{
Ivana Ihnatova
}
\examples{
data(ColonData)
clinical.sum(ColonData)
}
\keyword{ univar }

