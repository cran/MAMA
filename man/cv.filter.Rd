\name{cv.filter}
\Rdversion{1.1}
\alias{cv.filter}
\title{Microarray probes filtering}
\description{Function to filter microarray probes according to coefficient of variation}
\usage{
cv.filter(data, cutoff = 0.05)
}

\arguments{
  \item{data}{expression matrix with probes in rows and samples in columns}
  \item{cutoff}{cutoff value for filtering}
}

\value{Expression matrix, probes with CV below cutoff are filtered out.}

\author{Ivana Ihnatova}



\examples{
data(Singhdata)
data<-Singhdata$esets[[1]][1:1000,]
data.filtered<-cv.filter(data)
head(data.filtered)
}
\keyword{ manip}

