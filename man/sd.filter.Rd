\name{sd.filter}
\Rdversion{1.1}
\alias{sd.filter}
\title{Microarray probes filtering}
\description{Function to filter microarray probes according to standard deviation}
\usage{
sd.filter(data, cutoff = 0.5)
}
\arguments{
  \item{data}{expression matrix with probes in rows and samples in columns}
  \item{cutoff}{cutoff value for filtering}
}
\value{Expression matrix, probes with SD below cutoff are filtered out.}
\author{Ivana Ihnatova}
\examples{
data(Singhdata)
data<-Singhdata$esets[[1]][1:1000,]
data.filtered<-sd.filter(data)
head(data.filtered)
}

\keyword{ manip }
