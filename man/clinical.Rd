\name{clinical}
\alias{clinical}
\alias{clinical<-, MetaArray-method}
\alias{clinical<-}
\alias{GEDM}
\alias{GEDM<-, MetaArray-method}
\alias{GEDM<-}
\alias{datanames}
\alias{datanames<-, MetaArray-method}
\alias{datanames<-}
\title{
Functions to retrieve and assign 
}
\description{
Functions access the individual slots of an object derived from 'MetaArray' class.
}
\usage{
clinical(object)
clinical(object)<-value

GEDM(object)
GEDM(object)<-value

datanames(object)
datanames(object)<-value
}

\arguments{
  \item{object}{An object derived from 'MetaArray' class}
  \item{value}{A list of gene expression data matrices, clinical data matrices or a vector of data names}
}

\value{
'clinical' returns the list of clinical data matrices, one for each data set;
'GEDM' returns the list of gene expression data matrices, one for each data set;
'datanames' returns the vector of data sets names
}
\author{Ivana Ihnatova}

\keyword{manip}

