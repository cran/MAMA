\name{MetaArray-class}
\Rdversion{1.1}
\docType{class}
\alias{MetaArray-class}
\alias{plot,MetaArray,missing-method}
\alias{plot,MetaArray,ANY-method}
\alias{print,MetaArray-method}
\alias{show,MetaArray-method}
\alias{as.list,MetaArray-method}
%\alias{GEDM<-, MetaArray-method}
%\alias{clinical<-MetaArray-method}
%\alias{datanames<-MetaArray-method}

\title{Class "MetaArray" ~~~ }
\description{	A class created for meta-analysis of microarray}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MetaArray", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{GEDM}:}{Object of class \code{"list"} - gene expression data matrices are stored in individual slot of the list. Each matrix refer to one dataset, genes are represented in rows, samples in columns. }
    \item{\code{clinical}:}{Object of class \code{"list"} - clinical data matrices, clinical description of samples, rows refer to samples, columns to clinical characteristics}
    \item{\code{datanames}:}{Object of class \code{"character"} - vector of names of the datasets }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "MetaArray", y = "missing")}: draws distribution of clinical variables of several datasets. Boxplot is drawn for numerical variables and barplot for categorical ones. }
    \item{print}{\code{signature(x = "MetaArray")}: prints the number of samples and genes in each dataset, followed by summarization of each clinical characteristic of the samples }
    \item{show}{Same as \code{print}}
    \item{as.list}{Function transforms a MetaArray object into a list, in which each slot is again a list of three slots: gene expression data matrix \code{GEDM}, clinical data \code{clinical}, name of the dataset \code{dataname}}

	 }
	 
}

\author{ Ivana Ihnatova }


\examples{
showClass("MetaArray")
}
\keyword{classes}
