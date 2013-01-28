\name{join.results}
\alias{join.results}
\title{
Function to join results from meta-analysis to one list
}
\description{
Function joins results from meta-analysis to one list. It uses predefined types of results and transform some of them. 
}
\usage{
join.results(..., type = NULL , genenames = NULL)
}

\arguments{
  \item{\dots}{
Outputs from different function for methods of meta-analysis of microarray
}
  \item{type}{
a numeric vector idicating from which function the output is, \code{k}th element in \code{type} corresponds to \code{k}th element of \code{\dots}. It can be \code{NULL}, if the wrapper functions were used.
}
  \item{genenames}{
a character vector - names of all genes (or probe ID) included in meta-analysis = rownames of gene expression data matrix. It can be \code{NULL}, if the wrapper functions were used. 
}
}
\details{
Values below have to be used in \code{type}.
\itemize{
\item{1}{for functions: \code{pvalcombination}, \code{pvalcombination.paired}, \code{EScombination} or \code{EScombination.paired}}
\item{2}{for function \code{performSOGL}}
\item{3}{for function \code{topGene}}
\item{4}{for function \code{MAP.genes}}
\item{5}{for function \code{zScores}, \code{ScoresFDR}, \code{z.stat}, \code{tspcalc}}
}
}
\value{
A list in which each slot refers to one meta-analytical method and contains a data frame with all outputs available from the method for one gene.
}
\author{
Ivana Ihnatova
}


\keyword{ manip }

