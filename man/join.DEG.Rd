\name{join.DEG}
\alias{join.DEG}
\title{
Function to join vectors of differentially expressed genes to one list
}
\description{
The function takes outputs from meta-analysis of microarrays, extracts names of differentially expressed genes from them and joins these names into one list, where each slot refer to one output. 
}
\usage{
join.DEG(..., genenames, type = NULL, cutoff)
}

\arguments{
  \item{\dots}{
Outputs from different function for methods of meta-analysis of microarray
}
  \item{genenames}{
a character vector - names of all genes (or probe ID) included in meta-analysis = rownames of gene expression data matrix
}
  \item{type}{
a numeric vector idicating from which function the output is, \code{k}th element in \code{type} corresponds to \code{k}th element of \code{\dots}. It is not needed when wrapper functions where used.
}
  \item{cutoff}{
a numeric value - a cutoff level for p-value to select significant genes
}
}
\details{
Values below have to be used in \code{type}.
\itemize{
\item{1}{for functions: \code{pvalcombination}, \code{pvalcombination.paired}, \code{EScombination} or \code{EScombination.paired}}
\item{2}{for function \code{zScores}}
\item{3}{for function \code{ScoresFDR}}
\item{4}{for function \code{performSOGL}}
\item{5}{for function \code{topGene}}
\item{6}{for function \code{z.stat}}
\item{7}{for function \code{tspcalc}}
\item{8}{for function \code{MAP.genes}}
}}
\value{
A list in which each slot refers to one meta-analytical method and contains names of differentially expressed genes found by the method. 
}

\author{
Ivana Ihnatova
}


\keyword{ manip}

