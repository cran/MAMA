\name{join.DEG}
\alias{join.DEG}
\title{
Function to join vectors of differentially expressed genes to one list
}
\description{
The function takes outputs from meta-analysis of microarrays, extracts names of differentially expressed genes from them and joins these names into one list, where each slot refer to one output. 
}
\usage{
join.DEG(..., genenames, type, cutoff)
}

\arguments{
  \item{\dots}{
Outputs from different function for methods of meta-analysis of microarray
}
  \item{genenames}{
a character vector - names of all genes (or probe ID) included in meta-analysis = rownames of gene expression data matrix
}
  \item{type}{
a numeric vector idicating from which function the output is, \code{k}th element in \code{type} corresponds to \code{k}th element of \code{\dots}
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
\item{4}{for function \code{OrderedList}}
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

\examples{

## The function is currently defined as
function(...,genenames, type, cutoff)
{ 
args<-list(...)
N<-length(args)
if (N!=length(type)) stop ("Vector type has not correct lenght")
genelist<-list()

for (i in 1:N)
{
if (type[i]==1) {genelist[[i]]<-genenames[args[[i]]$Meta]}
if (type[i]==2) {}
if (type[i]==3) {genelist[[i]]<-rownames(args[[i]]$two.sided)[args[[i]]$two.sided[,
 8]<cutoff]}
if (type[i]==4) {genelist[[i]]<-args[[i]]$intersect}
if (type[i]==5) {genelist[[i]]<-unique(c(rownames(args[[i]]$Table1),rownames(
 args[[i]]$Table2)))}
if (type[i]==6) {genelist[[i]]<-rownames(args[[i]])[args[[i]]$Pvalue<cutoff]}
if (type[i]==7) {genelist[[i]]<-genenames[args[[i]]$index]}
if (type[i]==8) {genelist[[i]]<-unique(unlist(args[[i]]))}
  }
return(genelist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip}

