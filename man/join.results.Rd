\name{join.results}
\alias{join.results}
\title{
Function to join results from meta-analysis to one list
}
\description{
Function joins results from meta-analysis to one list. It uses predefined types of results and transform some of them. 
}
\usage{
join.results(..., type, genenames)
}

\arguments{
  \item{\dots}{
Outputs from different function for methods of meta-analysis of microarray
}
  \item{type}{
a numeric vector idicating from which function the output is, \code{k}th element in \code{type} corresponds to \code{k}th element of \code{\dots}
}
  \item{genenames}{
a character vector - names of all genes (or probe ID) included in meta-analysis = rownames of gene expression data matrix
}
}
\details{
Values below have to be used in \code{type}.
\itemize{
\item{1}{for functions: \code{pvalcombination}, \code{pvalcombination.paired}, \code{EScombination} or \code{EScombination.paired}}
\item{2}{for function \code{OrderedList}}
\item{3}{for function \code{topGene}}
\item{4}{for function \code{MAP.genes}}
\item{5}{for function \code{zScores}, \code{ScoresFDR}, \code{z.stat}, \code{tspcalc}}
\item{6}{for function \code{z.stat}}
\item{7}{for function \code{tspcalc}}
\item{8}{for function \code{MAP.genes}}
}}
\value{
A list in which each slot refers to one meta-analytical method and contains a data frame with all outputs available from the method for one gene.
}
\author{
Ivana Ihnatova
}
\examples{

## The function is currently defined as
function(...,type , genenames)
{
args<-list(...)
N<-length(args)
if (N!=length(type)) stop ("Vector type has not correct length")
results<-list()

for (i in 1:N)
{
if (type[i]==1) {results[[i]]<-metalist.to.matrix(args[[i]],genenames)}
if (type[i]==2) {dum<-as.data.frame(genenames \%in\% args[[i]]$intersect)
 rownames(dum)<-genenames
 results[[i]]<-dum}
if (type[i]==3) {results[[i]]<-rbind(args[[i]]$Table1, args[[i]]$Table2)}
if (type[i]==4) {results[[i]]<-probs.to.matrix(args[[i]], genenames)}
if (type[i]==5) {results[[i]]<-args[[i]]}
  }
return(results)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

