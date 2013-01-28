\name{performSOGL}
\alias{performSOGL}
\title{
Function to perform analysis using Similarity of Ordered Gene Lists
}
\description{
This a wrapper function to perform all steps designed in Similarity of Ordered Gene Lists.
}
\usage{
performSOGL(data, varname, test, B, which = c("score", "empirical"), min.weight = 1e-05, two.sided = TRUE, percent = 0.95)
}
\arguments{
  \item{data}{MetaArray object, the rownames in the gene expression data matrices must be equal}
  \item{varname}{A string indicating which column of clinical data matrices 
  should be used to compute test statistic. Same column is used in all datasets.}
  \item{test}{\code{"FCH"} for fold change (function \code{fold.change}) 
  or \code{"T"} for T-test (function \code{meta.test}) }
  \item{B}{Number of permuatations}
  \item{which}{if \code{"empirical"} then empirical confidence intervals of number of overlapping genes are also provided, if \code{"score"} only random and subsampled scores necessary for tunning alpha parameter are calculated}
  \item{min.weight}{Minimal weight for score calculation}  
  \item{two.sided}{if \code{TRUE} both top and bottom of the ordered gene lists are considered, if \code{FALSE} only top ones}
  \item{percent}{Percentage (Numeric between \code{0} and \code{1}) of the score for genes selection }
}

\value{
Object of class SOGLresult, it is a list containig:
 \item{ordering}{Ordered Gene Lists as a data.frame where columns refer to datasets}
 \item{alpha.selected}{Selected value of alpha parameter}
 \item{alpha.considered}{Vector of alpha considered for selection}
 \item{pAUC}{pAUC values related to all alphas considered}
 \item{random}{Random scores (permutations of class labels)}
 \item{subsample}{Scores after subsampling from each class and dataset}
 \item{emp.ci}{Empirical confidence intervals for number of overlapping genes}
 \item{common.genes}{Vector of number of overlapping genes}
 \item{score}{Observed similarity score}
 \item{significance}{Significance of the observed score in form of p-value}
 \item{genes}{Genes that account for observed similarity score}
 \item{all.genes}{Names of the all genes in the analysis}
}
\references{
Yang, X., Bentink, S., Scheid, S. Spang, R., Similarities of ordered gene lists, 2005
}
\author{Ivana Ihnatova}


\keyword{ univar }
