\name{VennMapper}
\alias{VennMapper.res}
\alias{VennMapper}
\title{
Wrapper function for VennMapping
}
\description{
This is a wrapper function for meta-analysis using VennMapping method. It performs all necessary steps and provides all available outputs.
}
\usage{
VennMapper(data, varname, cutoff, ngenes)
}
\arguments{
  \item{data}{MetaArray object}
  \item{varname}{Character String - name of one column in clinical data matrices to be used as class labels}
  \item{cutoff}{Numeric - cutoff for selection of genes according to their fold-change in log2-scale. e.g. \code{1} equals to two-fold expression change}
  \item{ngenes}{Numer of genes involved in the analysis}
}

\value{
An object of class \code{VennMapper.res} containing
  \item{conting.tab}{A contingency table with numbers of overlapping genes in pairs of the datasets}
  \item{z.score}{A table of z-scores describing the significance of overlap in pairs of the datasets}
  \item{genes}{A table of gene names that overlap in pairs of the datasets}
}
\references{  Smid, M., Dorssers, L. C. J. and Jenster, G. 2003, Venn Mapping: clustering of heterologous microarray data based on the number of co-occurring differentially expressed genes, Bioinformatics, vol. 19 no. 16 2003
}
\author{
Ivana Ihnatova
}

\examples{
data(ColonData)
vm<-VennMapper(ColonData, "satelite", 1, 500)
}
\keyword{nonparametric }

