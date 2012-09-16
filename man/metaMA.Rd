\name{metaMA}
\alias{metaMA}
\alias{metaMA.res}
\title{
Wrapper function for effect size or p-value combination methods
}
\description{
This is a wrapper function for effect size or p-value combination as implemented in metaMA package.
}
\usage{
metaMA(data, varname, moderated = c("limma", "SMVar", "t")[1], BHth = 0.05, which = c("pval", "ES")[1])
}
\arguments{
  \item{data}{MetaArray object}
  \item{varname}{Character String - name of one column in clinical data matrices to be used as class labels}
  \item{moderated}{Character - method to calculate the test statistic (or p-value) inside each study, one of: "limma", "SMVar" and "t"
}
  \item{BHth}{Numeric - threshold for Benjamini Hochenberg adjusted p-values for selection of significant genes in meta-analysis}
  \item{which}{Character - choose \code{"pval"} for combination of p-values, or \code{"ES"} for effect sizes
}
}

\value{
An object of class \code{"metaMA.res"}. It is a list where:
\item{Study1}{Vector of indices of differentially expressed genes in study 1. Similar names are given for the other individual studies.}
\item{AllIndStudies}{Vector of indices of differentially expressed genes found by at least one of the individual studies.}
\item{Meta}{Vector of indices of differentially expressed genes in the meta-analysis.}
\item{TestStatistic}{Vector with test statistics for differential expression in the meta-analysis.}
}
\references{Marot, G., Foulley, J.-L., Mayer, C.-D., Jaffrezic, F. Moderated effect size and p-value combinations for microarray meta-analyses. }
\author{
Ivana Ihnatova
}

\seealso{
\code{\link{pvalcombination}}, \code{\link{EScombination}}
}
\examples{
data(ColonData)
pv<-metaMA(ColonData, "satelite", moderated = "t")
  }

\keyword{ univar }
