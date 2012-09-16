\name{posterior.mean}
\alias{posterior.mean}
\title{
Function to calculate posterior mean differential expression
}
\description{
Function calculates posterior mean differential expression in form of Z-score and its p-value as described in Wang et al., 2004. 
}
\usage{
posterior.mean(data, varname, nsamp, permute = 0)
}
\arguments{
  \item{data}{An MetaArray object}
  \item{varname}{A string indicating which column of clinical data matrices 
  should be used to compute test statistic. Same column is used in all datasets.}
  \item{nsamp}{Number of samples. It is suggested to use same number of samples in each class and dataset.}
  \item{permute}{If permute is 0, weighted Z-score will be referenced to standard normal distribution for two-sided p-value. Otherwise, columns of all datasets (each dataset separately) will be shuffled at random, from which a permutation distribution of Z-scores are formed and Z-scores are referenced to this distribution.}
}
\details{
The main idea of this method is that one can use data from one study to con-
struct a prior distribution of differential expression and thus utilize the posterior
mean differential expression, weighted by variances, whose distribution is stan-
dard normal distribution due to classic Bayesian probability calculation. 
It is based on assumption that gene expression is normally distributed and that we can estimate the standard deviation of this distribution by pooling together all genes with similar levels of mean expression.}
\value{
Object of class \code{posterior.mean}. It is a data frame, where the first column contain Z-scores, the second p-values, rows refer to genes.
}
\references{
 Wang, J., Coombes, K. R., Highsmith, W. E., Keating, M. J. a Abruzzo, L.
V. 2004, Differences in gene expression between B-cell chronic lymphocytic
leukemia and normal B cells: a meta-analysis of three microarray studies
}
\author{
Ivana Ihnatova
}


\examples{
data(Singhdata)

cl1<-as.data.frame(Singhdata$classes[[1]])
names(cl1)<-"classlab"
cl2<-as.data.frame(Singhdata$classes[[2]])
names(cl2)<-"classlab"
cl3<-as.data.frame(Singhdata$classes[[3]])
names(cl3)<-"classlab"
rownames(Singhdata$esets[[1]])<-Singhdata$geneNames
rownames(Singhdata$esets[[2]])<-Singhdata$geneNames
rownames(Singhdata$esets[[3]])<-Singhdata$geneNames

data<-new("MetaArray", GEDM=list(Singhdata$esets[[1]], Singhdata$esets[[2]], Singhdata$esets[[3]]),
clinical=list(cl1, cl2, cl3), datanames=c("dataset1", "dataset2", "dataset3"))

pm<-posterior.mean(data, "classlab", 4)
head(pm)
}

\keyword{ univar }