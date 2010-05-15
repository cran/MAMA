\name{plotQvsChi}
\alias{plotQvsChi}
\title{
Function to plot quantiles of Cochran's Q statistic and Chi-square quantiles.
}
\description{
Function plots quantiles of Cochran's Q statistic and Chi-square quantiles as a scatter plot with diagonal line. Plot can help to decide between random-effect and fixed-effect model. It is a wrapper function to provide a complete plot.  
}
\usage{
plotQvsChi(Q, num.studies)
}
\arguments{
  \item{Q}{
A vector of Cochran's Q statistic used to test between-study variability.
}
  \item{num.studies}{
Number of studies involved in meta-analysis.
}
}

\author{
Ivana Ihnatova
}

\examples{
## The function is currently defined as
function(Q,num.studies)
{
 chisqq <- qchisq(seq(0, 0.9999, 0.001), df = num.studies-1)
 tmp <- quantile(my.Q, seq(0, 0.9999, 0.001))
 qqplot(chisqq, tmp, ylab = "Quantiles of Sample", pch = "*", 
 xlab = "Quantiles of Chi > square", main = "QQ Plot")
 lines(chisqq, chisqq, lty = "dotted", col = "red")
  }
}

\keyword{ hplot }

