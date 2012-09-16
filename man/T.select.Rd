\name{T.select}
\alias{T.select}
\title{
Function to help with selection of threshold for T-statistics
}
\description{
Function calculates quantiles of T-statistics to help with selection of threshold for it as part of MAP-Matches method. 
}
\usage{
T.select(stat, fig = TRUE)
}
\arguments{
  \item{stat}{
Vector of T-statistics
}
  \item{fig}{
If \code{TRUE} a plot of quantiles and sequence from 0.97 to 0.98 is provided.
}
}

\value{
A vector of T-statistics quantiles.
}

\author{
Ivana Ihnatova
}

\examples{
## The function is currently defined as
function(stat,fig=TRUE)
{
 quan <- quantile(abs(stat),seq(0.97,0.98,.0001))
 x <- seq(0.97,0.98,1e-04)
if (fig) {
 plot(x,quan,type="b",xlab="percent",ylab="t")
 z <- lm(quan ~ x)
 abline(z,col="red")
 points(0.9787,quan["97.87\%"],pch=19,cex=1.5,col="red")
 }
return(quan)
  }
}

\keyword{ univar }
\keyword{ hplot }
