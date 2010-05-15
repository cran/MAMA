\name{plotclinical}
\Rdversion{1.1}
\alias{plotclinical}
\title{Plot of clinical variables}
\description{
\code{plotclinical} draws distribution of clinical variables of several datasets. Boxplot is drawn for numerical variables and barplot for categorical ones.  
}
\usage{
plotclinical(DATA, nam = names(DATA[[1]]), col = c("darkolivegreen3", "darkorange1", "lightsalmon3", "khaki1"))
}
\arguments{
  \item{DATA}{list of dataframes, each data frame refers to one data set and contains same clinical variables. 
  NAs are used if a variable is unavaiable for that dataset and no-plot is drawn.}
  \item{nam}{vector of names of clinical variables to be drawn}
  \item{col}{vector of colors for ploting barplots}
}

\author{Ivana Ihnatova}

\examples{
\dontrun{plotclinical(DATA)}
\dontrun{
data( Loblolly)
data1<-Loblolly[1:20,]
data2<-Loblolly[30:50,]
data3<-Loblolly[60:80,]
DATA<-list(data1=data1, data2=data2,data3=data3,total=rbind(data1,data2,data3))
plotclinical(DATA)
}         }
\keyword{ hplot }

