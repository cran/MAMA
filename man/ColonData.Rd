\name{ColonData}
\alias{ColonData}
\docType{data}
\title{
Example dataset for meta-analysis of microarray
}
\description{
This is an example dataset for meta-analysis of microarray. It has been created from three datasets form Gene Expression Omnibus (GSE13067, GSE13294 and GSE4554). The data have been normalized, log2-transformed and only random selection of 500 gene is included.
}
\usage{data(ColonData)}
\format{
  The format is:
Formal class 'MetaArray' [package "MAMA"] with 3 slots
  ..@ GEDM     :List of 3
  .. ..$ : num [1:500, 1:77] 2.49 3.87 2.95 6.39 6.06 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:500] "217562_at" "203766_s_at" "1554394_at" "212662_at" ...
  .. .. .. ..$ : chr [1:77] "GSM335574" "GSM335645" "GSM335546" "GSM335623" ...
  .. ..$ : num [1:500, 1:36] 3.24 5.32 3.24 7.24 4.75 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:500] "217562_at" "203766_s_at" "1554394_at" "212662_at" ...
  .. .. .. ..$ : chr [1:36] "GSM327331" "GSM327282" "GSM327313" "GSM327353" ...
  .. ..$ : num [1:500, 1:41] 0.595 3 1.618 4.668 2.887 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:500] "217562_at" "203766_s_at" "1554394_at" "212662_at" ...
  .. .. .. ..$ : chr [1:41] "GSM101849" "GSM101851" "GSM101857" "GSM101799" ...
  ..@ clinical :List of 3
  .. ..$ :'data.frame':	77 obs. of  1 variable:
  .. .. ..$ satelite: Factor w/ 2 levels "MSI","MSS": 1 1 1 1 1 1 1 1 1 1 ...
  .. ..$ :'data.frame':	36 obs. of  1 variable:
  .. .. ..$ satelite: Factor w/ 2 levels "MSI","MSS": 1 1 1 1 1 2 2 2 2 2 ...
  .. ..$ :'data.frame':	41 obs. of  2 variables:
  .. .. ..$ position: Factor w/ 3 levels "distal","proximal",..: 2 1 2 2 1 3 2 2 1 2 ...
  .. .. ..$ satelite: Factor w/ 2 levels "MSI","MSS": 1 1 1 1 1 1 1 1 1 1 ...
  ..@ datanames: chr [1:3] "denmark" "australia" "japan"
}

\source{
http://www.ncbi.nlm.nih.gov/geo/
}

\examples{
data(ColonData)
plot(ColonData)
}
\keyword{datasets}
