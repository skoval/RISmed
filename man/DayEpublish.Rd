\name{DayEpublish}
\alias{DayEpublish}

\title{
Extracts \code{DayEpublish} from  \code{Medline} object.
}

\description{
Extractor for the \code{DayEpublish} of a \code{Medline} object. This date corresponds to the date the article was published online.
}
\usage{
DayEpublish(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
DayEpublish(myeloma)
}
