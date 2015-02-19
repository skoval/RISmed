\name{HourEpublish}
\alias{HourEpublish}

\title{
Extracts \code{HourEpublish} from  \code{Medline} object.
}

\description{
Extractor for the \code{HourEpublish} of a \code{Medline} object. This date corresponds to the date the article was published online.
}
\usage{
HourEpublish(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
HourEpublish(myeloma)
}
