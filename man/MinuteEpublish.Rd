\name{MinuteEpublish}
\alias{MinuteEpublish}

\title{
Extracts \code{MinuteEpublish} from  \code{Medline} object.
}

\description{
Extractor for the \code{MinuteEpublish} of a \code{Medline} object. This date corresponds to the date the article was published online.
}
\usage{
MinuteEpublish(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MinuteEpublish(myeloma)
}
