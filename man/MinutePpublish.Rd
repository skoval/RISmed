\name{MinutePpublish}
\alias{MinutePpublish}

\title{
Extracts \code{MinutePpublish} from  \code{Medline} object.
}

\description{
Extractor for the \code{MinutePpublish} of a \code{Medline} object. This date corresponds to the date the article was published in print.
}
\usage{
MinutePpublish(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MinutePpublish(myeloma)
}
