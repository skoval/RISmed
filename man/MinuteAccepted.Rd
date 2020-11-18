\name{MinuteAccepted}
\alias{MinuteAccepted}

\title{
Extracts \code{MinuteAccepted} from  \code{Medline} object.
}

\description{
Extractor for the \code{MinuteAccepted} of a \code{Medline} object. This date corresponds to the date the article was accepted.
}
\usage{
MinuteAccepted(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MinuteAccepted(myeloma)
}
