\name{HourReceived}
\alias{HourReceived}

\title{
Extracts \code{HourReceived} from  \code{Medline} object.
}

\description{
Extractor for the \code{HourReceived} of a \code{Medline} object. This date corresponds to the date the article was received.
}
\usage{
HourReceived(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
HourReceived(myeloma)
}
