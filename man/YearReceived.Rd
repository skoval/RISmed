\name{YearReceived}
\alias{YearReceived}

\title{
Extracts \code{YearReceived} from  \code{Medline} object.
}

\description{
Extractor for the \code{YearReceived} of a \code{Medline} object. This date corresponds to the date the article was received.
}
\usage{
YearReceived(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
YearReceived(myeloma)
}
