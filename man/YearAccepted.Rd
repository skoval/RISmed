\name{YearAccepted}
\alias{YearAccepted}

\title{
Extracts \code{YearAccepted} from  \code{Medline} object.
}

\description{
Extractor for the \code{YearAccepted} of a \code{Medline} object. This date corresponds to the date the article was accepted.
}
\usage{
YearAccepted(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
YearAccepted(myeloma)
}
