\name{DayPubDate}
\alias{DayPubDate}

\title{
Extracts \code{DayPubDate} from  \code{Medline} object.
}

\description{
Extractor for the \code{DayPubDate} of a \code{Medline} object. This date corresponds to the date the article was PubDate.
}
\usage{
DayPubDate(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
DayPubDate(myeloma)
}
