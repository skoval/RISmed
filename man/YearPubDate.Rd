\name{YearPubDate}
\alias{YearPubDate}

\title{
Extracts \code{YearPubDate} from  \code{Medline} object.
}

\description{
Extractor for the \code{YearPubDate} of a \code{Medline} object. This date corresponds to the date the article was PubDate.
}
\usage{
YearPubDate(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
YearPubDate(myeloma)
}
