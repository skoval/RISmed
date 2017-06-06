\name{HourPpublish}
\alias{HourPpublish}

\title{
Extracts \code{HourPpublish} from  \code{Medline} object.
}

\description{
Extractor for the \code{HourPpublish} of a \code{Medline} object. This date corresponds to the date the article was published in print.
}
\usage{
HourPpublish(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
HourPpublish(myeloma)
}
