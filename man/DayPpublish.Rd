\name{DayPpublish}
\alias{DayPpublish}

\title{
Extracts \code{DayPpublish} from  \code{Medline} object.
}

\description{
Extractor for the \code{DayPpublish} of a \code{Medline} object. This date corresponds to the date the article was published in print.
}
\usage{
DayPpublish(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
DayPpublish(myeloma)
}
