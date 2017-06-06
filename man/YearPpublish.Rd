\name{YearPpublish}
\alias{YearPpublish}

\title{
Extracts \code{YearPpublish} from  \code{Medline} object.
}

\description{
Extractor for the \code{YearPpublish} of a \code{Medline} object. This date corresponds to the date the article was published in print.
}
\usage{
YearPpublish(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
YearPpublish(myeloma)
}
