\name{MonthEpublish}
\alias{MonthEpublish}

\title{
Extracts \code{MonthEpublish} from  \code{Medline} object.
}

\description{
Extractor for the \code{MonthEpublish} of a \code{Medline} object. This date corresponds to the date the article was published online.
}
\usage{
MonthEpublish(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MonthEpublish(myeloma)
}
