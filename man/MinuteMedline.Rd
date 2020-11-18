\name{MinuteMedline}
\alias{MinuteMedline}

\title{
Extracts \code{MinuteMedline} from  \code{Medline} object.
}

\description{
Extractor for the \code{MinuteMedline} of a \code{Medline} object. This date corresponds to the date the article was ArticleDate.
}
\usage{
MinuteMedline(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MinuteMedline(myeloma)
}
