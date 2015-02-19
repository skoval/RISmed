\name{MinutePmc}
\alias{MinutePmc}

\title{
Extracts \code{MinutePmc} from  \code{Medline} object.
}

\description{
Extractor for the \code{MinutePmc} of a \code{Medline} object. This date corresponds to the date the article was made available through Pubmed Central.
}
\usage{
MinutePmc(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MinutePmc(myeloma)
}
