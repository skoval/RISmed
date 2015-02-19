\name{MonthReceived}
\alias{MonthReceived}

\title{
Extracts \code{MonthReceived} from  \code{Medline} object.
}

\description{
Extractor for the \code{MonthReceived} of a \code{Medline} object. This date corresponds to the date the article was received.
}
\usage{
MonthReceived(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MonthReceived(myeloma)
}
