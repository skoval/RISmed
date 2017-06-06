\name{MonthPmc}
\alias{MonthPmc}

\title{
Extracts \code{MonthPmc} from  \code{Medline} object.
}

\description{
Extractor for the \code{MonthPmc} of a \code{Medline} object. This date corresponds to the date the article was made available through Pubmed Central.
}
\usage{
MonthPmc(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MonthPmc(myeloma)
}
