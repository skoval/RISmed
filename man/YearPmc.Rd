\name{YearPmc}
\alias{YearPmc}

\title{
Extracts \code{YearPmc} from  \code{Medline} object.
}

\description{
Extractor for the \code{YearPmc} of a \code{Medline} object. This date corresponds to the date the article was made available through Pubmed Central.
}
\usage{
YearPmc(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
YearPmc(myeloma)
}
