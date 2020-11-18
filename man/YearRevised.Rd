\name{YearRevised}
\alias{YearRevised}

\title{
Extracts \code{YearRevised} from  \code{Medline} object.
}

\description{
Extractor for the \code{YearRevised} of a \code{Medline} object. This date corresponds to the date the article was Revised.
}
\usage{
YearRevised(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
YearRevised(myeloma)
}
