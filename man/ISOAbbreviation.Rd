\name{ISOAbbreviation}
\alias{ISOAbbreviation}

\title{
Extracts \code{ISOAbbreviation} from  \code{Medline} object.
}

\description{
Extractor for the \code{ISOAbbreviation} of a \code{Medline} object.
}
\usage{
ISOAbbreviation(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
ISOAbbreviation(myeloma)
}
