\name{ISSN}
\alias{ISSN}

\title{
Extracts \code{ISSN} from  \code{Medline} object.
}

\description{
Extractor for the \code{ISSN} of a \code{Medline} object.
}
\usage{
ISSN(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
ISSN(myeloma)
}
