\name{PMID}
\alias{PMID}

\title{
Extracts \code{PMID} from  \code{Medline} object.
}

\description{
Extractor for the \code{PMID} of a \code{Medline} object.
}
\usage{
PMID(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
PMID(myeloma)
}
