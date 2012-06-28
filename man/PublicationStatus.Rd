\name{PublicationStatus}
\alias{PublicationStatus}

\title{
Extracts \code{PublicationStatus} from  \code{Medline} object.
}

\description{
Extractor for the \code{PublicationStatus} of a \code{Medline} object.
}
\usage{
PublicationStatus(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
PublicationStatus(myeloma)
}
