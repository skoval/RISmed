\name{PublicationType}
\alias{PublicationType}

\title{
Extracts \code{PublicationType} from  \code{Medline} object.
}

\description{
Extractor for the \code{PublicationType} of a \code{Medline} object.
}
\usage{
PublicationType(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{list}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
PublicationType(myeloma)
}
