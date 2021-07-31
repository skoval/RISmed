\name{BookPublisherLocation}
\alias{BookPublisherLocation}

\title{
Extracts \code{BookPublisherLocation} from  \code{Medline} object.
}

\description{
Extractor for the \code{BookPublisherLocation} of a \code{Medline} object.
}
\usage{
BookPublisherLocation(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
BookPublisherLocation(myeloma)
}
