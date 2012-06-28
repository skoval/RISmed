\name{ELocationID}
\alias{ELocationID}

\title{
Extracts \code{ELocationID} from  \code{Medline} object.
}

\description{
Extractor for the \code{ELocationID} of a \code{Medline} object.
}
\usage{
ELocationID(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
ELocationID(myeloma)
}
