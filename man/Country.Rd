\name{Country}
\alias{Country}

\title{
Extracts \code{Country} from  \code{Medline} object.
}

\description{
Extractor for the \code{Country} of a \code{Medline} object.
}
\usage{
Country(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
Country(myeloma)
}
