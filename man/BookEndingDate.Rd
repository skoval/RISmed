\name{BookEndingDate}
\alias{BookEndingDate}

\title{
Extracts \code{BookEndingDate} from  \code{Medline} object.
}

\description{
Extractor for the \code{BookEndingDate} of a \code{Medline} object.
}
\usage{
BookEndingDate(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
BookEndingDate(myeloma)
}
