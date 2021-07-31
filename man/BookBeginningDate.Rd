\name{BookBeginningDate}
\alias{BookBeginningDate}

\title{
Extracts \code{BookBeginningDate} from  \code{Medline} object.
}

\description{
Extractor for the \code{BookBeginningDate} of a \code{Medline} object.
}
\usage{
BookBeginningDate(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
BookBeginningDate(myeloma)
}
