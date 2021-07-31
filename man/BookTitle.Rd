\name{BookTitle}
\alias{BookTitle}

\title{
Extracts \code{BookTitle} from  \code{Medline} object.
}

\description{
Extractor for the \code{BookTitle} of a \code{Medline} object.
}
\usage{
BookTitle(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
BookTitle(myeloma)
}
