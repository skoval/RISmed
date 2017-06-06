\name{Issue}
\alias{Issue}

\title{
Extracts \code{Issue} from  \code{Medline} object.
}

\description{
Extractor for the \code{Issue} of a \code{Medline} object.
}
\usage{
Issue(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
Issue(myeloma)
}
