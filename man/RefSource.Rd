\name{RefSource}
\alias{RefSource}

\title{
Extracts \code{RefSource} from  \code{Medline} object.
}

\description{
Extractor for the \code{RefSource} of a \code{Medline} object.
}
\usage{
RefSource(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
RefSource(myeloma)
}
