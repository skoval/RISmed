\name{CollectiveName}
\alias{CollectiveName}

\title{
Extracts \code{CollectiveName} from  \code{Medline} object.
}

\description{
Extractor for the \code{CollectiveName} of a \code{Medline} object.
}
\usage{
CollectiveName(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
CollectiveName(myeloma)
}
