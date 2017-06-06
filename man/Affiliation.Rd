\name{Affiliation}
\alias{Affiliation}

\title{
Extracts \code{Affiliation} from  \code{Medline} object.
}

\description{
Extractor for the \code{Affiliation} of a \code{Medline} object.
}
\usage{
Affiliation(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
Affiliation(myeloma)
}
