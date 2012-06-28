\name{RegistryNumber}
\alias{RegistryNumber}

\title{
Extracts \code{RegistryNumber} from  \code{Medline} object.
}

\description{
Extractor for the \code{RegistryNumber} of a \code{Medline} object.
}
\usage{
RegistryNumber(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
RegistryNumber(myeloma)
}
