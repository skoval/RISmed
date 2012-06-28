\name{NlmUniqueID}
\alias{NlmUniqueID}

\title{
Extracts \code{NlmUniqueID} from  \code{Medline} object.
}

\description{
Extractor for the \code{NlmUniqueID} of a \code{Medline} object.
}
\usage{
NlmUniqueID(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
NlmUniqueID(myeloma)
}
