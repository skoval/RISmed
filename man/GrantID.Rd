\name{GrantID}
\alias{GrantID}

\title{
Extracts \code{GrantID} from  \code{Medline} object.
}

\description{
Extractor for the \code{GrantID} of a \code{Medline} object.
}
\usage{
GrantID(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
GrantID(myeloma)
}
