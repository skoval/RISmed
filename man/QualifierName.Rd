\name{QualifierName}
\alias{QualifierName}

\title{
Extracts \code{QualifierName} from  \code{Medline} object.
}

\description{
Extractor for the \code{QualifierName} of a \code{Medline} object.
}
\usage{
QualifierName(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
QualifierName(myeloma)
}
