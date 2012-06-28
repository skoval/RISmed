\name{ISSNLinking}
\alias{ISSNLinking}

\title{
Extracts \code{ISSNLinking} from  \code{Medline} object.
}

\description{
Extractor for the \code{ISSNLinking} of a \code{Medline} object.
}
\usage{
ISSNLinking(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
ISSNLinking(myeloma)
}
