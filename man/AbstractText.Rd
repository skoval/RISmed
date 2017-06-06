\name{AbstractText}
\alias{AbstractText}

\title{
Extracts \code{AbstractText} from  \code{Medline} object.
}

\description{
Extractor for the \code{AbstractText} of a \code{Medline} object.
}
\usage{
AbstractText(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
AbstractText(myeloma)
}
