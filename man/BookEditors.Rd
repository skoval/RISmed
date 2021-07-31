\name{BookEditors}
\alias{BookEditors}

\title{
Extracts \code{BookEditors} from  \code{Medline} object.
}

\description{
Extractor for the \code{BookEditors} of a \code{Medline} object.
}
\usage{
BookEditors(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
BookEditors(myeloma)
}
