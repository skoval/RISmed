\name{BookPublisher}
\alias{BookPublisher}

\title{
Extracts \code{BookPublisher} from  \code{Medline} object.
}

\description{
Extractor for the \code{BookPublisher} of a \code{Medline} object.
}
\usage{
BookPublisher(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
BookPublisher(myeloma)
}
