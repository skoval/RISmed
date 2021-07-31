\name{BookMedium}
\alias{BookMedium}

\title{
Extracts \code{BookMedium} from  \code{Medline} object.
}

\description{
Extractor for the \code{BookMedium} of a \code{Medline} object.
}
\usage{
BookMedium(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
BookMedium(myeloma)
}
