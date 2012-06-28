\name{DescriptorName}
\alias{DescriptorName}

\title{
Extracts \code{DescriptorName} from  \code{Medline} object.
}

\description{
Extractor for the \code{DescriptorName} of a \code{Medline} object.
}
\usage{
DescriptorName(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
DescriptorName(myeloma)
}
