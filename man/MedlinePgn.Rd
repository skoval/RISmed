\name{MedlinePgn}
\alias{MedlinePgn}

\title{
Extracts \code{MedlinePgn} from  \code{Medline} object.
}

\description{
Extractor for the \code{MedlinePgn} of a \code{Medline} object.
}
\usage{
MedlinePgn(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
MedlinePgn(myeloma)
}
