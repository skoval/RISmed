\name{CopyrightInformation}
\alias{CopyrightInformation}

\title{
Extracts \code{CopyrightInformation} from  \code{Medline} object.
}

\description{
Extractor for the \code{CopyrightInformation} of a \code{Medline} object.
}
\usage{
CopyrightInformation(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
CopyrightInformation(myeloma)
}
