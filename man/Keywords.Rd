\name{Keywords}
\alias{Keywords}

\title{
Extracts list of \code{Keywords} info from \code{Medline} object.
}

\description{
Extractor for the \code{Keywords} of a \code{Medline} object.
}
\usage{
Keywords(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
Keywords(myeloma)
}
