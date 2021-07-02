\name{GrantID}
\alias{GrantID}

\title{
Extracts list of \code{GrantID} info  \code{Medline} object including \code{GrantID} and \code{GrantAgency} where listed in PubMed.
}

\description{
Extractor for the \code{GrantID} of a \code{Medline} object.
}
\usage{
GrantID(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
GrantID(myeloma)
}
