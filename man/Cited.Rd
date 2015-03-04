\name{Cited}
\alias{Cited}

\title{
Number of citations
}

\description{
Retrieves the number of citations in PubMed Central for the articles contained in the \code{EUtilsSummary} or \code{Medline} object.
}

\usage{
Cited(object)
}

\arguments{
  \item{object}{instance of class \code{Medline} or \code{EUtilsSummary}}
}

\value{vector}

\seealso{\code{\link{Medline}}, \code{\link{EUtilsSummary}}}

\examples{
res <- EUtilsSummary("myeloma[ti]", mindate = "2002/01/01", maxdate = "2002/02/01", retmax = 5)
Cited(res)
}
