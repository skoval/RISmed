\name{c}
\alias{c}

\title{Collection method for Medline class}

\usage{
c(x, ...)
}

\arguments{
\item{x}{Medline object}
\item{...}{Additional Medline objects to add to \code{x}}
}

\description{
Combines multiple Medline objects resulting from a call to \link{EUtilsGet} into a single object. There is currently no check for duplicate entries.
}

\seealso{\code{\link{Medline}}, \code{\link{EUtilsGet}}}


\examples{
res <- EUtilsSummary("myeloma[ti]",retmax=2,mindate = 2011, maxdate = 2012)
fetch2012 <- EUtilsGet(res)

res <- EUtilsSummary("myeloma[ti]",retmax=2,mindate = 2013, maxdate = 2014)
fetch2014 <- EUtilsGet(res)

fetch <- c(fetch2012, fetch2014)

YearPubmed(fetch)
ArticleTitle(fetch)
}



\author{Stephanie Kovalchik \email{s.a.kovalchik@gmail.com}}

