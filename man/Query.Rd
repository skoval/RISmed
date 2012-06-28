\name{Query}
\alias{Query}

\title{
Extract \code{Query} from  \code{Medline} object.
}

\description{
Extractor for the translated \code{Query} of a \code{Medline} object.
}
\usage{
Query(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}


\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
Query(myeloma)
}
