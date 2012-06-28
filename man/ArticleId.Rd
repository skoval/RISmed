\name{ArticleId}
\alias{ArticleId}

\title{
Extracts \code{ArticleId} from  \code{Medline} object.
}

\description{
Extractor for the \code{ArticleId} of a \code{Medline} object.
}
\usage{
ArticleId(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
ArticleId(myeloma)
}
