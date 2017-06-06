\name{ArticleTitle}
\alias{ArticleTitle}

\title{
Extracts \code{ArticleTitle} from  \code{Medline} object.
}

\description{
Extractor for the \code{ArticleTitle} of a \code{Medline} object.
}
\usage{
ArticleTitle(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
ArticleTitle(myeloma)
}
