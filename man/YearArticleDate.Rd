\name{YearArticleDate}
\alias{YearArticleDate}

\title{
Extracts \code{YearArticleDate} from  \code{Medline} object.
}

\description{
Extractor for the \code{YearArticleDate} of a \code{Medline} object. This date corresponds to the date the article was ArticleDate.
}
\usage{
YearArticleDate(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
YearArticleDate(myeloma)
}
