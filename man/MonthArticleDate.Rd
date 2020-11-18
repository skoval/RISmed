\name{MonthArticleDate}
\alias{MonthArticleDate}

\title{
Extracts \code{MonthArticleDate} from  \code{Medline} object.
}

\description{
Extractor for the \code{MonthArticleDate} of a \code{Medline} object. This date corresponds to the date the article was ArticleDate.
}
\usage{
MonthArticleDate(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MonthArticleDate(myeloma)
}
