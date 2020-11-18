\name{YearEntrez}
\alias{YearEntrez}

\title{
Extracts \code{YearEntrez} from  \code{Medline} object.
}

\description{
Extractor for the \code{YearEntrez} of a \code{Medline} object. This date corresponds to the date the article was ArticleDate.
}
\usage{
YearEntrez(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
YearEntrez(myeloma)
}
