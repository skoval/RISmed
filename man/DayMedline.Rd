\name{DayMedline}
\alias{DayMedline}

\title{
Extracts \code{DayMedline} from  \code{Medline} object.
}

\description{
Extractor for the \code{DayMedline} of a \code{Medline} object. This date corresponds to the date the article was ArticleDate.
}
\usage{
DayMedline(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
DayMedline(myeloma)
}
