\name{MonthRevised}
\alias{MonthRevised}

\title{
Extracts \code{MonthRevised} from  \code{Medline} object.
}

\description{
Extractor for the \code{MonthRevised} of a \code{Medline} object. This date corresponds to the date the article was Revised.
}
\usage{
MonthRevised(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
MonthRevised(myeloma)
}
