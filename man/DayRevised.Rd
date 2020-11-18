\name{DayRevised}
\alias{DayRevised}

\title{
Extracts \code{DayRevised} from  \code{Medline} object.
}

\description{
Extractor for the \code{DayRevised} of a \code{Medline} object. This date corresponds to the date the article was Revised.
}
\usage{
DayRevised(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
DayRevised(myeloma)
}
