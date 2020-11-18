\name{DOI}
\alias{DOI}

\title{
Extracts \code{DOI} from  \code{Medline} object.
}

\description{
Extractor for the \code{DOI} of a \code{Medline} object. 
}
\usage{
DOI(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{vector}

\seealso{\code{\link{Medline}}}

\references{\url{https://www.nlm.nih.gov/bsd/mms/medlineelements.html#phst}}

\examples{
data(myeloma)
DOI(myeloma)
}
