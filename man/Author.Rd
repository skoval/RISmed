\name{Author}
\alias{Author}

\title{
Extracts \code{Author} from  \code{Medline} object.
}

\description{
Extractor for the \code{Author} of a \code{Medline} object.
}
\usage{
Author(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{List by Pubmed article. Each list contains a data frame with \code{LastName}, \code{ForeName}, \code{Initials}, and \code{order} of article authors.}

\seealso{\code{\link{Medline}}}

\examples{
data(myeloma)
Author(myeloma)
}
