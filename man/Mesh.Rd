\name{Mesh}
\alias{Mesh}

\title{
Extracts \code{Mesh} headings from \code{Medline} object.
}

\description{
Extractor for the \code{Mesh} headings of a \code{Medline} object.
}
\usage{
Mesh(object)
}

\arguments{
  \item{object}{instance of class \code{Medline}}
}

\value{List by Pubmed article. Each list contains a data frame with \code{Heading} and \code{Type}. The \code{Heading} is a MeSH Term and \code{Type} is either a \code{Descriptor} or a \code{Qualifier} of a Descriptor term. Qualifiers of a Descriptor immediately follow the Descriptor term in the data frame. When MeSH headings have not been included with a MEDLINE record, the list will contain \code{NAs} (see details).
}

\seealso{\code{\link{Medline}}}

\details{In Process and publisher-supplied records lack MeSH terms. See the MeSH home page 
\url{https://www.nlm.nih.gov/mesh/meshhome.html} for additional information about MeSH.
Note that more recent records may lack MeSH headings.
}
