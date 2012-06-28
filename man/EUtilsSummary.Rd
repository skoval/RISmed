\name{EUtilsSummary}
\alias{EUtilsSummary}

\title{Get summary of NCBI EUtils query}

\description{
Get summary information on the results of a query for any database of the National Center for Biotechnology Information (NCBI).
}

\usage{
EUtilsSummary(query,type="esearch",db="pubmed",url=NULL,encoding="unknown",...)
}

\arguments{

\item{query}{string query as given in NCBI search box}
\item{type}{string indicating type of NCBI Eutility}
\item{db}{string indicating the NCBI database used in query}
\item{url}{string of url to bypass query construction}
\item{encoding}{encoding to be assumed for input strings, see \code{\link{readLines}}}
\item{...}{additional limits added to query}
}



\details{

Possible E-utilities to supply to \code{type} are \code{einfo}, \code{esearch}, \code{epost}, \code{esummary}, \code{elink}, \code{egquery}, \code{espell}.

The database name supplied to \code{db} can be any valid Entrez database, such as \code{pubmed}, \code{protein}, \code{nlm}, etc.

For further details on available E-utilities and databases visit \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/}.

Limits that can be supplied to \code{...} to refine the \code{query} include:

\tabular{ll}{
\code{reldate} \tab Limits search results to be within the specified number of days from current date. \cr
\code{mindate} \tab Minimum of date range for search results (examples: 2002; 2002/01/01); must be supplied with \code{maxdate}.\cr
\code{maxdate} \tab Maximum of date range for search results; must be supplied with \code{mindate}.\cr
\code{datetype} \tab Which date field to use in setting date limits. Possible choices are \code{edat}, Entrez date, which is the date article was added to Entez, or \code{ppdt}, the article publication date. Default is \code{edat}.\cr
\code{retstart} \tab Where in the sequence of returned results to begin retrieving, default is 0.\cr
\code{retmax} \tab Maximum number of records to retrieve, default is 1000.
}

}

\value{
Returns an \code{EUtilsSummary} object.
}

\seealso{\code{\link{EUtilsQuery}}}

\references{
Summary help:

\url{http://www.ncbi.nlm.nih.gov/books/NBK3827/}

EUtils ESearch

\url{http://eutils.ncbi.nlm.nih.gov/entrez/query/static/esearch_help.html}

}

\examples{

# PUBMED QUERY
res <- EUtilsSummary("myeloma[ti] jones[au]")

res

summary(res)
QueryCount(res)
QueryId(res)
QueryTranslation(res)

# PROTEIN QUERY
res <- EUtilsSummary("C-reactive[ti]",db="protein")
summary(res)

}

\author{Stephanie Kovalchik \email{s.a.kovalchik@gmail.com}}
