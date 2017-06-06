\name{EUtilsGet}
\alias{EUtilsGet}

\title{Results of an NCBI EUtils query}

\description{
Download results of a query for any database of the National Center for Biotechnology Information (NCBI).
}

\usage{
EUtilsGet(x,type="efetch",db="pubmed")
}

\arguments{

\item{x}{Either vector of NCBI records ids, i.e. returned ids from \code{EUtilsSummary}, or the \code{EUtilsSummary} object itself}
\item{type}{string indicating type of NCBI Eutility, \code{efetch}, \code{esummary}, etc.}
\item{db}{string indicating the NCBI database used in query}
}



\details{

Possible E-utilities to supply to \code{type} are \code{efetch}, \code{esummary}, and \code{elink}.

The database name supplied to \code{db} can be any valid Entrez database, such as \code{pubmed}, \code{protein}, \code{nlm}, etc.

For further details on available E-utilities and databases visit \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/}.

Note on usage:

In order not to overload the E-utility servers, NCBI recommends that users post no more than three URL requests per second and limit large jobs to either weekends or between 9:00 PM and 5:00 AM Eastern time during weekdays. Failure to comply with this policy may result in an IP address being blocked from accessing NCBI. 

}

\value{
For queries to PubMed, a list by article, with named vectors for each field of the article result.

For all other database, a vector with each element name the field returned from \code{XML} output.
}

\seealso{\code{\link{EUtilsSummary}}}

\references{
Summary help:

\url{http://www.ncbi.nlm.nih.gov/books/NBK3827/}

MEDLINE/PubMed Field Descriptions

\url{http://www.nlm.nih.gov/bsd/mms/medlineelements.html}

}

\examples{

# PUBMED QUERY FOR MYELOMA ARTICLES FOR LAST YEAR
res <- EUtilsSummary("myeloma[ti]",retmax=2,reldate=365)

summary(res)

fetch <- EUtilsGet(res)

# PUBMED RETURNS A MEDLINE OBJECT
fetch

PMID(fetch)

Author(fetch)[[1]]

}

\author{Stephanie Kovalchik \email{s.a.kovalchik@gmail.com}}
