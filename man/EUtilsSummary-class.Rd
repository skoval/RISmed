\name{EUtilsSummary-class}
\docType{class}
\alias{EUtilsSummary-class}

\title{Class \code{"EUtilsSummary"}}

\description{
Summary of query of an NCBI E-utility
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EUtilsSummary", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{db}:}{Name of database}
    \item{\code{count}:}{total ids}
    \item{\code{retmax}:}{limit on number of records to return}
    \item{\code{retstart}:}{numeric indicating where return records start}
    \item{\code{PMID}:}{character vector of matching ids}
    \item{\code{querytranslation}:}{character of NCBI translation of query}
  }
}
\section{Methods}{
  \describe{
    \item{QueryCount}{\code{signature(object = "EUtilsSummary")}: ... }
    \item{QueryId}{\code{signature(object = "EUtilsSummary")}: ... }
    \item{print}{\code{signature(x = "EUtilsSummary")}: ... }
    \item{show}{\code{signature(object = "EUtilsSummary")}: ... }
    \item{summary}{\code{signature(object = "EUtilsSummary")}: ... }
    \item{QueryTranslation}{\code{signature(object = "EUtilsSummary")}: ... }
	 }
}


\author{
Stephanie Kovalchik
}

\keyword{classes}
