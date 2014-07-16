\name{Medline-class}
\docType{class}
\alias{Medline}
\alias{Medline-class}

\title{Class \code{"Medline"}}

\description{
Class for Medline citation of query to PubMed.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Medline", ...)}.
}
\section{Slots}{
 Unless stated otherwise, each of the components is a vector of the given field where each element corresponds to a PubMed article. 
  \describe{
\item{\code{Query}:}{}
\item{\code{PMID}:}{}                
\item{\code{Year}:}{}                
\item{\code{Month}:}{}               
\item{\code{Day}:}{}                 
\item{\code{Author}:}{ list of data frames giving \code{LastName}, \code{ForeName}, \code{Initials}, and \code{order} of authors by PubMed article.}              
\item{\code{ISSN}:}{}                
\item{\code{Title}:}{}               
\item{\code{ArticleTitle}:}{}        
\item{\code{ELocationID}:}{}         
\item{\code{AbstractText}:}{}        
\item{\code{Affiliation}:}{}         
\item{\code{Language}:}{}            
\item{\code{PublicationType}:}{}     
\item{\code{MedlineTA}:}{}           
\item{\code{NlmUniqueID}:}{}         
\item{\code{ISSNLinking}:}{}         
\item{\code{Hour}:}{}                
\item{\code{Minute}:}{}              
\item{\code{PublicationStatus}:}{}   
\item{\code{ArticleId}:}{}           
\item{\code{Volume}:}{}              
\item{\code{Issue}:}{}               
\item{\code{ISOAbbreviation}:}{}     
\item{\code{MedlinePgn}:}{}          
\item{\code{CopyrightInformation}:}{}
\item{\code{Country}:}{}             
\item{\code{GrantID}:}{}             
\item{\code{Acronym}:}{}             
\item{\code{Agency}:}{}              
\item{\code{RegistryNumber}:}{}      
\item{\code{RefSource}:}{}           
\item{\code{CollectiveName}:}{}   
\item{\code{Mesh}:}{ list of data frames giving \code{Heading} and \code{Type} of MeSH term or \code{NA} if no terms are in MEDLINE record}
 }
}
\section{Methods}{
  In addition to \code{print} and \code{show} methods, each slot of the \code{Medline} class has a corresponding extractor of the same name.
  \describe{
\item{print}{\code{signature(x = "Medline", ...)}: ...} 
\item{show}{\code{signature(object = "Medline")}: ...} 
\item{Query}{\code{signature(object = "Medline")}: ...}                
\item{PMID}{\code{signature(object = "Medline")}: ...}                
\item{Year}{\code{signature(object = "Medline")}: ...}                
\item{Month}{\code{signature(object = "Medline")}: ...}               
\item{Day}{\code{signature(object = "Medline")}: ...}                 
\item{Author}{\code{signature(object = "Medline")}: ...}
\item{ISSN}{\code{signature(object = "Medline")}: ...}                
\item{Title}{\code{signature(object = "Medline")}: ...}               
\item{ArticleTitle}{\code{signature(object = "Medline")}: ...}        
\item{ELocationID}{\code{signature(object = "Medline")}: ...}         
\item{AbstractText}{\code{signature(object = "Medline")}: ...}        
\item{Affiliation}{\code{signature(object = "Medline")}: ...}         
\item{Language}{\code{signature(object = "Medline")}: ...}            
\item{PublicationType}{\code{signature(object = "Medline")}: ...}     
\item{MedlineTA}{\code{signature(object = "Medline")}: ...}           
\item{NlmUniqueID}{\code{signature(object = "Medline")}: ...}         
\item{ISSNLinking}{\code{signature(object = "Medline")}: ...}         
\item{Hour}{\code{signature(object = "Medline")}: ...}                
\item{Minute}{\code{signature(object = "Medline")}: ...}              
\item{PublicationStatus}{\code{signature(object = "Medline")}: ...}   
\item{ArticleId}{\code{signature(object = "Medline")}: ...}           
\item{Volume}{\code{signature(object = "Medline")}: ...}              
\item{Issue}{\code{signature(object = "Medline")}: ...}               
\item{ISOAbbreviation}{\code{signature(object = "Medline")}: ...}     
\item{MedlinePgn}{\code{signature(object = "Medline")}: ...}          
\item{CopyrightInformation}{\code{signature(object = "Medline")}: ...}
\item{Country}{\code{signature(object = "Medline")}: ...}             
\item{GrantID}{\code{signature(object = "Medline")}: ...}             
\item{Acronym}{\code{signature(object = "Medline")}: ...}             
\item{Agency}{\code{signature(object = "Medline")}: ...}              
\item{RegistryNumber}{\code{signature(object = "Medline")}: ...}      
\item{RefSource}{\code{signature(object = "Medline")}: ...}           
\item{CollectiveName}{\code{signature(object = "Medline")}: ...}
\item{Mesh}{\code{signature(object = "Medline")}: ...}   
  }
}


\author{
Stephanie Kovalchik
}

\keyword{classes}
