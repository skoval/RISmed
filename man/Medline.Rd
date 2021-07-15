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
\item{\code{YearPubDate}:}{}                
\item{\code{MonthPubDate}:}{}               
\item{\code{DayPubDate}:}{}  
\item{\code{YearRevised}:}{}                
\item{\code{MonthRevised}:}{}               
\item{\code{DayRevised}:}{}  
\item{\code{YearEntrez}:}{}                
\item{\code{MonthEntrez}:}{}               
\item{\code{DayEntrez}:}{}   
\item{\code{HourEntrez}:}{}                
\item{\code{MinuteEntrez}:}{} 
\item{\code{YearArticleDate}:}{}                
\item{\code{MonthArticleDate}:}{}               
\item{\code{DayArticleDate}:}{}   
\item{\code{YearMedline}:}{}                
\item{\code{MonthMedline}:}{}               
\item{\code{DayMedline}:}{}   
\item{\code{HourMedline}:}{}                
\item{\code{MinuteMedline}:}{}
\item{\code{YearAccepted}:}{}                
\item{\code{MonthAccepted}:}{}               
\item{\code{DayAccepted}:}{}   
\item{\code{HourAccepted}:}{}                
\item{\code{MinuteAccepted}:}{}      
\item{\code{YearReceived}:}{}                
\item{\code{MonthReceived}:}{}               
\item{\code{DayReceived}:}{}   
\item{\code{HourReceived}:}{}                
\item{\code{MinuteReceived}:}{}   
\item{\code{YearEpublish}:}{}                
\item{\code{MonthEpublish}:}{}               
\item{\code{DayEpublish}:}{}   
\item{\code{HourEpublish}:}{}                
\item{\code{MinuteEpublish}:}{}      
\item{\code{YearPpublish}:}{}                
\item{\code{MonthPpublish}:}{}               
\item{\code{DayPpublish}:}{}   
\item{\code{HourPpublish}:}{}                
\item{\code{MinutePpublish}:}{}    
\item{\code{YearPmc}:}{}                
\item{\code{MonthPmc}:}{}               
\item{\code{DayPmc}:}{}   
\item{\code{HourPmc}:}{}                
\item{\code{MinutePmc}:}{}      
\item{\code{YearPubmed}:}{}                
\item{\code{MonthPubmed}:}{}               
\item{\code{DayPubmed}:}{}   
\item{\code{HourPubmed}:}{}                
\item{\code{MinutePubmed}:}{}      
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
\item{\code{PublicationStatus}:}{}   
\item{\code{ArticleId}:}{}     
\item{\code{DOI}:}{}  
\item{\code{Volume}:}{}              
\item{\code{Issue}:}{}               
\item{\code{ISOAbbreviation}:}{}     
\item{\code{MedlinePgn}:}{}          
\item{\code{CopyrightInformation}:}{}
\item{\code{Country}:}{}             
\item{\code{GrantID}:}{}       
\item{\code{COIStatement}:}{} 
\item{\code{Mesh}:}{ list of data frames giving \code{Heading} and \code{Type} of MeSH term or \code{NA} if no terms are in MEDLINE record}
\item{\code{Keywords}:}{}
\item{\code{Citations}:}{}
 }
}
\section{Methods}{
  In addition to \code{print} and \code{show} methods, each slot of the \code{Medline} class has a corresponding extractor of the same name.
  \describe{
\item{print}{\code{signature(x = "Medline", ...)}: ...} 
\item{show}{\code{signature(object = "Medline")}: ...} 
\item{c}{\code{signature(x = "Medline", ...)}: ...} 
\item{Query}{\code{signature(object = "Medline")}: ...}                
\item{PMID}{\code{signature(object = "Medline")}: ...}     
\item{YearPubDate}{\code{signature(object = "Medline")}: ...}               
\item{MonthPubDate}{\code{signature(object = "Medline")}: ...}              
\item{DayPubDate}{\code{signature(object = "Medline")}: ...} 
\item{YearRevised}{\code{signature(object = "Medline")}: ...}               
\item{MonthRevised}{\code{signature(object = "Medline")}: ...}              
\item{DayRevised}{\code{signature(object = "Medline")}: ...} 
\item{YearEntrez}{\code{signature(object = "Medline")}: ...}               
\item{MonthEntrez}{\code{signature(object = "Medline")}: ...}              
\item{DayEntrez}{\code{signature(object = "Medline")}: ...}  
\item{HourEntrez}{\code{signature(object = "Medline")}: ...}               
\item{MinuteEntrez}{\code{signature(object = "Medline")}: ...}
\item{YearArticleDate}{\code{signature(object = "Medline")}: ...}               
\item{MonthArticleDate}{\code{signature(object = "Medline")}: ...}              
\item{DayArticleDate}{\code{signature(object = "Medline")}: ...}  
\item{YearMedline}{\code{signature(object = "Medline")}: ...}               
\item{MonthMedline}{\code{signature(object = "Medline")}: ...}              
\item{DayMedline}{\code{signature(object = "Medline")}: ...}  
\item{HourMedline}{\code{signature(object = "Medline")}: ...}               
\item{MinuteMedline}{\code{signature(object = "Medline")}: ...} 
\item{YearAccepted}{\code{signature(object = "Medline")}: ...}                
\item{MonthAccepted}{\code{signature(object = "Medline")}: ...}               
\item{DayAccepted}{\code{signature(object = "Medline")}: ...}                 
\item{HourAccepted}{\code{signature(object = "Medline")}: ...}                
\item{MinuteAccepted}{\code{signature(object = "Medline")}: ...}   
\item{YearReceived}{\code{signature(object = "Medline")}: ...}                
\item{MonthReceived}{\code{signature(object = "Medline")}: ...}               
\item{DayReceived}{\code{signature(object = "Medline")}: ...}                 
\item{HourReceived}{\code{signature(object = "Medline")}: ...}                
\item{MinuteReceived}{\code{signature(object = "Medline")}: ...} 
\item{YearEpublish}{\code{signature(object = "Medline")}: ...}                
\item{MonthEpublish}{\code{signature(object = "Medline")}: ...}               
\item{DayEpublish}{\code{signature(object = "Medline")}: ...}                 
\item{HourEpublish}{\code{signature(object = "Medline")}: ...}                
\item{MinuteEpublish}{\code{signature(object = "Medline")}: ...} 
\item{YearPpublish}{\code{signature(object = "Medline")}: ...}                
\item{MonthPpublish}{\code{signature(object = "Medline")}: ...}               
\item{DayPpublish}{\code{signature(object = "Medline")}: ...}                 
\item{HourPpublish}{\code{signature(object = "Medline")}: ...}                
\item{MinutePpublish}{\code{signature(object = "Medline")}: ...} 
\item{YearPmc}{\code{signature(object = "Medline")}: ...}                
\item{MonthPmc}{\code{signature(object = "Medline")}: ...}               
\item{DayPmc}{\code{signature(object = "Medline")}: ...}                 
\item{HourPmc}{\code{signature(object = "Medline")}: ...}                
\item{MinutePmc}{\code{signature(object = "Medline")}: ...}  
\item{YearPubmed}{\code{signature(object = "Medline")}: ...}                
\item{MonthPubmed}{\code{signature(object = "Medline")}: ...}               
\item{DayPubmed}{\code{signature(object = "Medline")}: ...}                 
\item{HourPubmed}{\code{signature(object = "Medline")}: ...}                
\item{MinutePubmed}{\code{signature(object = "Medline")}: ...}           
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
\item{PublicationStatus}{\code{signature(object = "Medline")}: ...}   
\item{ArticleId}{\code{signature(object = "Medline")}: ...}   
\item{DOI}{\code{signature(object = "Medline")}: ...} 
\item{Volume}{\code{signature(object = "Medline")}: ...}              
\item{Issue}{\code{signature(object = "Medline")}: ...}               
\item{ISOAbbreviation}{\code{signature(object = "Medline")}: ...}     
\item{MedlinePgn}{\code{signature(object = "Medline")}: ...}          
\item{CopyrightInformation}{\code{signature(object = "Medline")}: ...}
\item{Country}{\code{signature(object = "Medline")}: ...}             
\item{GrantID}{\code{signature(object = "Medline")}: ...}             
\item{COIStatement}{\code{signature(object = "Medline")}: ...}
\item{Mesh}{\code{signature(object = "Medline")}: ...}   
\item{Keywords}{\code{signature(object = "Medline")}: ...}   
\item{Affiliations}{\code{signature(object = "Medline")}: ...}   
  }
}


\author{
Stephanie Kovalchik
}

\keyword{classes}
