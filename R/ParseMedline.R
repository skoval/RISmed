ParseMedline <- function(x){
	if(names(x)[[1]] == "BookDocument")
		ParseMedlineBook(x)
	else
		ParseMedlineArticle(x)
}

ParseMedlineArticle <- function(x){

Tags <- c("PMID",
"YearRevised",
"MonthRevised",
"DayRevised",
"YearPubDate",
"MonthPubDate",
"DayPubDate",
"YearArticleDate",
"MonthArticleDate",
"DayArticleDate",
"YearEntrez",
"MonthEntrez",
"DayEntrez",
"HourEntrez",
"MinuteEntrez",
"YearMedline",
"MonthMedline",
"DayMedline",
"HourMedline",
"MinuteMedline",
"YearAccepted",
"MonthAccepted",
"DayAccepted",
"HourAccepted",
"MinuteAccepted",
"YearReceived",
"MonthReceived",
"DayReceived",
"HourReceived",
"MinuteReceived",
"YearEpublish",
"MonthEpublish",
"DayEpublish",
"HourEpublish",
"MinuteEpublish",
"YearPpublish",
"MonthPpublish",
"DayPpublish",
"HourPpublish",
"MinutePpublish",
"YearPmc",
"MonthPmc",
"DayPmc",
"HourPmc",
"MinutePmc",
"YearPubmed",
"MonthPubmed",
"DayPubmed",
"HourPubmed",
"MinutePubmed",
"ISSN",
"Title",
"Author",
"ArticleTitle",
"ELocationID",
"AbstractText",
"Affiliation",
"Language",
"PublicationType",
"MedlineTA",
"NlmUniqueID",
"ISSNLinking",
"PublicationStatus",
"ArticleId",
"DOI",
"Volume",
"Issue",
"ISOAbbreviation",
"MedlinePgn",
"CopyrightInformation",
"Country",
"GrantID",
"COIStatement",
"Mesh",
"Keywords",
"Citations",
"BookPublisher",
"BookPublisherLocation",
"BookTitle",
"BookBeginningDate",
"BookEndingDate",
"BookEditors",
"BookMedium"
)

Fields <- vector(mode = "list", length = length(Tags))
names(Fields) <- Tags


Fields[["PMID"]] <- x$MedlineCitation$PMID[[1]]

Fields[["YearRevised"]] <- x$MedlineCitation$DateRevised$Year[[1]]
Fields[["MonthRevised"]] <- x$MedlineCitation$DateRevised$Month[[1]]
Fields[["DayRevised"]] <- x$MedlineCitation$DateRevised$Day[[1]]
Fields[["YearPubDate"]] <- x$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year[[1]]
Fields[["MonthPubDate"]] <- x$MedlineCitation$Article$Journal$JournalIssue$PubDate$Month[[1]]
Fields[["DayPubDate"]] <- x$MedlineCitation$Article$Journal$JournalIssue$PubDate$Day[[1]]
Fields[["YearArticleDate"]] <- x$MedlineCitation$Article$ArticleDate$Year[[1]]
Fields[["MonthArticleDate"]] <- x$MedlineCitation$Article$ArticleDate$Month[[1]]
Fields[["DayArticleDate"]] <- x$MedlineCitation$Article$ArticleDate$Day[[1]]

pubmed_states <- sapply(x$PubmedData$History, function(z) attr(z, "PubStatus"))


if(length(pubmed_states) > 0){
	if(any(pubmed_states == "entrez")){
		i <- which(pubmed_states == "entrez")[1]
		Fields[["YearEntrez"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthEntrez"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayEntrez"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteEntrez"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourEntrez"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}
	
	if(any(pubmed_states == "medline")){
		i <- which(pubmed_states == "medline")[1]
		Fields[["YearMedline"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthMedline"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayMedline"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteMedline"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourMedline"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "accepted")){
		i <- which(pubmed_states == "accepted")[1]		
		Fields[["YearAccepted"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthAccepted"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayAccepted"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteAccepted"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourAccepted"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "received")){
		i <- which(pubmed_states == "received")[1]
		Fields[["YearReceived"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthReceived"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayReceived"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteReceived"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourReceived"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "epublish")){
		i <- which(pubmed_states == "epublish")[1]
		Fields[["YearEpublish"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthEpublish"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayEpublish"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteEpublish"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourEpublish"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}			

	if(any(pubmed_states == "ppublish")){
		i <- which(pubmed_states == "ppublish")[1]
		Fields[["YearPpublish"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthPpublish"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayPpublish"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinutePpublish"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourPpublish"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "pmc")){
		i <- which(pubmed_states == "pmc")
		Fields[["YearPmc"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthPmc"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayPmc"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinutePmc"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourPmc"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}		

	if(any(pubmed_states == "pubmed")){
		i <- which(pubmed_states == "pubmed")[1]
		Fields[["YearPubmed"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthPubmed"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayPubmed"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinutePubmed"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourPubmed"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}
}

Fields[["ISSN"]] <-  x$MedlineCitation$Article$Journal$ISSN[[1]]
Fields[["Volume"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$Volume[[1]]
Fields[["Issue"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$Issue[[1]]
Fields[["Title"]] <-  x$MedlineCitation$Article$Journal$Title[[1]]

if(!is.null(x$MedlineCitation$Article$ArticleTitle))
	Fields[["ArticleTitle"]] <-  paste(unlist(x$MedlineCitation$Article[names(x$MedlineCitation$Article) == "ArticleTitle"]), collapse = "")

if(!is.null(x$MedlineCitation$Article$AuthorList)){
	
	Authors <- do.call("rbind", lapply(x$MedlineCitation$Article$AuthorList, function(z){

		if(any(names(z) == "CollectiveName") | (is.null(z$ForeName) & is.null(z$Initials))){
			data.frame(
			CollectiveName = ifelse(is.null(z$CollectiveName), as.character(z$LastName[[1]]), as.character(z$CollectiveName[[1]])),
			LastName = NA,
			ForeName = NA,
			Initials = NA,
			stringsAsFactors=FALSE
			)
		}
		else{
			data.frame(
			CollectiveName = NA,
			LastName = ifelse(is.null(z$LastName[[1]]), NA, as.character(z$LastName[[1]])),
			ForeName = ifelse(is.null(z$ForeName[[1]]), NA, as.character(z$ForeName[[1]])),
			Initials = ifelse(is.null(z$Initials[[1]]), NA, as.character(z$Initials[[1]])),
			stringsAsFactors=FALSE
			)			
		}
	}))
	
	Authors$order <- 1:nrow(Authors)
	
	Fields[["Author"]] <- Authors
	
	Affiliations <- lapply(x$MedlineCitation$Article$AuthorList, function(z){
		z$AffiliationInfo$Affiliation[[1]]
	})
	
	names(Affiliations) <- 1:length(Affiliations)
	
	Fields[["Affiliation"]] <- unlist(Affiliations)
}

elocations <- names(x$MedlineCitation$Article)

if(any(elocations == "ELocationID")){
	
	elocations <- x$MedlineCitation$Article[which(elocations == "ELocationID")]
	
	for(e in elocations){
		if(attr(e, "EIdType") == "pii")
			Fields[["ELocationID"]] <- e[[1]]
			
		if(attr(e, "EIdType") == "doi")
			Fields[["DOI"]] <- e[[1]]			
	}	
}

if(!is.null(x$MedlineCitation$Article$Abstract)){
	AbstractText <- paste(unlist(x$MedlineCitation$Article$Abstract[names(x$MedlineCitation$Article$Abstract) == "AbstractText"]), collapse = " ")
	
	Fields[["AbstractText"]] <- AbstractText
}

Fields[["Language"]] <- x$MedlineCitation$Article$Language[[1]]
if(!is.null(x$MedlineCitation$Article$PublicationTypeList))
	Fields[["PublicationType"]] <- unlist(x$MedlineCitation$Article$PublicationTypeList[names(x$MedlineCitation$Article$PublicationTypeList) == "PublicationType"])
Fields[["Country"]] <- x$MedlineCitation$MedlineJournalInfo$Country[[1]]
Fields[["MedlineTA"]] <- x$MedlineCitation$MedlineJournalInfo$MedlineTA[[1]]
Fields[["NlmUniqueID"]] <- x$MedlineCitation$MedlineJournalInfo$NlmUniqueID[[1]]
Fields[["ISSNLinking"]] <- x$MedlineCitation$MedlineJournalInfo$ISSNLinking[[1]]
Fields[["PublicationStatus"]] <- x$PubmedData$PublicationStatus[[1]]
Fields[["ArticleId"]] <- x$PubmedData$ArticleIdList$ArticleId[[1]]
Fields[["ISOAbbreviation"]] <- x$MedlineCitation$Article$Journal$ISOAbbreviation[[1]]
Fields[["MedlinePgn"]] <- x$MedlineCitation$Article$Pagination$MedlinePgn[[1]]
Fields[["CopyrightInformation"]] <- x$MedlineCitation$Article$Abstract$CopyrightInformation[[1]]


if(!is.null(x$MedlineCitation$Article$GrantList)){
	
	Grants <- do.call("rbind", lapply(x$MedlineCitation$Article$GrantList, function(z){
		data.frame(
		GrantID = ifelse(is.null(z$GrantID[[1]]), NA, z$GrantID[[1]]),
		Agency =  ifelse(is.null(z$Agency[[1]]), NA, z$Agency[[1]]),
		stringsAsFactors = F
		)	
	}))
	
	Fields[["GrantID"]] <- Grants
}

Fields[["COIStatement"]] <- x$MedlineCitation$CoiStatement[[1]]

if(!is.null(x$MedlineCitation$KeywordList)){
	keywords <- x$MedlineCitation$KeywordList[names(x$MedlineCitation$KeywordList) == "Keyword"]
	Fields[["Keywords"]] <- unlist(keywords)
}


if(!is.null(x$MedlineCitation$MeshHeadingList$MeshHeading)){
	Mesh <- data.frame(
		  	   Heading = unlist(x$MedlineCitation$MeshHeadingList),
               Type = ifelse(grepl("DescriptorName", names(unlist(x$MedlineCitation$MeshHeadingList))),"Descriptor","Qualifier"),
               stringsAsFactors = F
               )
               
   Fields[["Mesh"]] <- Mesh
 }
 
 
if(!is.null(x$PubmedData$ReferenceList)){
	References <- lapply(x$PubmedData$ReferenceList, function(z){
		paste(unlist(z), collapse = "")
	})
               
   Fields[["Citations"]] <- References
} 

Fields
}




ParseMedlineBook <- function(x){

Tags <- c("PMID",
"YearRevised",
"MonthRevised",
"DayRevised",
"YearPubDate",
"MonthPubDate",
"DayPubDate",
"YearArticleDate",
"MonthArticleDate",
"DayArticleDate",
"YearEntrez",
"MonthEntrez",
"DayEntrez",
"HourEntrez",
"MinuteEntrez",
"YearMedline",
"MonthMedline",
"DayMedline",
"HourMedline",
"MinuteMedline",
"YearAccepted",
"MonthAccepted",
"DayAccepted",
"HourAccepted",
"MinuteAccepted",
"YearReceived",
"MonthReceived",
"DayReceived",
"HourReceived",
"MinuteReceived",
"YearEpublish",
"MonthEpublish",
"DayEpublish",
"HourEpublish",
"MinuteEpublish",
"YearPpublish",
"MonthPpublish",
"DayPpublish",
"HourPpublish",
"MinutePpublish",
"YearPmc",
"MonthPmc",
"DayPmc",
"HourPmc",
"MinutePmc",
"YearPubmed",
"MonthPubmed",
"DayPubmed",
"HourPubmed",
"MinutePubmed",
"ISSN",
"Title",
"Author",
"ArticleTitle",
"ELocationID",
"AbstractText",
"Affiliation",
"Language",
"PublicationType",
"MedlineTA",
"NlmUniqueID",
"ISSNLinking",
"PublicationStatus",
"ArticleId",
"DOI",
"Volume",
"Issue",
"ISOAbbreviation",
"MedlinePgn",
"CopyrightInformation",
"Country",
"GrantID",
"COIStatement",
"Mesh",
"Keywords",
"Citations",
"BookPublisher",
"BookPublisherLocation",
"BookTitle",
"BookBeginningDate",
"BookEndingDate",
"BookEditors",
"BookMedium"
)

Fields <- vector(mode = "list", length = length(Tags))
names(Fields) <- Tags


Fields[["PMID"]] <- x$BookDocument$PMID[[1]]

Fields[["YearRevised"]] <- x$BookDocument$DateRevised$Year[[1]]
Fields[["MonthRevised"]] <- x$BookDocument$DateRevised$Month[[1]]
Fields[["DayRevised"]] <- x$BookDocument$DateRevised$Day[[1]]
Fields[["YearPubDate"]] <- x$BookDocument$Book$ContributionDate$Year[[1]]
Fields[["MonthPubDate"]] <- x$BookDocument$Book$ContributionDate$Month[[1]]
Fields[["DayPubDate"]] <- x$BookDocument$Book$ContributionDate$Day[[1]]
Fields[["YearArticleDate"]] <- x$BookDocument$ArticleDate$Year[[1]]
Fields[["MonthArticleDate"]] <- x$BookDocument$ArticleDate$Month[[1]]
Fields[["DayArticleDate"]] <- x$BookDocument$ArticleDate$Day[[1]]

pubmed_states <- sapply(x$PubmedBookData$History, function(z) attr(z, "PubStatus"))


if(length(pubmed_states) > 0){
	if(any(pubmed_states == "entrez")){
		i <- which(pubmed_states == "entrez")[1]
		Fields[["YearEntrez"]] <- x$PubmedBookData$History[[i]]$Year[[1]]
		Fields[["MonthEntrez"]] <- x$PubmedBookData$History[[i]]$Month[[1]]
		Fields[["DayEntrez"]] <- x$PubmedBookData$History[[i]]$Day[[1]]
		Fields[["MinuteEntrez"]] <- x$PubmedBookData$History[[i]]$Minute[[1]]
		Fields[["HourEntrez"]] <- x$PubmedBookData$History[[i]]$Hour[[1]]
	}
	
	if(any(pubmed_states == "medline")){
		i <- which(pubmed_states == "medline")[1]
		Fields[["YearMedline"]] <- x$PubmedBookData$History[[i]]$Year[[1]]
		Fields[["MonthMedline"]] <- x$PubmedBookData$History[[i]]$Month[[1]]
		Fields[["DayMedline"]] <- x$PubmedBookData$History[[i]]$Day[[1]]
		Fields[["MinuteMedline"]] <- x$PubmedBookData$History[[i]]$Minute[[1]]
		Fields[["HourMedline"]] <- x$PubmedBookData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "accepted")){
		i <- which(pubmed_states == "accepted")[1]		
		Fields[["YearAccepted"]] <- x$PubmedBookData$History[[i]]$Year[[1]]
		Fields[["MonthAccepted"]] <- x$PubmedBookData$History[[i]]$Month[[1]]
		Fields[["DayAccepted"]] <- x$PubmedBookData$History[[i]]$Day[[1]]
		Fields[["MinuteAccepted"]] <- x$PubmedBookData$History[[i]]$Minute[[1]]
		Fields[["HourAccepted"]] <- x$PubmedBookData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "received")){
		i <- which(pubmed_states == "received")[1]
		Fields[["YearReceived"]] <- x$PubmedBookData$History[[i]]$Year[[1]]
		Fields[["MonthReceived"]] <- x$PubmedBookData$History[[i]]$Month[[1]]
		Fields[["DayReceived"]] <- x$PubmedBookData$History[[i]]$Day[[1]]
		Fields[["MinuteReceived"]] <- x$PubmedBookData$History[[i]]$Minute[[1]]
		Fields[["HourReceived"]] <- x$PubmedBookData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "epublish")){
		i <- which(pubmed_states == "epublish")[1]
		Fields[["YearEpublish"]] <- x$PubmedBookData$History[[i]]$Year[[1]]
		Fields[["MonthEpublish"]] <- x$PubmedBookData$History[[i]]$Month[[1]]
		Fields[["DayEpublish"]] <- x$PubmedBookData$History[[i]]$Day[[1]]
		Fields[["MinuteEpublish"]] <- x$PubmedBookData$History[[i]]$Minute[[1]]
		Fields[["HourEpublish"]] <- x$PubmedBookData$History[[i]]$Hour[[1]]
	}			

	if(any(pubmed_states == "ppublish")){
		i <- which(pubmed_states == "ppublish")[1]
		Fields[["YearPpublish"]] <- x$PubmedBookData$History[[i]]$Year[[1]]
		Fields[["MonthPpublish"]] <- x$PubmedBookData$History[[i]]$Month[[1]]
		Fields[["DayPpublish"]] <- x$PubmedBookData$History[[i]]$Day[[1]]
		Fields[["MinutePpublish"]] <- x$PubmedBookData$History[[i]]$Minute[[1]]
		Fields[["HourPpublish"]] <- x$PubmedBookData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "pmc")){
		i <- which(pubmed_states == "pmc")
		Fields[["YearPmc"]] <- x$PubmedBookData$History[[i]]$Year[[1]]
		Fields[["MonthPmc"]] <- x$PubmedBookData$History[[i]]$Month[[1]]
		Fields[["DayPmc"]] <- x$PubmedBookData$History[[i]]$Day[[1]]
		Fields[["MinutePmc"]] <- x$PubmedBookData$History[[i]]$Minute[[1]]
		Fields[["HourPmc"]] <- x$PubmedBookData$History[[i]]$Hour[[1]]
	}		

	if(any(pubmed_states == "pubmed")){
		i <- which(pubmed_states == "pubmed")[1]
		Fields[["YearPubmed"]] <- x$PubmedBookData$History[[i]]$Year[[1]]
		Fields[["MonthPubmed"]] <- x$PubmedBookData$History[[i]]$Month[[1]]
		Fields[["DayPubmed"]] <- x$PubmedBookData$History[[i]]$Day[[1]]
		Fields[["MinutePubmed"]] <- x$PubmedBookData$History[[i]]$Minute[[1]]
		Fields[["HourPubmed"]] <- x$PubmedBookData$History[[i]]$Hour[[1]]
	}
}

Fields[["ISSN"]] <-  x$BookDocument$Article$Journal$ISSN[[1]]
Fields[["Volume"]] <-  x$BookDocument$Article$Journal$JournalIssue$Volume[[1]]
Fields[["Issue"]] <-  x$BookDocument$Article$Journal$JournalIssue$Issue[[1]]
Fields[["Title"]] <-  x$BookDocument$Article$Journal$JournalIssue$Title[[1]]

if(!is.null(x$BookDocument$ArticleTitle))
	Fields[["ArticleTitle"]] <-  paste(unlist(x$BookDocument[names(x$BookDocument) == "ArticleTitle"]), collapse = "")

authors_editors <- x$BookDocument$Book[names(x$BookDocument$Book) == "AuthorList"]
authors <- authors_editors[sapply(authors_editors, function(x) attr(x, "Type")) == "authors"]
editors <- authors_editors[sapply(authors_editors, function(x) attr(x, "Type")) == "editors"]


if(length(authors) != 0){
	
	Authors <- do.call("rbind", lapply(authors[[1]], function(z){

		if(any(names(z) == "CollectiveName") | (is.null(z$ForeName) & is.null(z$Initials))){
			data.frame(
			CollectiveName = ifelse(is.null(z$CollectiveName), as.character(z$LastName[[1]]), as.character(z$CollectiveName[[1]])),
			LastName = NA,
			ForeName = NA,
			Initials = NA,
			stringsAsFactors=FALSE
			)
		}
		else{
			data.frame(
			CollectiveName = NA,
			LastName = ifelse(is.null(z$LastName[[1]]), NA, as.character(z$LastName[[1]])),
			ForeName = ifelse(is.null(z$ForeName[[1]]), NA, as.character(z$ForeName[[1]])),
			Initials = ifelse(is.null(z$Initials[[1]]), NA, as.character(z$Initials[[1]])),
			stringsAsFactors=FALSE
			)			
		}
	}))
	
	Authors$order <- 1:nrow(Authors)
	
	Fields[["Author"]] <- Authors
	
	Affiliations <- lapply(authors, function(z){
		z$AffiliationInfo$Affiliation[[1]]
	})
	
	names(Affiliations) <- 1:length(Affiliations)
	
	Fields[["Affiliation"]] <- unlist(Affiliations)
}


if(length(editors) != 0){
	
	Editors <- do.call("rbind", lapply(editors[[1]], function(z){

		data.frame(
			LastName = ifelse(is.null(z$LastName[[1]]), NA, as.character(z$LastName[[1]])),
			ForeName = ifelse(is.null(z$ForeName[[1]]), NA, as.character(z$ForeName[[1]])),
			Initials = ifelse(is.null(z$Initials[[1]]), NA, as.character(z$Initials[[1]])),
			stringsAsFactors=FALSE
			)			
	}))
	
	Editors$order <- 1:nrow(Editors)
	
	Fields[["BookEditors"]] <- Editors
}

elocations <- names(x$BookDocument$Article)

if(any(elocations == "ELocationID")){
	
	elocations <- x$BookDocument$Article[which(elocations == "ELocationID")]
	
	for(e in elocations){
		if(attr(e, "EIdType") == "pii")
			Fields[["ELocationID"]] <- e[[1]]
			
		if(attr(e, "EIdType") == "doi")
			Fields[["DOI"]] <- e[[1]]			
	}	
}

if(!is.null(x$BookDocument$Abstract)){
	AbstractText <- paste(unlist(x$BookDocument$Abstract[names(x$BookDocument$Abstract) == "AbstractText"]), collapse = " ")
	
	Fields[["AbstractText"]] <- AbstractText
}

Fields[["Language"]] <- x$BookDocument$Language[[1]]
Fields[["PublicationType"]] <- x$BookDocument$Book$PublicationType[[1]]
Fields[["Country"]] <- x$BookDocument$Article$Country[[1]]
Fields[["MedlineTA"]] <- x$BookDocument$Article$MedlineTA[[1]]
Fields[["NlmUniqueID"]] <- x$BookDocument$Article$NlmUniqueID[[1]]
Fields[["ISSNLinking"]] <- x$BookDocument$Article$ISSNLinking[[1]]
Fields[["PublicationStatus"]] <- x$PubmedBookData$PublicationStatus[[1]]
Fields[["ArticleId"]] <- x$BookDocument$ArticleIdList$ArticleId[[1]]
Fields[["ISOAbbreviation"]] <- x$BookDocument$Article$Journal$ISOAbbreviation[[1]]
Fields[["MedlinePgn"]] <- x$BookDocument$Article$Pagination$MedlinePgn[[1]]
Fields[["CopyrightInformation"]] <- x$BookDocument$Abstract$CopyrightInformation[[1]]
Fields[["BookPublisher"]] <- x$BookDocument$Book$Publisher$PublisherName[[1]]
Fields[["BookPublisherLocation"]] <- x$BookDocument$Book$Publisher$PublisherLocation[[1]]
Fields[["BookTitle"]] <- x$BookDocument$Book$BookTitle[[1]]
Fields[["BookBeginningDate"]] <- x$BookDocument$Book$BeginningDate$Year[[1]]
Fields[["BookEndingDate"]] <- x$BookDocument$Book$EndingDate$Year[[1]]
Fields[["BookMedium"]] <- x$BookDocument$Book$Medium[[1]]


if(!is.null(x$BookDocument$GrantList)){
	
	Grants <- do.call("rbind", lapply(x$BookDocument$GrantList, function(z){
		data.frame(
		GrantID = ifelse(is.null(z$GrantID[[1]]), NA, z$GrantID[[1]]),
		Agency =  ifelse(is.null(z$Agency[[1]]), NA, z$Agency[[1]]),
		stringsAsFactors = F
		)	
	}))
	
	Fields[["GrantID"]] <- Grants
}

Fields[["COIStatement"]] <- x$BookDocument$CoiStatement[[1]]

if(!is.null(x$BookDocument$KeywordList)){
	keywords <- x$BookDocument$KeywordList[names(x$BookDocument$KeywordList) == "Keyword"]
	Fields[["Keywords"]] <- unlist(keywords)
}


if(!is.null(x$BookDocument$MeshHeadingList$MeshHeading)){
	Mesh <- data.frame(
		  	   Heading = unlist(x$BookDocument$MeshHeadingList),
               Type = ifelse(grepl("DescriptorName", names(unlist(x$BookDocument$MeshHeadingList))),"Descriptor","Qualifier"),
               stringsAsFactors = F
               )
               
   Fields[["Mesh"]] <- Mesh
 }
 
 
if(!is.null(x$PubmedBookData$ReferenceList)){
	References <- lapply(x$PubmedBookData$ReferenceList, function(z){
		paste(unlist(z), collapse = "")
	})
               
   Fields[["Citations"]] <- References
} 

Fields
}
