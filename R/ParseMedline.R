ParseMedline <- function(x){

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
"Citations")

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
		i <- which(pubmed_states == "entrez")
		Fields[["YearEntrez"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthEntrez"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayEntrez"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteEntrez"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourEntrez"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}
	
	if(any(pubmed_states == "medline")){
		i <- which(pubmed_states == "medline")
		Fields[["YearMedline"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthMedline"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayMedline"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteMedline"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourMedline"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "accepted")){
		i <- which(pubmed_states == "accepted")
		Fields[["YearAccepted"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthAccepted"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayAccepted"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteAccepted"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourAccepted"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "received")){
		i <- which(pubmed_states == "received")
		Fields[["YearReceived"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthReceived"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayReceived"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteReceived"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourReceived"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}	
	
	if(any(pubmed_states == "epublish")){
		i <- which(pubmed_states == "epublish")
		Fields[["YearEpublish"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthEpublish"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayEpublish"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinuteEpublish"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourEpublish"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}			

	if(any(pubmed_states == "ppublish")){
		i <- which(pubmed_states == "ppublish")
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
		i <- which(pubmed_states == "pubmed")
		Fields[["YearPubmed"]] <- x$PubmedData$History[[i]]$Year[[1]]
		Fields[["MonthPubmed"]] <- x$PubmedData$History[[i]]$Month[[1]]
		Fields[["DayPubmed"]] <- x$PubmedData$History[[i]]$Day[[1]]
		Fields[["MinutePubmed"]] <- x$PubmedData$History[[i]]$Minute[[1]]
		Fields[["HourPubmed"]] <- x$PubmedData$History[[i]]$Hour[[1]]
	}
}

Fields[["ISSN"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$ISSN[[1]]
Fields[["Volume"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$Volume[[1]]
Fields[["Issue"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$Issue[[1]]
Fields[["Title"]] <-  x$MedlineCitation$Article$Journal$JournalIssue$Title[[1]]

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
			Fields[["ELocationID"]] <- e$ELocationID[[1]]
			
		if(attr(e, "EIdType") == "doi")
			Fields[["DOI"]] <- e$ELocationID[[1]]			
	}	
}

if(!is.null(x$MedlineCitation$Article$Abstract)){
	AbstractText <- paste(unlist(x$MedlineCitation$Article$Abstract[names(x$MedlineCitation$Article$Abstract) == "AbstractText"]), collapse = " ")
	
	Fields[["AbstractText"]] <- AbstractText
}

Fields[["Language"]] <- x$MedlineCitation$Article$Language[[1]]
Fields[["PublicationType"]] <- x$MedlineCitation$Article$PublicationTypeList$PublicationType[[1]]
Fields[["Country"]] <- x$MedlineCitation$Article$Country[[1]]
Fields[["MedlineTA"]] <- x$MedlineCitation$Article$MedlineTA[[1]]
Fields[["NlmUniqueID"]] <- x$MedlineCitation$Article$NlmUniqueID[[1]]
Fields[["ISSNLinking"]] <- x$MedlineCitation$Article$ISSNLinking[[1]]
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