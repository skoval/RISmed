##############################################################
#' Concatenate Medline 
#'
#' Concatenates multiple Medline objects
#'
#' @param x a Medline object
#' @param ... Additional Medline objects to concatenate
#'
#' @return A Medline object
#' @export
#' @docType methods
#' @rdname c-methods
#' @aliases c,ANY,ANY-method
setMethod("c","Medline", function(x, ...){
	
	slots <- slotNames(x)
	
	addx <- list(...)
	addx <- c(list(x), addx)
	
	# Make a list of all the components
	GetComponents <- lapply(addx, function(y){
		result <- lapply(slots, function(z) slot(y, z))
		names(result) <- slots
	result
	})
	
	MeshTerms <- list()
	for(x in GetComponents)
		MeshTerms <- c(MeshTerms, x[["Mesh"]])

	Authors <- list()
	for(x in GetComponents)
		Authors <- c(Authors, x[["Author"]])

	Affilitations <- list()
	for(x in GetComponents)
		Affilitations <- c(Affilitations, x[["Affiliation"]])

	Keywords <- list()
	for(x in GetComponents)
		Keywords<- c(Keywords, x[["Keywords"]])
		
	Citations <- list()
	for(x in GetComponents)
		Citations <- c(Citations, x[["Citations"]])		

	GrantID <- list()
	for(x in GetComponents)
		GrantID <- c(GrantID, x[["GrantID"]])		

		
	new("Medline",
			Query = sapply(GetComponents, function(x) x[["Query"]]),
			PMID = unlist(lapply(GetComponents, function(x) x[["PMID"]])),
			YearRevised = unlist(lapply(GetComponents, function(x) x[["YearRevised"]])),
		    MonthRevised = unlist(lapply(GetComponents, function(x) x[["MonthRevised"]])),
		    DayRevised  = unlist(lapply(GetComponents, function(x) x[["DayRevised"]])),
			YearPubDate = unlist(lapply(GetComponents, function(x) x[["YearPubDate"]])),
		    MonthPubDate = unlist(lapply(GetComponents, function(x) x[["MonthPubDate"]])),
		    DayPubDate  = unlist(lapply(GetComponents, function(x) x[["DayPubDate"]])),
			YearArticleDate = unlist(lapply(GetComponents, function(x) x[["YearArticleDate"]])),
		    MonthArticleDate = unlist(lapply(GetComponents, function(x) x[["MonthArticleDate"]])),
		    DayArticleDate  = unlist(lapply(GetComponents, function(x) x[["DayArticleDate"]])),
		YearEntrez = unlist(lapply(GetComponents, function(x) x[["YearEntrez"]])),
		    MonthEntrez = unlist(lapply(GetComponents, function(x) x[["MonthEntrez"]])),
		    DayEntrez  = unlist(lapply(GetComponents, function(x) x[["DayEntrez"]])),
			HourEntrez = unlist(lapply(GetComponents, function(x) x[["HourEntrez"]])),			
			MinuteEntrez = unlist(lapply(GetComponents, function(x) x[["MinuteEntrez"]])),	
		YearMedline = unlist(lapply(GetComponents, function(x) x[["YearMedline"]])),
		    MonthMedline = unlist(lapply(GetComponents, function(x) x[["MonthMedline"]])),
		    DayMedline  = unlist(lapply(GetComponents, function(x) x[["DayMedline"]])),
			HourMedline = unlist(lapply(GetComponents, function(x) x[["HourMedline"]])),			
			MinuteMedline = unlist(lapply(GetComponents, function(x) x[["MinuteMedline"]])),	
			YearAccepted = unlist(lapply(GetComponents, function(x) x[["YearAccepted"]])),
		    MonthAccepted = unlist(lapply(GetComponents, function(x) x[["MonthAccepted"]])),
		    DayAccepted  = unlist(lapply(GetComponents, function(x) x[["DayAccepted"]])),
			HourAccepted = unlist(lapply(GetComponents, function(x) x[["HourAccepted"]])),			
			MinuteAccepted = unlist(lapply(GetComponents, function(x) x[["MinuteAccepted"]])),	
			YearReceived = unlist(lapply(GetComponents, function(x) x[["YearReceived"]])),
		    MonthReceived = unlist(lapply(GetComponents, function(x) x[["MonthReceived"]])),
		    DayReceived  = unlist(lapply(GetComponents, function(x) x[["DayReceived"]])),
			HourReceived = unlist(lapply(GetComponents, function(x) x[["HourReceived"]])),
			MinuteReceived = unlist(lapply(GetComponents, function(x) x[["MinuteReceived"]])),
			YearEpublish = unlist(lapply(GetComponents, function(x) x[["YearEpublish"]])),
		    MonthEpublish = unlist(lapply(GetComponents, function(x) x[["MonthEpublish"]])),
		    DayEpublish  = unlist(lapply(GetComponents, function(x) x[["DayEpublish"]])),
			HourEpublish = unlist(lapply(GetComponents, function(x) x[["HourEpublish"]])),
			MinuteEpublish = unlist(lapply(GetComponents, function(x) x[["MinuteEpublish"]])),
			YearPpublish = unlist(lapply(GetComponents, function(x) x[["YearPpublish"]])),
		    MonthPpublish = unlist(lapply(GetComponents, function(x) x[["MonthPpublish"]])),
		    DayPpublish  = unlist(lapply(GetComponents, function(x) x[["DayPpublish"]])),
			HourPpublish = unlist(lapply(GetComponents, function(x) x[["HourPpublish"]])),
			MinutePpublish = unlist(lapply(GetComponents, function(x) x[["MinutePpublish"]])),
			YearPmc = unlist(lapply(GetComponents, function(x) x[["YearPmc"]])),
		    MonthPmc = unlist(lapply(GetComponents, function(x) x[["MonthPmc"]])),
		    DayPmc  = unlist(lapply(GetComponents, function(x) x[["DayPmc"]])),
			HourPmc = unlist(lapply(GetComponents, function(x) x[["HourPmc"]])),
			MinutePmc = unlist(lapply(GetComponents, function(x) x[["MinutePmc"]])),												    
			YearPubmed = unlist(lapply(GetComponents, function(x) x[["YearPubmed"]])),
		    MonthPubmed = unlist(lapply(GetComponents, function(x) x[["MonthPubmed"]])),
		    DayPubmed  = unlist(lapply(GetComponents, function(x) x[["DayPubmed"]])),
			HourPubmed = unlist(lapply(GetComponents, function(x) x[["HourPubmed"]])),
			MinutePubmed = unlist(lapply(GetComponents, function(x) x[["MinutePubmed"]])),													    
		    ISSN  = unlist(lapply(GetComponents, function(x) x[["ISSN"]])),
		    Title  = unlist(lapply(GetComponents, function(x) x[["Title"]])),		    
		    Author = Authors,
		    ArticleTitle = unlist(lapply(GetComponents, function(x) x[["ArticleTitle"]])),
			ELocationID = unlist(lapply(GetComponents, function(x) x[["ELocationID"]])),
			AbstractText = unlist(lapply(GetComponents, function(x) x[["AbstractText"]])),
			Affiliation = Affilitations,
			Language = unlist(lapply(GetComponents, function(x) x[["Language"]])),
			PublicationType =  unlist(lapply(GetComponents, function(x) x[["PublicationType"]])),
			MedlineTA = unlist(lapply(GetComponents, function(x) x[["MedlineTA"]])),
			NlmUniqueID = unlist(lapply(GetComponents, function(x) x[["NlmUniqueID"]])),
			ISSNLinking = unlist(lapply(GetComponents, function(x) x[["ISSNLinking"]])),
			PublicationStatus = unlist(lapply(GetComponents, function(x) x[["PublicationStatus"]])),
			ArticleId = unlist(lapply(GetComponents, function(x) x[["ArticleId"]])),
			DOI = unlist(lapply(GetComponents, function(x) x[["DOI"]])),
			Volume = unlist(lapply(GetComponents, function(x) x[["Volume"]])),
			Issue = unlist(lapply(GetComponents, function(x) x[["Issue"]])),
			ISOAbbreviation = unlist(lapply(GetComponents, function(x) x[["ISOAbbreviation"]])),
			MedlinePgn = unlist(lapply(GetComponents, function(x) x[["MedlinePgn"]])),
			CopyrightInformation = unlist(lapply(GetComponents, function(x) x[["CopyrightInformation"]])),
			Country = unlist(lapply(GetComponents, function(x) x[["Country"]])),
			GrantID = GrantID,
	COIStatement = unlist(lapply(GetComponents, function(x) x[["COIStatement"]])),	
            Mesh = MeshTerms,
            Keywords = Keywords,
            Citations = Citations
	)
	
})