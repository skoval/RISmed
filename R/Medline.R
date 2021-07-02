setClass("Medline",
	representation(
			Query = "character",
			PMID = "character",

			YearRevised = "numeric",
            MonthRevised = "numeric",
            DayRevised= "numeric",
	
			YearPubDate = "numeric",
            MonthPubDate = "character",
            DayPubDate= "numeric",
	
			YearArticleDate = "numeric",
            MonthArticleDate = "numeric",
            DayArticleDate= "numeric",

	
			YearEntrez = "numeric",
            MonthEntrez = "numeric",
            DayEntrez= "numeric",
			HourEntrez= "numeric",
			MinuteEntrez= "numeric",           

	
			YearMedline = "numeric",
            MonthMedline = "numeric",
            DayMedline= "numeric",
			HourMedline= "numeric",
			MinuteMedline= "numeric",           
	
			YearReceived = "numeric",
            MonthReceived = "numeric",
            DayReceived= "numeric",
			HourReceived= "numeric",
			MinuteReceived= "numeric",           
			
			YearAccepted = "numeric",
            MonthAccepted = "numeric",
            DayAccepted= "numeric",
			HourAccepted= "numeric",
			MinuteAccepted= "numeric",  

			YearEpublish = "numeric",
            MonthEpublish = "numeric",
            DayEpublish= "numeric",
			HourEpublish= "numeric",
			MinuteEpublish= "numeric",           
			
			YearPpublish = "numeric",
            MonthPpublish = "numeric",
            DayPpublish= "numeric",
			HourPpublish= "numeric",
			MinutePpublish= "numeric",  

			YearPmc = "numeric",
            MonthPmc  = "numeric",
            DayPmc = "numeric",
			HourPmc = "numeric",
			MinutePmc = "numeric",  

			YearPubmed = "numeric",
            MonthPubmed  = "numeric",
            DayPubmed = "numeric",
			HourPubmed = "numeric",
			MinutePubmed = "numeric", 
												 
            Author = "list",
            ISSN= "character",
            Title = "character",
            ArticleTitle= "character",
			ELocationID= "character",
			AbstractText= "character",
			Affiliation= "list",
			Language= "character",
			PublicationType= "character",
			MedlineTA= "character",
			NlmUniqueID= "character",
			ISSNLinking= "character",
			PublicationStatus= "character",
			ArticleId= "character",
			DOI = "character",
			Volume= "character",
			Issue= "character",
			ISOAbbreviation= "character",
			MedlinePgn= "character",
			CopyrightInformation= "character",
			Country= "character",
			GrantID= "list",
			COIStatement = "character",
            Mesh="list",
            Keywords="list",
            Citations="list")
)

null_replace <- function(x) ifelse(is.null(x), NA, x)

list_null_replace <- function(x) if(class(x) == "NULL") NA else x

Medline <- function(object, query = character(0)){
    
    TagIndex <- lapply(object, names)
    
	# ARTICLE LIST FROM PUBMED QUERY
	PMID <- sapply(object, function(x) null_replace(x[["PMID"]]),USE.NAMES=FALSE)
	
	YearRevised <- sapply(object, function(x) null_replace(x[["YearRevised"]]),USE.NAMES=FALSE)
	
	MonthRevised <- sapply(object, function(x) null_replace(x[["MonthRevised"]]),USE.NAMES=FALSE)
	
	DayRevised <- sapply(object, function(x) null_replace(x[["DayRevised"]]),USE.NAMES=FALSE)

	YearArticleDate <- sapply(object, function(x) null_replace(x[["YearArticleDate"]]),USE.NAMES=FALSE)
	
	MonthArticleDate <- sapply(object, function(x) null_replace(x[["MonthArticleDate"]]),USE.NAMES=FALSE)
	
	
	DayArticleDate <- sapply(object, function(x) null_replace(x[["DayArticleDate"]]),USE.NAMES=FALSE)
	

	YearPubDate <-sapply(object, function(x) null_replace(x[["YearPubDate"]]),USE.NAMES=FALSE)
	
	MonthPubDate <- sapply(object, function(x) null_replace(x[["MonthPubDate"]]),USE.NAMES=FALSE)
	
	DayPubDate <- sapply(object, function(x) null_replace(x[["DayPubDate"]]),USE.NAMES=FALSE)


	YearEntrez <- sapply(object, function(x) null_replace(x[["YearEntrez"]]),USE.NAMES=FALSE)

	MonthEntrez <- sapply(object, function(x) null_replace(x[["MonthEntrez"]]),USE.NAMES=FALSE)

	DayEntrez <- sapply(object, function(x) null_replace(x[["DayEntrez"]]),USE.NAMES=FALSE)

	MinuteEntrez <- sapply(object, function(x) null_replace(x[["MinuteEnrez"]]),USE.NAMES=FALSE)

	HourEntrez <- sapply(object, function(x) null_replace(x[["HourEntrez"]]),USE.NAMES=FALSE)


	YearMedline <- sapply(object, function(x) null_replace(x[["YearMedline"]]),USE.NAMES=FALSE)

	MonthMedline <- sapply(object, function(x) null_replace(x[["MonthMedline"]]),USE.NAMES=FALSE)

	DayMedline <- sapply(object, function(x) null_replace(x[["DayMedline"]]),USE.NAMES=FALSE)

	MinuteMedline <- sapply(object, function(x) null_replace(x[["MinuteMedline"]]),USE.NAMES=FALSE)

	HourMedline <- sapply(object, function(x) null_replace(x[["HourMedline"]]),USE.NAMES=FALSE)

	YearReceived <- sapply(object, function(x) null_replace(x[["YearReceived"]]),USE.NAMES=FALSE)

	MonthReceived <- sapply(object, function(x) null_replace(x[["MonthReceived"]]),USE.NAMES=FALSE)

	DayReceived <- sapply(object, function(x) null_replace(x[["DayReceived"]]),USE.NAMES=FALSE)

	MinuteReceived <- sapply(object, function(x) null_replace(x[["MinuteReceived"]]),USE.NAMES=FALSE)

	HourReceived <- sapply(object, function(x) null_replace(x[["HourReceived"]]),USE.NAMES=FALSE)

	YearEpublish <- sapply(object, function(x) null_replace(x[["YearEpublish"]]),USE.NAMES=FALSE)

	MonthEpublish <- sapply(object, function(x) null_replace(x[["MonthEpublish"]]),USE.NAMES=FALSE)

	DayEpublish <- sapply(object, function(x) null_replace(x[["DayEpublish"]]),USE.NAMES=FALSE)

	MinuteEpublish <- sapply(object, function(x) null_replace(x[["MinuteEpublish"]]),USE.NAMES=FALSE)

	HourEpublish <- sapply(object, function(x) null_replace(x[["HourEpublish"]]),USE.NAMES=FALSE)
	
	YearPpublish <- sapply(object, function(x) null_replace(x[["YearPpublish"]]),USE.NAMES=FALSE)

	MonthPpublish <- sapply(object, function(x) null_replace(x[["MonthPpublish"]]),USE.NAMES=FALSE)

	DayPpublish <- sapply(object, function(x) null_replace(x[["DayPpublish"]]),USE.NAMES=FALSE)

	MinutePpublish <- sapply(object, function(x) null_replace(x[["MinutePpublish"]]),USE.NAMES=FALSE)

	HourPpublish <- sapply(object, function(x) null_replace(x[["HourPpublish"]]),USE.NAMES=FALSE)
	

	YearPmc <- sapply(object, function(x) null_replace(x[["YearPmc"]]),USE.NAMES=FALSE)

	MonthPmc <- sapply(object, function(x) null_replace(x[["MonthPmc"]]),USE.NAMES=FALSE)

	DayPmc <- sapply(object, function(x) null_replace(x[["DayPmc"]]),USE.NAMES=FALSE)

	MinutePmc <- sapply(object, function(x) null_replace(x[["MinutePmc"]]),USE.NAMES=FALSE)

	HourPmc <- sapply(object, function(x) null_replace(x[["HourPmc"]]),USE.NAMES=FALSE)
	
	YearPubmed <- sapply(object, function(x) null_replace(x[["YearPubmed"]]),USE.NAMES=FALSE)

	MonthPubmed <- sapply(object, function(x) null_replace(x[["MonthPubmed"]]),USE.NAMES=FALSE)

	DayPubmed <- sapply(object, function(x) null_replace(x[["DayPubmed"]]),USE.NAMES=FALSE)

	MinutePubmed <- sapply(object, function(x) null_replace(x[["MinutePubmed"]]),USE.NAMES=FALSE)

	HourPubmed <- sapply(object, function(x) null_replace(x[["HourPubmed"]]),USE.NAMES=FALSE)
	
	YearAccepted <- sapply(object, function(x) null_replace(x[["YearAccepted"]]),USE.NAMES=FALSE)

	MonthAccepted <- sapply(object, function(x) null_replace(x[["MonthAccepted"]]),USE.NAMES=FALSE)

	DayAccepted <- sapply(object, function(x) null_replace(x[["DayAccepted"]]),USE.NAMES=FALSE)

	MinuteAccepted <- sapply(object, function(x) null_replace(x[["MinuteAccepted"]]),USE.NAMES=FALSE)

	HourAccepted <- sapply(object, function(x) null_replace(x[["HourAccepted"]]),USE.NAMES=FALSE)

    AbstractText <- lapply(object, function(x) list_null_replace(x[["AbstractText"]]))
    Author <- lapply(object, function(x) list_null_replace(x[["Author"]]))
    Affiliation <- lapply(object, function(x) list_null_replace(x[["Affiliation"]]))
    
    names(AbstractText) <- PMID
    names(Author) <- PMID
    names(Affiliation) <- PMID
    
	ISSN <- sapply(object, function(x) null_replace(x[["ISSN"]]),USE.NAMES=FALSE)
	Title <- sapply(object, function(x) null_replace(x[["Title"]]),USE.NAMES=FALSE)
	ArticleTitle <- sapply(object, function(x) null_replace(x[["ArticleTitle"]]),USE.NAMES=FALSE)
	ELocationID <- sapply(object, function(x) null_replace(x[["ELocationID"]]),USE.NAMES=FALSE)
	Language <- sapply(object, function(x) null_replace(x[["Language"]]),USE.NAMES=FALSE)
	PublicationType <- sapply(object, function(x) null_replace(x[["PublicationType"]]),USE.NAMES=FALSE)
	MedlineTA <- sapply(object, function(x) null_replace(x[["MedlineTA"]]),USE.NAMES=FALSE)
	NlmUniqueID <- sapply(object, function(x) null_replace(x[["NlmUniqueID"]]),USE.NAMES=FALSE)
	ISSNLinking <- sapply(object, function(x) null_replace(x[["ISSNLinking"]]),USE.NAMES=FALSE)
	PublicationStatus <- sapply(object, function(x) null_replace(x[["PublicationStatus"]]),USE.NAMES=FALSE)
	ArticleId <- sapply(object, function(x) null_replace(x[["ArticleId"]]),USE.NAMES=FALSE)
	DOI <- sapply(object, function(x) null_replace(x[["DOI"]]),USE.NAMES=FALSE)
	Volume <- sapply(object, function(x) null_replace(x[["Volume"]]),USE.NAMES=FALSE)
	Issue <- sapply(object, function(x) null_replace(x[["Issue"]]),USE.NAMES=FALSE)
	ISOAbbreviation <- sapply(object, function(x) null_replace(x[["ISOAbbreviation"]]),USE.NAMES=FALSE)
	MedlinePgn <- sapply(object, function(x) null_replace(x[["MedlinePgn"]]),USE.NAMES=FALSE)
	CopyrightInformation <- sapply(object, function(x) null_replace(x[["CopyrightInformation"]]),USE.NAMES=FALSE)
	Country <- sapply(object, function(x) null_replace(x[["Country"]]),USE.NAMES=FALSE)
	GrantID <- lapply(object, function(x) list_null_replace(x[["GrantID"]]))
	COIStatement <- sapply(object, function(x) null_replace(x[["CoiStatement"]]),USE.NAMES=FALSE)

	Mesh <- lapply(object, function(x) list_null_replace(x[["Mesh"]]))
	Keywords <- lapply(object, function(x) list_null_replace(x[["Keywords"]]))
	Citations <- lapply(object, function(x) list_null_replace(x[["Citations"]]))
	
	names(GrantID) <- PMID
	names(Mesh) <- PMID	
	names(Keywords) <- PMID
	names(Citations) <- PMID

	PMID <- as.character(PMID)
	
	YearRevised <- as.numeric(YearRevised)
	MonthRevised <- as.numeric(MonthRevised)
	DayRevised <- as.numeric(DayRevised)
	YearPubDate <- as.numeric(YearPubDate)
	MonthPubDate <- as.character(MonthPubDate)
	DayPubDate <- as.numeric(DayPubDate)
	YearArticleDate <- as.numeric(YearArticleDate)
	MonthArticleDate <- as.numeric(MonthArticleDate)
	DayArticleDate <- as.numeric(DayArticleDate)

	YearEntrez <- as.numeric(YearEntrez)
	MonthEntrez <- as.numeric(MonthEntrez)
	DayEntrez <- as.numeric(DayEntrez)
	HourEntrez <- as.numeric(HourEntrez)
	MinuteEntrez <- as.numeric(MinuteEntrez)
	YearMedline <- as.numeric(YearMedline)
	MonthMedline <- as.numeric(MonthMedline)
	DayMedline <- as.numeric(DayMedline)
	HourMedline <- as.numeric(HourMedline)
	MinuteMedline <- as.numeric(MinuteMedline)
	
	YearAccepted <- as.numeric(YearAccepted)
	YearReceived <- as.numeric(YearReceived)
	YearEpublish <- as.numeric(YearEpublish)
	YearPpublish <- as.numeric(YearPpublish)
	YearPmc <- as.numeric(YearPmc)
	YearPubmed <- as.numeric(YearPubmed)

	MonthAccepted <- as.numeric(MonthAccepted)
	MonthReceived <- as.numeric(MonthReceived)
	MonthEpublish <- as.numeric(MonthEpublish)
	MonthPpublish <- as.numeric(MonthPpublish)
	MonthPmc <- as.numeric(MonthPmc)
	MonthPubmed <- as.numeric(MonthPubmed)

	DayAccepted <- as.numeric(DayAccepted)
	DayReceived <- as.numeric(DayReceived)
	DayEpublish <- as.numeric(DayEpublish)
	DayPpublish <- as.numeric(DayPpublish)
	DayPmc <- as.numeric(DayPmc)
	DayPubmed <- as.numeric(DayPubmed)

	HourAccepted <- as.numeric(HourAccepted)
	HourReceived <- as.numeric(HourReceived)
	HourEpublish <- as.numeric(HourEpublish)
	HourPpublish <- as.numeric(HourPpublish)
	HourPmc <- as.numeric(HourPmc)
	HourPubmed <- as.numeric(HourPubmed)

	MinuteAccepted <- as.numeric(MinuteAccepted)
	MinuteReceived <- as.numeric(MinuteReceived)
	MinuteEpublish <- as.numeric(MinuteEpublish)
	MinutePpublish <- as.numeric(MinutePpublish)
	MinutePmc <- as.numeric(MinutePmc)
	MinutePubmed <- as.numeric(MinutePubmed)

	ISSN <- as.character(ISSN)
	Title <- as.character(Title)
	ArticleTitle <- as.character(ArticleTitle)
	ELocationID <- as.character(ELocationID)
	AbstractText <- as.character(AbstractText)
	Affiliation <- Affiliation
	Language <- as.character(Language)
	PublicationType <- as.character(PublicationType)
	MedlineTA <- as.character(MedlineTA)
	NlmUniqueID <- as.character(NlmUniqueID)
	ISSNLinking <- as.character(ISSNLinking)
	PublicationStatus <- as.character(PublicationStatus)
	ArticleId <- as.character(ArticleId)
	DOI <- as.character(DOI)
	Volume <- as.character(Volume)
	Issue <- as.character(Issue)
	ISOAbbreviation <- as.character(ISOAbbreviation)
	MedlinePgn <- as.character(MedlinePgn)
	CopyrightInformation <- as.character(CopyrightInformation)
	Country <- as.character(Country)
	GrantID <- GrantID
	COIStatement <- as.character(COIStatement)

  	
	new("Medline",
			Query = query,
			PMID = PMID,
			YearRevised = YearRevised, 
		    MonthRevised = MonthRevised , 
		    DayRevised  = DayRevised, 
			YearPubDate = YearPubDate, 
		    MonthPubDate = MonthPubDate , 
		    DayPubDate  = DayPubDate, 
			YearArticleDate = YearArticleDate, 
		    MonthArticleDate = MonthArticleDate , 
		    DayArticleDate  = DayArticleDate, 
			YearEntrez = YearEntrez, 
		    MonthEntrez = MonthEntrez , 
		    DayEntrez  = DayEntrez, 
			HourEntrez = HourEntrez, 
			MinuteEntrez = MinuteEntrez, 	
			YearMedline = YearMedline, 
		    MonthMedline = MonthMedline , 
		    DayMedline  = DayMedline, 
			HourMedline = HourMedline, 
			MinuteMedline = MinuteMedline, 				
			YearAccepted = YearAccepted, 
		    MonthAccepted = MonthAccepted , 
		    DayAccepted  = DayAccepted, 
			HourAccepted = HourAccepted, 
			MinuteAccepted = MinuteAccepted, 	
			YearReceived = YearReceived, 
		    MonthReceived = MonthReceived, 
		    DayReceived  = DayReceived, 
			HourReceived = HourReceived, 
			MinuteReceived = MinuteReceived, 
			YearEpublish = YearEpublish, 
		    MonthEpublish = MonthEpublish , 
		    DayEpublish  = DayEpublish, 
			HourEpublish = HourEpublish, 
			MinuteEpublish = MinuteEpublish, 
			YearPpublish = YearPpublish, 
		    MonthPpublish = MonthPpublish , 
		    DayPpublish  = DayPpublish, 
			HourPpublish = HourPpublish, 
			MinutePpublish = MinutePpublish, 
			YearPmc = YearPmc, 
		    MonthPmc = MonthPmc , 
		    DayPmc  = DayPmc, 
			HourPmc = HourPmc, 
			MinutePmc = MinutePmc, 													    
			YearPubmed = YearPubmed, 
		    MonthPubmed = MonthPubmed , 
		    DayPubmed  = DayPubmed, 
			HourPubmed = HourPubmed, 
			MinutePubmed = MinutePubmed, 											
		    ISSN  = ISSN, 
		    Title  = Title, 
		    Author = Author,
		    ArticleTitle = ArticleTitle, 
			ELocationID = ELocationID, 
			AbstractText = AbstractText, 
			Affiliation = Affiliation, 
			Language = Language, 
			PublicationType = PublicationType, 
			MedlineTA = MedlineTA, 
			NlmUniqueID = NlmUniqueID, 
			ISSNLinking = ISSNLinking, 
			PublicationStatus = PublicationStatus, 
			ArticleId = ArticleId, 
			DOI = DOI, 
			Volume = Volume, 
			Issue = Issue, 
			ISOAbbreviation = ISOAbbreviation, 
			MedlinePgn = MedlinePgn, 
			CopyrightInformation = CopyrightInformation, 
			Country = Country, 
			GrantID = GrantID, 
			COIStatement = COIStatement,
            Mesh = Mesh,
            Keywords = Keywords,
            Citations = Citations
	)
}


setMethod("print","Medline",function(x,...){
		cat("PubMed query: ",x@Query,"\n\n")
		cat("Records: ",length(x@PMID),"\n")
})

setMethod("show","Medline",function(object){
		cat("PubMed query: ",object@Query,"\n\n")
		cat("Records: ",length(object@PMID),"\n")
})

setMethod("Query","Medline",function(object) object@Query)                                
setMethod("PMID","Medline",function(object) object@PMID)                                
setMethod("YearRevised","Medline",function(object) object@YearRevised)                                
setMethod("MonthRevised","Medline",function(object) object@MonthRevised)                              
setMethod("DayRevised","Medline",function(object) object@DayRevised)                                  
setMethod("YearPubDate","Medline",function(object) object@YearPubDate)                                
setMethod("MonthPubDate","Medline",function(object) object@MonthPubDate)                              
setMethod("DayPubDate","Medline",function(object) object@DayPubDate)                                  
setMethod("YearArticleDate","Medline",function(object) object@YearArticleDate)                                
setMethod("MonthArticleDate","Medline",function(object) object@MonthArticleDate)                              
setMethod("DayArticleDate","Medline",function(object) object@DayArticleDate)                                  
setMethod("YearEntrez","Medline",function(object) object@YearEntrez)                                
setMethod("MonthEntrez","Medline",function(object) object@MonthEntrez)                              
setMethod("DayEntrez","Medline",function(object) object@DayEntrez)                                  
setMethod("HourEntrez","Medline",function(object) object@HourEntrez)                                
setMethod("MinuteEntrez","Medline",function(object) object@MinuteEntrez)                            
setMethod("YearMedline","Medline",function(object) object@YearMedline)                                
setMethod("MonthMedline","Medline",function(object) object@MonthMedline)                              
setMethod("DayMedline","Medline",function(object) object@DayMedline)                                  
setMethod("HourMedline","Medline",function(object) object@HourMedline)                                
setMethod("MinuteMedline","Medline",function(object) object@MinuteMedline)                            
setMethod("YearAccepted","Medline",function(object) object@YearAccepted)                                
setMethod("MonthAccepted","Medline",function(object) object@MonthAccepted)                              
setMethod("DayAccepted","Medline",function(object) object@DayAccepted)                                  
setMethod("HourAccepted","Medline",function(object) object@HourAccepted)                                
setMethod("MinuteAccepted","Medline",function(object) object@MinuteAccepted)                            
setMethod("YearReceived","Medline",function(object) object@YearReceived)                                
setMethod("MonthReceived","Medline",function(object) object@MonthReceived)                              
setMethod("DayReceived","Medline",function(object) object@DayReceived)                                  
setMethod("HourReceived","Medline",function(object) object@HourReceived)                                
setMethod("MinuteReceived","Medline",function(object) object@MinuteReceived)  
setMethod("YearEpublish","Medline",function(object) object@YearEpublish)                                
setMethod("MonthEpublish","Medline",function(object) object@MonthEpublish)                              
setMethod("DayEpublish","Medline",function(object) object@DayEpublish)                                  
setMethod("HourEpublish","Medline",function(object) object@HourEpublish)                                
setMethod("MinuteEpublish","Medline",function(object) object@MinuteEpublish)  
setMethod("YearPpublish","Medline",function(object) object@YearPpublish)                                
setMethod("MonthPpublish","Medline",function(object) object@MonthPpublish)                              
setMethod("DayPpublish","Medline",function(object) object@DayPpublish)                                  
setMethod("HourPpublish","Medline",function(object) object@HourPpublish)                                
setMethod("MinutePpublish","Medline",function(object) object@MinutePpublish)  
setMethod("YearPmc","Medline",function(object) object@YearPmc)                                
setMethod("MonthPmc","Medline",function(object) object@MonthPmc)                              
setMethod("DayPmc","Medline",function(object) object@DayPmc)                                  
setMethod("HourPmc","Medline",function(object) object@HourPmc)                                
setMethod("MinutePmc","Medline",function(object) object@MinutePmc)  
setMethod("YearPubmed","Medline",function(object) object@YearPubmed)                                
setMethod("MonthPubmed","Medline",function(object) object@MonthPubmed)                              
setMethod("DayPubmed","Medline",function(object) object@DayPubmed)                                  
setMethod("HourPubmed","Medline",function(object) object@HourPubmed)                                
setMethod("MinutePubmed","Medline",function(object) object@MinutePubmed)  
setMethod("Author","Medline",function(object) object@Author)                            
setMethod("ISSN","Medline",function(object) object@ISSN)                                
setMethod("Title","Medline",function(object) object@Title)                              
setMethod("ArticleTitle","Medline",function(object) object@ArticleTitle)                
setMethod("ELocationID","Medline",function(object) object@ELocationID)                  
setMethod("AbstractText","Medline",function(object) object@AbstractText)                
setMethod("Affiliation","Medline",function(object) object@Affiliation)                  
setMethod("Language","Medline",function(object) object@Language)                        
setMethod("PublicationType","Medline",function(object) object@PublicationType)          
setMethod("MedlineTA","Medline",function(object) object@MedlineTA)                      
setMethod("NlmUniqueID","Medline",function(object) object@NlmUniqueID)                  
setMethod("ISSNLinking","Medline",function(object) object@ISSNLinking)                  
setMethod("PublicationStatus","Medline",function(object) object@PublicationStatus)      
setMethod("ArticleId","Medline",function(object) object@ArticleId)       
setMethod("DOI","Medline",function(object) object@DOI)                
setMethod("Volume","Medline",function(object) object@Volume)                            
setMethod("Issue","Medline",function(object) object@Issue)                              
setMethod("ISOAbbreviation","Medline",function(object) object@ISOAbbreviation)          
setMethod("MedlinePgn","Medline",function(object) object@MedlinePgn)                    
setMethod("CopyrightInformation","Medline",function(object) object@CopyrightInformation)
setMethod("Country","Medline",function(object) object@Country)                          
setMethod("GrantID","Medline",function(object) object@GrantID)                          
setMethod("COIStatement","Medline",function(object) object@COIStatement) 
setMethod("Mesh","Medline",function(object) object@Mesh)
setMethod("Keywords","Medline",function(object) object@Keywords)
setMethod("Citations","Medline",function(object) object@Citations)

