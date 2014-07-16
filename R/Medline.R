GetAuthors <- function(object){

	names <- names(object)
	
	first.check <- grep("ForeName",names)
	initial.check <- grep("Initials",names)
	last.index <- grep("LastName",names)
	
	first.index <- last.index+1
	initial.index <- last.index+2
	
	initial.index[!(initial.index%in%initial.check)] <- NA
	first.index[!(first.index%in%first.check)] <- NA

	if(length(last.index)==0){ # NO AUTHORS LISTED
	
	df <- data.frame(
		LastName = NA,
		ForeName = NA,
		Initials = NA
	)

	df$order <- NA
	
	}
	else{
	
	df <- data.frame(
		LastName = as.character(object[last.index]),
		ForeName = as.character(object[first.index]),
		Initials = as.character(object[initial.index]),
		stringsAsFactors=FALSE)

	df$order <- 1:nrow(df)
	
	}
df
}


# # GetAuthors <- function(object){

	# names <- names(object)
	
	# first.index <- grep("ForeName",names)
	# initial.index <- grep("Initials",names)
	# last.index <- grep("LastName",names)
	# lengths <- c(length(first.index),length(initial.index),length(last.index))
	# maxlength <- max(lengths)
	
	# if(any(lengths!=maxlength)){
		# max.index <- which(lengths==maxlength)[1]
		# if(max.index==1){
			# initial.index <- first.index+1
			# last.index <- first.index+2 
		# }
		# else if(max.index==2){
			# first.index <- initial.index-1
			# last.index <- initial.index+1
		# }
		# else{
			# initial.index <- last.index-1
			# first.index <- last.index-2
		# }
	# }
	
	# if(all(lengths==0)){ # NO AUTHORS LISTED
	
	# df <- data.frame(
		# LastName = NA,
		# ForeName = NA,
		# Initials = NA
	# )

	# df$order <- NA

	
	# }
	# else{
	
	# df <- data.frame(
		# LastName = as.character(object[last.index]),
		# ForeName = as.character(object[first.index]),
		# Initials = as.character(object[initial.index]),
		# stringsAsFactors=FALSE)

	# df$order <- 1:nrow(df)
	
	# }
# df
# }

GetMeshMajor <- function(object){

  names <- names(object)

  if(any(names=="DescriptorName")){

    index <- which(names=="DescriptorName")
    index <- min(index):max(index)
    data.frame(
               Heading = object[index],
               Type = ifelse(names[index]=="DescriptorName","Descriptor","Qualifier")
               )
  }
  else
    NA
}


setClass("Medline",
	representation(
			Query = "character",
			PMID = "character",
			Year = "numeric",
                        Month = "numeric",
                        Day= "numeric",
                        Author = "list",
                        ISSN= "character",
                        Title = "character",
                        ArticleTitle= "character",
			ELocationID= "character",
			AbstractText= "character",
			Affiliation= "character",
			Language= "character",
			PublicationType= "character",
			MedlineTA= "character",
			NlmUniqueID= "character",
			ISSNLinking= "character",
			Hour= "numeric",
			Minute= "numeric",
			PublicationStatus= "character",
			ArticleId= "character",
			Volume= "character",
			Issue= "character",
			ISOAbbreviation= "character",
			MedlinePgn= "character",
			CopyrightInformation= "character",
			Country= "character",
			GrantID= "character",
			Acronym= "character",
			Agency= "character",
			RegistryNumber= "character",
			RefSource= "character",
			CollectiveName="character",
                       	Mesh="list")
)

Medline <- function(object, query = character(0)){
        
	# ARTICLE LIST FROM PUBMED QUERY
	PMID <- sapply(object, function(x) x["PMID"],USE.NAMES=FALSE)
	Year <- sapply(object, function(x) x["Year"],USE.NAMES=FALSE)	
	Month <- sapply(object, function(x) x["Month"],USE.NAMES=FALSE)
	Day <- sapply(object, function(x) x["Day"],USE.NAMES=FALSE)
	ISSN <- sapply(object, function(x) x["ISSN"],USE.NAMES=FALSE)
	Title <- sapply(object, function(x) x["Title"],USE.NAMES=FALSE)
	ArticleTitle <- sapply(object, function(x) x["ArticleTitle"],USE.NAMES=FALSE)
	ELocationID <- sapply(object, function(x) x["ELocationID"],USE.NAMES=FALSE)
	AbstractText <- sapply(object, function(x) x["AbstractText"],USE.NAMES=FALSE)
	Affiliation <- sapply(object, function(x) x["Affiliation"],USE.NAMES=FALSE)
	Language <- sapply(object, function(x) x["Language"],USE.NAMES=FALSE)
	PublicationType <- sapply(object, function(x) x["PublicationType"],USE.NAMES=FALSE)
	MedlineTA <- sapply(object, function(x) x["MedlineTA"],USE.NAMES=FALSE)
	NlmUniqueID <- sapply(object, function(x) x["NlmUniqueID"],USE.NAMES=FALSE)
	ISSNLinking <- sapply(object, function(x) x["ISSNLinking"],USE.NAMES=FALSE)
	Hour <- sapply(object, function(x) x["Hour"],USE.NAMES=FALSE)
	Minute <- sapply(object, function(x) x["Minute"],USE.NAMES=FALSE)
	PublicationStatus <- sapply(object, function(x) x["PublicationStatus"],USE.NAMES=FALSE)
	ArticleId <- sapply(object, function(x) x["ArticleId"],USE.NAMES=FALSE)
	Volume <- sapply(object, function(x) x["Volume"],USE.NAMES=FALSE)
	Issue <- sapply(object, function(x) x["Issue"],USE.NAMES=FALSE)
	ISOAbbreviation <- sapply(object, function(x) x["ISOAbbreviation"],USE.NAMES=FALSE)
	MedlinePgn <- sapply(object, function(x) x["MedlinePgn"],USE.NAMES=FALSE)
	CopyrightInformation <- sapply(object, function(x) x["CopyrightInformation"],USE.NAMES=FALSE)
	Country <- sapply(object, function(x) x["Country"],USE.NAMES=FALSE)
	GrantID <- sapply(object, function(x) x["GrantID"],USE.NAMES=FALSE)
	Acronym <- sapply(object, function(x) x["Acronym"],USE.NAMES=FALSE)
	Agency <- sapply(object, function(x) x["Agency"],USE.NAMES=FALSE)
	RegistryNumber <- sapply(object, function(x) x["RegistryNumber"],USE.NAMES=FALSE)
	RefSource <- sapply(object, function(x) x["RefSource"],USE.NAMES=FALSE)
	CollectiveName <- sapply(object, function(x) x["CollectiveName"],USE.NAMES=FALSE)

        Mesh <- lapply(object, GetMeshMajor)     
	Author <- lapply(object,GetAuthors)
	
	PMID <- as.character(PMID)
	Year <- as.numeric(Year)
	Month <- as.numeric(Month)
	Day <- as.numeric(Day)
	ISSN <- as.character(ISSN)
	Title <- as.character(Title)
	ArticleTitle <- as.character(ArticleTitle)
	ELocationID <- as.character(ELocationID)
	AbstractText <- as.character(AbstractText)
	Affiliation <- as.character(Affiliation)
	Language <- as.character(Language)
	PublicationType <- as.character(PublicationType)
	MedlineTA <- as.character(MedlineTA)
	NlmUniqueID <- as.character(NlmUniqueID)
	ISSNLinking <- as.character(ISSNLinking)
	Hour <- as.numeric(Hour)
	Minute <- as.numeric(Minute)
	PublicationStatus <- as.character(PublicationStatus)
	ArticleId <- as.character(ArticleId)
	Volume <- as.character(Volume)
	Issue <- as.character(Issue)
	ISOAbbreviation <- as.character(ISOAbbreviation)
	MedlinePgn <- as.character(MedlinePgn)
	CopyrightInformation <- as.character(CopyrightInformation)
	Country <- as.character(Country)
	GrantID <- as.character(GrantID)
	Acronym <- as.character(Acronym)
	Agency <- as.character(Agency)
	RegistryNumber <- as.character(RegistryNumber)
	RefSource <- as.character(RefSource)
	CollectiveName <- as.character(CollectiveName)
  	
	new("Medline",
			Query = query,
			PMID = PMID,
			Year = Year, 
		    Month = Month , 
		    Day  = Day, 
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
			Hour = Hour, 
			Minute = Minute, 
			PublicationStatus = PublicationStatus, 
			ArticleId = ArticleId, 
			Volume = Volume, 
			Issue = Issue, 
			ISOAbbreviation = ISOAbbreviation, 
			MedlinePgn = MedlinePgn, 
			CopyrightInformation = CopyrightInformation, 
			Country = Country, 
			GrantID = GrantID, 
			Acronym = Acronym, 
			Agency = Agency, 
			RegistryNumber = RegistryNumber, 
			RefSource = RefSource, 
			CollectiveName = CollectiveName,
                        Mesh = Mesh
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
setMethod("Year","Medline",function(object) object@Year)                                
setMethod("Month","Medline",function(object) object@Month)                              
setMethod("Day","Medline",function(object) object@Day)                                  
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
setMethod("Hour","Medline",function(object) object@Hour)                                
setMethod("Minute","Medline",function(object) object@Minute)                            
setMethod("PublicationStatus","Medline",function(object) object@PublicationStatus)      
setMethod("ArticleId","Medline",function(object) object@ArticleId)                      
setMethod("Volume","Medline",function(object) object@Volume)                            
setMethod("Issue","Medline",function(object) object@Issue)                              
setMethod("ISOAbbreviation","Medline",function(object) object@ISOAbbreviation)          
setMethod("MedlinePgn","Medline",function(object) object@MedlinePgn)                    
setMethod("CopyrightInformation","Medline",function(object) object@CopyrightInformation)
setMethod("Country","Medline",function(object) object@Country)                          
setMethod("GrantID","Medline",function(object) object@GrantID)                          
setMethod("Acronym","Medline",function(object) object@Acronym)                          
setMethod("Agency","Medline",function(object) object@Agency)                            
setMethod("RegistryNumber","Medline",function(object) object@RegistryNumber)            
setMethod("RefSource","Medline",function(object) object@RefSource)                      
setMethod("CollectiveName","Medline",function(object) object@CollectiveName)            
setMethod("Mesh","Medline",function(object) object@Mesh)

