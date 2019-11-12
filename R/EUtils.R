# MAKING RNCBI PACKAGE
collapse <- function(...){paste(...,sep="",collapse="")}

EUtilsURL <- function(type="esearch",db="pubmed"){
	
	# CONSTRUCT ANY SERVICE TYPE AND DATABASE
	url <- 	"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/type.fcgi?db=DB&"
	
sub("(.*)(type)(.*)(DB)(.*)",collapse("\\1",type,"\\3",db,"\\5"),url)
}


EUtilsQuery <- function(query,type="esearch",db="pubmed",...){

	# CREATE URL WITH REQUEST
	PubMedURL <- EUtilsURL(type,db)
	Query <- gsub(" ","+",query)

	# PARTIAL MATCH OF POSSIBLE LIMITS
	OPTIONS <- c(
		"retstart",
		"retmax",
		"rettype",
		"field",
		"datetype",
		"reldate",
		"mindate",
		"maxdate"
	)
	
	ArgList <- list(...) # NEED FUNCTION PARTIAL MATCH HERE
	
	if(length(ArgList)==0){
		ArgList$retmax <- 1000 # DEFAULT 1000 RECORDS RETURNED
	}
	else{
		WhichArgs <- pmatch(names(ArgList),OPTIONS)	
		if(any(is.na(WhichArgs))||sapply(WhichArgs,length)>1)
			stop("Error in specified limits.")
		names(ArgList) <- OPTIONS[WhichArgs]
		if(all(names(ArgList)!="retmax"))
			ArgList$retmax <- 1000 # DEFAULT 1000 RECORDS RETURNED
	}
	
	ArgList$tool <- "RISmed"
	ArgList$email <- "s.a.kovalchik@gmail.com"
	
	# REPLACE RETMAX IF NOT USED
	ArgStr <- paste(names(ArgList),unlist(ArgList),sep="=")
	ArgStr <- paste(ArgStr,collapse="&")

paste(PubMedURL,"term=",Query,"&",ArgStr,sep="",collapse="")	
}

ParseTags <- function(lines){

	StripLines <- sapply(lines, function(x)gsub("> +",">",x)) # REPLACE WHITE SPACE
	
	Fields <- c("Count",
				"RetMax",
				"RetStart",
				"Id",
				"QueryTranslation")
			
	Patterns <- paste("(.*<",Fields,">)(.*)(<\\/",Fields,">).*",sep="")

	FieldIndex <- lapply(Patterns, function(pattern) grep(pattern, StripLines))
	FieldIndex[[1]] <- FieldIndex[[1]][1] # TAKE COUNT OF COMBINED QUERY

	Values <- lapply(1:length(Fields),
					function(i){
						result <- sapply(StripLines[FieldIndex[[i]]],
										function(x) sub(Patterns[i],"\\2",x),USE.NAMES=FALSE)
as.vector(result)
})


names(Values) <- Fields
Values$Count <- as.numeric(Values$Count)
Values$RetMax <- as.numeric(Values$RetMax)
Values$RetStart <- as.numeric(Values$RetStart)

Values
}


SplitIDs <- function(ids){

	if(length(ids)>200){
		group <- rep(1:ceiling(length(ids)/200),each=200)
		group <- group[1:length(ids)]
		split(ids,group)
	}
	else{
		list(ids)
	}

}


EUtilsGet <- function(x, type="efetch", db="pubmed"){

	if(class(x)[1]=="EUtilsSummary"){
		query <- x@querytranslation
		x <- x@PMID
	}
	else{
		query <- ""
	}
	
	IDList <- SplitIDs(x)
	Result <- lapply(IDList, EUtilsSubGet, type = type, db = db)
	Result <- unlist(Result, recursive=FALSE)

	if(type=="efetch"&db=="pubmed"){
		
		Result <- Medline(Result, query)
	}
			
Result
}


EUtilsSubGet <- function(ids, type="efetch", db="pubmed"){

	FetchURL <- EUtilsURL(type,db=db)
	IDStr <- collapse("id=",paste(ids,collapse=","))
	EUtilsFetch <- collapse(FetchURL,IDStr)	
	
	res <- readLines(collapse(EUtilsFetch,"&retmode=xml"), warn = FALSE, encoding = "UTF-8")	
	
	index <- grep("Article.*doi", res)
	
	if(length(index) != 0)
		res[index] <- sub("ArticleId", "DOI", res[index])
	
	# Date replacements
	index <- grep("<DateRevised>", res)
	
	if(length(index) != 0){
		res[index + 1] <- sub("<Year>", "<YearRevised>", res[index + 1])
		res[index + 2] <- sub("<Month>", "<MonthRevised>", res[index + 2])
		res[index + 3] <- sub("<Day>", "<DayRevised>", res[index + 3])
	}
		
	index <- grep("<PubDate>", res)
	
	if(length(index) != 0){
		res[index + 1] <- sub("<Year>", "<YearPubDate>", res[index + 1])
		res[index + 2] <- sub("<Month>", "<MonthPubDate>", res[index + 2])
		res[index + 3] <- sub("<Day>", "<DayPubDate>", res[index + 3])
		
	}
	
	index <- grep("<ArticleDate", res)
	
	if(length(index) != 0){
		res[index + 1] <- sub("<Year>", "<YearArticleDate>", res[index + 1])
		res[index + 2] <- sub("<Month>", "<MonthArticleDate>", res[index + 2])
		res[index + 3] <- sub("<Day>", "<DayArticleDate>", res[index + 3])
	}
		
			
	pubstatus <- c(
		"received", 
		"accepted",
		"epublish",
		"ppublish",
		"pmc",
		"pubmed",
		"entrez",
		"medline"
	)
	
	if(db=="pubmed"){
	
	ArticleList <- mapply(GroupArticle, start = ArticleStart(res),
                              end = ArticleEnd(res),
                              MoreArgs = list(.obj = res), 
                              SIMPLIFY = FALSE)

										
	ParseEUtilsFetch <- lapply(ArticleList, function(x){
		for(i in pubstatus){
			if(any(grepl("PubStatus", x) & grepl(i, x))){
				index <- which(grepl("PubStatus", x) & grepl(i, x))
				x[index + 1:5] <- gsub("(Year|Month|Day|Hour|Minute)",paste(c("\\1",First_Upper(i)),collapse = ""), x[index + 1:5])
			}
		}	
		
		x[grep("AbstractText", x)] <- gsub("<[a-z]+>", "", x[grep("AbstractText", x)])
		x[grep("AbstractText", x)] <- gsub("</[a-z]+>", "", x[grep("AbstractText", x)])
		

		if(any(grepl("AbstractText", x) & grepl("Label", x))){
			index <- grep("<AbstractText.*Label", x)
			x[index] <- sub("</AbstractText>", "", x[index])
			x[index] <- sub("(<AbstractText +)(.*)(>)", "\\2:", x[index])
			x[index[1]] <- paste("<AbstractText>", paste(x[index], collapse = " "), "</AbstractText>", collapse = " ")
		}
		
		lines <- LinesWithValues(x)
		full <- GetFullFields(x[lines])		
		exclusions <- grepl("ELocation.*pii", full)
		val <- GetValues(x[lines[!exclusions]])
		names(val) <- GetFields(x[lines[!exclusions]])
	val
	})
	
	}
	else{
		lines <- LinesWithValues(res)
		full <- GetFullFields(res[lines])		
		exclusions <- grepl("ELocation.*pii", full)
		tags <- GetFields(res[lines[!exclusions]])
		ParseEUtilsFetch <- GetValues(res[lines[!exclusions]])
		names(ParseEUtilsFetch) <- tags
	}
	
ParseEUtilsFetch
}

First_Upper <- function(x) paste(toupper(substr(x,1,1)),substr(x,2,nchar(x)),collapse = "", sep = "")

LinesWithValues <- function(.obj){
	grep(">(\\[?[a-zA-Z]|[0-9]).*<",.obj)
}

GetFields <- function(.obj){
	sub("(.*<)([a-zA-Z]+)(.*>)(\\[?([a-zA-Z]|[0-9]).*<..*>.*)","\\2",.obj)
}

#GetValues <- function(.obj){
#	sub("(.*<)([a-zA-Z]+)(.*>)(\\[?([a-zA-Z]|[0-9]).*)(<..*>.*)","\\4",.obj)
#}

LinesWithValues <- function(.obj){
	grep(">.*<",.obj)
}


GetFullFields <- function(.obj){
	sub("(.*<)([a-zA-Z].*)(>)(.*)(<..*>.*)","\\2",.obj)
}

GetFields <- function(.obj){
	sub("(.*<)([a-zA-Z]+)(.*>)(.*)(<..*>.*)","\\2",.obj)
}

GetValues <- function(.obj){
	sub("(.*<)([a-zA-Z]+)(.*>)(.*)(<..*>.*)","\\4",.obj)
}

ArticleStart <- function(.obj) which(.obj=="<PubmedArticle>")
ArticleEnd <- function(.obj) which(.obj=="</PubmedArticle>")
GroupArticle <- function(start, end, .obj) .obj[start:end] 








