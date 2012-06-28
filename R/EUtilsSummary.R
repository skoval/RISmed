setClass("EUtilsSummary",
	representation(
		count = "numeric",
		retmax = "numeric",
		retstart = "numeric",
		id = "character",
		querytranslation = "character"
	)
)

EUtilsSummary <- function(query,type="esearch",db="pubmed",url=NULL,encoding="unknown",...){

	if(is.null(url)){
		url <- EUtilsQuery(query,type,db,...)
	}

	res <- ParseTags(readLines(url,warn=FALSE,encoding=encoding))
	if(res$Count==0) res$Id <- character(0)
	
	new("EUtilsSummary",
		count = res$Count,
		retstart = res$RetStart,
		retmax = res$RetMax,
		id = res$Id,
		querytranslation = res$QueryTranslation
		)
}

setMethod("print","EUtilsSummary",function(x,...) print(x@querytranslation))
setMethod("show","EUtilsSummary",function(object) print(object@querytranslation))
setMethod("summary","EUtilsSummary",function(object,...){

			cat("Query:\n")
			cat(object@querytranslation,"\n\n")
			cat("Result count: ",object@count)
			
			invisible(object@id)
			
})

# GENERICS
setMethod("QueryCount","EUtilsSummary",function(object) object@count)
setMethod("QueryId","EUtilsSummary",function(object) object@id)
setMethod("QueryTranslation","EUtilsSummary",function(object) object@querytranslation)
