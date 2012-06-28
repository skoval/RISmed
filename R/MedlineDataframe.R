MedlineDataframe <- function(object){

	data.frame(
	PMID = object@PMID,
	Year = object@Year,
	Month = object@Month,
	Day = object@Day,
	Title = object@Title,
	ArticleTitle = object@ArticleTitle,
	Affiliation = object@Affiliation,
	PublicationType = object@PublicationType,
	PublicationStatus = object@PublicationStatus,
	Volume = object@Volume,
	Issue = object@Issue,
	ISOAbbreviation = object@ISOAbbreviation,
	Country = object@Country,
	stringsAsFactors = FALSE
		)
}