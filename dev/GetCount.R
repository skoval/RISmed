library(RISmed)
library(plyr)
library(ggplot2)

# Using EUtilsSummary to write a function to get yearly counts for a PubMed query
GetCount <- function(start, query, datetype = "dp"){
	Fetch <- EUtilsSummary(query,retmax=1,mindate=start,maxdate=(start+1),datetype=datetype)
QueryCount(Fetch)
}

# Create data frame for publication trends for CBT
years <- 1970:2010
result <- ldply(years,.fun=GetCount,query="cognitive behavior therapy[ti]")
names(result) <- "count"
result$year <- years

fig <- ggplot(data=result, aes(y=count,x=year))+geom_area(fill=cm.colors(1))+geom_line()
fig+scale_y_continuous("total medline publications")+scale_x_continuous("")

# Downloading Medline content for CBT to identify top 10 authors in field
cbt <- EUtilsSummary("cognitive behavior therapy[ti]", maxdate = 2011)
fetch <- EUtilsGet(cbt)
CBTAuthors <- Author(fetch)
CBTAuthors <- unlist(sapply(CBTAuthors, function(x) x$LastName))
topAuthors <- sort(table(CBTAuthors), dec = TRUE)[1:30]


qplot(x=names(topAuthors), xend= names(topAuthors), y = 0, yend = topAuthors, geom = "segment",
				ylab = "publication count",
				xlab = "", lwd = 2, 
				las = 2)+opts(legend.position="none",axis.text.x=theme_text(angle=90))

# JOURNALS PUBLISHING THE MOST ARTICLES ON cbt
journals <- MedlineTA(fetch)
top10 <- sort(table(journals), dec = TRUE)[1:10]
top10
