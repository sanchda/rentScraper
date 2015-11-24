library(rvest)
library(httr)
library(ggplot2)
library(data.table)

## Description
# Scrapes the rental property listings from a notable company.  First aggregates a list of properties, then iterate through the resulting links.
# TODO: fix overall execution pattern.

## Functions
source("helperFunctions.r")


## Define the base query.  Subsequent queries will append the page number to it.
# Obfuscated to external file to protect the innocent
fileName <- 'query.txt'
queryRoot = readChar(fileName, file.info(fileName)$size)

fileName <- 'pageBase.txt'
pageBase = readChar(fileName, file.info(fileName)$size)

maxHits   = 500

# set rcurl config
cookieFile = "cookies.txt"
agent = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.80 Safari/537.36";
thisConfig = set_config( config(cookiejar=cookieFile, cookiefile=cookieFile, useragent=agent, followlocation = TRUE) )

# Run the query up until maxHits listings.
linkList = listingScraper( queryRoot, thisConfig, pageBase )
for( i in 2:floor(maxHits/9) ) {
  Sys.sleep(4)
  pageQuery = sprintf("%s/page:%d", queryRoot, i)
  newLinks = listingScraper( pageQuery, thisConfig, pageBase )
  
  if( any(grep("login_signup", newLinks)) ) {
    cat(sprintf("Break: login nag.\n"))
    break;
  } else {
    linkList = unique( c( linkList, newLinks ) )
  }
  
  cat(sprintf("Processed %f percent of links.\n", 100*(i*9)/maxHits))
  
}


scrapedContent = data.frame(price=numeric(0), ownership=character(0), neighborhood=character(0), size=character(0));
i=0
for( thisLink in linkList ) {
  i = i+1;
  cat(sprintf("Processed %f percent of links.\n", 100*i/length(linkList)))
  
  Sys.sleep(0)
  thisId = gsub(".*/(.*)","\\1",linkList[1]);
  scrapedContent = rbind(scrapedContent, pageScraper( thisLink, thisId, thisConfig ))
}

# Process bedroom number
scrapedContent = as.data.table( scrapedContent );
scrapedContent[ , size := gsub("\\sbedroom","", size, ignore.case=TRUE) ];
scrapedContent[ grep("one", size, ignore.case=TRUE),   size := "1" ]
scrapedContent[ grep("two", size, ignore.case=TRUE),   size := "2" ]
scrapedContent[ grep("three", size, ignore.case=TRUE), size := "3" ]
scrapedContent[ grep("four", size, ignore.case=TRUE),  size := "4" ]
scrapedContent[ grep("five", size, ignore.case=TRUE),  size := "5" ]


# TODO: oh god, this is horrible--why did I write this?
## Process and display the content
# Note the painful acrobatics to reorder the factor levels of neighborhoods according to the mean price in that neighborhood.
# I think I know how to make this nicer, but I didn't have time to write short code.
SC = as.data.table( scrapedContent[ size %in% c("1")] );
mp = SC[,.(price.mean = mean(price)),by=neighborhood]
mp[, neighborhood := as.character(neighborhood) ]
setkey(SC, "neighborhood")
setkey(mp, "neighborhood")
setorder(mp, neighborhood)

# Get into alphabetical order
meanPrice = merge(SC, mp)
meanPrice[, neighborhood := as.character(neighborhood) ]
setkey(meanPrice, "neighborhood")
setorder(meanPrice, neighborhood)

# And now reorder the factor variable
factorIndex = sort( mp$price.mean, index.return=TRUE)$ix;
meanPrice$neighborhood = factor( meanPrice$neighborhood, levels(factor(meanPrice$neighborhood))[ factorIndex ], ordered=TRUE)

# And plot the data!
plotWhiskerLabels(meanPrice, "neighborhood", "price", 1)
