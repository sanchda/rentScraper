library(rvest)
library(ggplot2)

# Scrape the results page, returning links
listingScraper = function( thisLink, thisConfig, pageBase ) {
  queryListings = html_session(thisLink, thisConfig)
  
  linkedUnits = queryListings %>%
    html_nodes("li") %>%
    html_nodes("div") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  newLinks = unlist(sprintf("%s%s",pageBase,linkedUnits))
  
  return( newLinks )
}

# Scrape content from the page
pageScraper = function( thisLink, thisId, thisConfig ) {
  thisPage = html_session(thisLink, thisConfig);
  
  infoBoxValues = thisPage %>%
    html_nodes(".information-box") %>%
    html_nodes("dd") %>%
    html_text();
  
  infoBoxValues = gsub("\\r", "", infoBoxValues);
  infoBoxValues = gsub("\\t", "", infoBoxValues);
  infoBoxValues = gsub("\\n", "", infoBoxValues);
  
  infoBoxLabels = thisPage %>%
    html_nodes(".information-box") %>%
    html_nodes("dt") %>%
    html_text();
  
  # We basically have key-values.  Parse them to return a template format
  # Price
  idx = grep("price", infoBoxLabels , ignore.case=TRUE);
  if( length( idx ) ) {
    priceVal =  infoBoxValues[ idx[[1]] ]
  } else {
    priceVal = NA
  }
  
  # Ownership
  idx = grep("ownership", infoBoxLabels , ignore.case=TRUE);
  if( length( idx ) ) {
    ownershipVal =  infoBoxValues[ idx[[1]] ]
  } else {
    ownershipVal = NA
  }
  
  # Neighborhood
  idx = grep("neighborhood", infoBoxLabels , ignore.case=TRUE);
  if( length( idx ) ) {
    neighborhoodVal =  infoBoxValues[ idx[[1]] ]
  } else {
    neighborhoodVal = NA
  }
  
  # Size
  idx = grep("size", infoBoxLabels , ignore.case=TRUE);
  if( length( idx ) ) {
    sizeVal =  infoBoxValues[ idx[[1]] ]
  } else {
    sizeVal = NA
  }
  
  # Define and pass the output value
  outputVal = data.frame(ID=thisId,
                         price=as.numeric(gsub("[\\$,]", "", priceVal)),
                         ownership=ownershipVal,
                         neighborhood=neighborhoodVal,
                         size=sizeVal);
  
  return( outputVal )
}

# Plot something or other
plotWhiskerLabels = function(thisData, catVar, numVar, boxWidth=1) {
  ggplot(thisData, aes_string(x=catVar, y=numVar)) +
    geom_boxplot(width = boxWidth, outlier.size=0, alpha=0.2) +
    #    coord_flip() +
    geom_point(colour="lightblue", alpha=0.9, position="jitter") +
    theme(panel.grid.major = element_line(colour="gray"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) +
    theme(plot.title = element_text(size=rel(2), face="bold")) +
    theme(axis.title = element_text(size=rel(0), face="bold")) +
    theme(axis.text  = element_text(size=rel(1), face="bold", colour="black")) +
    theme(axis.ticks = element_line(size=rel(1.2), colour = "black")) +
    theme(axis.title.y = element_text(vjust=2)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.title.x = element_text(vjust=-0.5)) +
    theme(legend.position="none")
}