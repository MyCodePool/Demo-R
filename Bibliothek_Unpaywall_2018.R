library(tidyverse)
library(foreach)
library(doParallel)
library(readr)
cl <- makeCluster(5, outfile = "") #determines how many parallel processes are used for the pdf downloading
registerDoParallel(cl)

email <- "test@tester.de"


#---------------------------------------------------------------------------------------------------------
# functions to interact with unpaywall API and calculate article colors
#---------------------------------------------------------------------------------------------------------

#converts normal DOIs to oaDOIs, adding email address
to_oaDOI <- function(dois, email)
{
  oaDOIs <- paste0("https://api.unpaywall.org/v2/", dois, "?email=", email)
  return(oaDOIs)
}


#returns article color (gold, green, bronze, hybrid, closed) for given oaDOI
get_article_color <- function(oaDOI)
{
  article_color <- ""
  article_info <- c("", "", "")
  tryCatch({
    oaDOI_result <- readLines(oaDOI, warn = FALSE)
    oaDOI_result <- jsonlite::fromJSON(oaDOI_result)

    is_oa <- oaDOI_result$is_oa
    journal_is_oa <- oaDOI_result$journal_is_oa
    journal_name <- oaDOI_result$journal_name
    publisher <- oaDOI_result$publisher

    #loop over all OA-locations and calculate the OA color for each
    oa_colors <- vector()
    if(length(oaDOI_result$oa_locations) == 0) {
      oa_colors <- "closed"
    } else {
      for(i in 1:dim(oaDOI_result$oa_locations)[1])
      {
        loc_article_color <- get_loc_article_color(oaDOI_result$oa_locations[i,], is_oa, journal_is_oa)
        oa_colors <- append(oa_colors, loc_article_color)
      }
    }

    if("gold" %in% oa_colors) {
      article_color <- "gold"
    } else if("hybrid" %in% oa_colors) {
      article_color <- "hybrid"
    } else if("green" %in% oa_colors) {
      article_color <- "green"
    } else if("bronze" %in% oa_colors) {
      article_color <- "bronze"
    } else {
      article_color <- "closed"
    }

    article_info <- c(article_color, journal_name, publisher)

  }, error=function(e){
    print("Could not access oaDOI")
  })

  return(article_info)
}


#decision on OA color from metadata
get_loc_article_color <- function(oa_location, is_oa, journal_is_oa)
{
  host_type <- oa_location$host_type
  article_license <- oa_location$license

  if(is.null(article_license) | is.na(article_license)) {
    article_license <- ""
  }
  if(is.null(oa_location)) {
    article_color = "closed"
  } else if(is_oa == FALSE) {
    article_color <- "closed"
  } else if(is_oa == TRUE && host_type == "repository") {
    article_color <- "green"
  } else if(is_oa == TRUE && host_type == "publisher" && journal_is_oa == TRUE) {
    article_color <- "gold"
  } else if(is_oa == TRUE && host_type == "publisher" &&
            journal_is_oa == FALSE && (stringr::str_sub(article_license, 1, 3) == "cc-")) {
    article_color <- "hybrid"
  } else if(is_oa == TRUE) { #} && host_type == "publisher" && journal_is_oa == FALSE) {
    article_color <- "bronze"
  }

  return(article_color)
}


#---------------------------------------------------------------------------------------------------------
# retrieve unpaywall OA colours for example dataset
#---------------------------------------------------------------------------------------------------------

publications <- read_delim("2018_Charite_WoS_Embase.csv", delim = ";")

dois <- publications$DOI
oa_dois <- to_oaDOI(dois, email)

#get all the OA colors from unpaywall
OA_colors <- foreach(i=1:length(oa_dois), .export = c("get_article_color")) %dopar% {
  get_article_color(oa_dois[i])
}

write_excel_csv(OA_out_merged_table, "OA_colors_Charite_2018_merged.txt")



