### Depends on 01-DownloadPublicationsCitations.R

### IMPORT LIBRARIES
library(tidyverse)
library(dplyr)
library(jsonlite)
library(stringr)

#load each JSON from the three CitedBy raw data directories
# extract original search term (search-results/opensearch:Query/@searchTerms -> look for refsrctitle())
# extract all the prism:url elements for each Duke article citing the journal
#   extract all URLs first and save with original journal title search term, tibble/csv

### CONVERT CITED BY DUKE TO DATAFRAME
# create list of all json files
filepath = c("DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDuke","RawData/ScopusAPIJsonsCitedbyDukeOA","RawData/ScopusAPIJsonsCitedbyDukeAmp")
filelist = list.files(path = filepath, pattern = ".json", full.names = TRUE)
#filelist = list.files(path = filepath, pattern = "60113147-Zeitschrift%20fur%20Medizinische%20Physik-1.json", full.names = TRUE)

# initialize dataframe for Duke articles that might be citing target journal
articles_df = tibble(path = character(),
                     journal_name = character(),
                     prism_url = character())

# Took about an hour to run
for (file in filelist) {
  json_data = fromJSON(file)
  
  # find number of results on the page and continue if results > 0
  results = as.numeric(json_data$`search-results`$`opensearch:totalResults`)
  
  if (results > 0) {
    # extract affiliation id and journal name from search string
    search_string = json_data$`search-results`$`opensearch:Query`$`@searchTerms`
    name = as.character(as.data.frame(str_match_all(search_string, "(refsrctitle\\()(.+)(\\\"\\))")[[1]])$V3) %>% str_replace_all('\"','')
    
    article_current_df = tibble(path = str_replace(file,'DataProcessingScripts/RawData/',''),
                                journal_name = name, 
                                prism_url = json_data$`search-results`$entry$`prism:url` %>% as.character())
    
  # bind per json list to the overall cited df
    articles_df = rbind(articles_df, article_current_df)
  }
}

write_csv(articles_df, "DataProcessingScripts/ProcessedData/cited_by_article_urls.csv")
