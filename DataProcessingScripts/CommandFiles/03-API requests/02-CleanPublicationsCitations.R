### Script depends on 01-DownloadPublicationsCitations.R

### IMPORT LIBRARIES
library(tidyverse)
library(jsonlite)
library(stringr)

### CONVERT AUTHORED BY DUKE TO DATAFRAME
# create list of all json files
filepath = "DataProcessingScripts/RawData/ScopusAPIJsons"
filelist = list.files(path = filepath, pattern = ".json", full.names = TRUE)

# initialize dataframe for papers authored by Duke
authored_df <- tibble(year = character(),
                      P.ISSN = character(),
                      E.ISSN = character(),
                      journal_name = character(),
                      open_access = numeric())

for (file in filelist) {
  json_data = fromJSON(file)
  
  # find number of results on the page and continue if results > 0
  results = as.numeric(json_data$`search-results`$`opensearch:totalResults`)
  
  if (results > 0) {
    # extract year from search string
    search_string <- json_data$`search-results`$`opensearch:Query`$`@searchTerms`
    year <- word(search_string, -1)
    
    entry <- json_data$`search-results`$entry
    
    # iterate through results one by one to make sure elements are properly matched
    for (result in 1:nrow(entry)) {
      
      record <- list(
        year = year,
        P.ISSN = as.character(entry$`prism:issn`[result]),
        E.ISSN = ifelse(typeof(entry$`prism:eIssn`[result]) == "NULL", "NULL", as.character(entry$`prism:eIssn`[result])),
        journal_name = as.character(entry$`prism:publicationName`[result]),
        open_access = ifelse(entry$openaccess[result] == "NULL", NA, as.numeric(entry$openaccess[result]))
      )
      authored_df <- bind_rows(authored_df, record)
      
    }
  }
}

write_csv(authored_df, "DataProcessingScripts/ProcessedData/authored_by_duke_journals.csv")

### CONVERT CITED BY DUKE TO DATAFRAME
# create list of all json files
filepath = "DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDuke" # "DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDukeOA" # "DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDukeAmp"
filelist = list.files(path = filepath, pattern = ".json", full.names = TRUE)

# initialize dataframe for journals cited by Duke
cited_df = data.frame(year_duke = numeric(),
                      af_id = character(),
                      journal_name = character())

for (file in filelist) {
  json_data = fromJSON(file)

  # find number of results on the page and continue if results > 0
  results = as.numeric(json_data$`search-results`$`opensearch:totalResults`)

  if (results > 0) {
    # extract affiliation id and journal name from search string
    search_string = json_data$`search-results`$`opensearch:Query`$`@searchTerms`
    paran = str_extract_all(search_string, "\\([^()]+\\)")[[1]]
    terms = substring(paran, 2, nchar(paran)-1)
    id = terms[1]
    name = substring(substring(terms[2],2), 1, str_length(substring(terms[2],2))-1)
    
    # create a list containing cited by information per json
    cited_current_df = cbind(year_duke = as.numeric(unlist(substring(json_data$`search-results`$entry$`prism:coverDate`, 1, 4))),
                             af_id = id,
                             journal_name = name)
   
    # bind per json list to the overall cited df
    cited_df = rbind(cited_df, cited_current_df)
  }
}

# filter out years not within 2012 to 2018
cited_df = data.frame(as.list(cited_df)) %>%
  mutate(year_duke = as.character(year_duke)) %>%
  mutate(year_duke = as.numeric(year_duke)) %>%
  filter(year_duke >= 2012 & year_duke <= 2018)

write.csv(cited_df, "DataProcessingScripts/ProcessedData/cited_by_duke_journals.csv") # write.csv(cited_df, "DataProcessingScripts/ProcessedData/cited_by_duke_OA.csv") # write.csv(cited_df, "DataProcessingScripts/ProcessedData/cited_by_duke_amp.csv")
