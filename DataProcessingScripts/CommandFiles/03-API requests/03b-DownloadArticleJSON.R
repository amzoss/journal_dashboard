### Depends on 03a-ReparseCitedByForArticleURLs.R

### CREATE DIRECTORIES
#dir.create("DataProcessingScripts/RawData/ScopusAPIArticles/", showWarnings = TRUE, recursive = TRUE)

### IMPORT LIBRARIES
library(tidyverse)
library(dplyr)
library(jsonlite)
library(stringr)

# set values for constant url parameters
apiKey_string = "?apiKey=ADD_API_KEY"
httpAccept_string = "&httpAccept=application/json"

#add API key and httpaccept string to each URLs
#make API call to get article metadata
articles_df <- read_csv("DataProcessingScripts/ProcessedData/cited_by_article_urls.csv")

article_path <- "DataProcessingScripts/RawData/ScopusAPIArticles/"

# want to dedup URLs before running calls
all_urls <- articles_df %>% dplyr::select(prism_url) %>% distinct() %>% unlist()

#rm(articles_df)

# need to remove url that is just https://api.elsevier.com/content/abstract/scopus_id/
min_length <- str_length("https://api.elsevier.com/content/abstract/scopus_id/") + 1
all_urls <- all_urls[sapply(all_urls, function (x) {str_length(x) > min_length })]

# API has 10k per week limit, even for subscribers
batch_urls <- split(all_urls, ceiling(seq_along(all_urls)/10000))

batch_call_try <- function(sample, apikey) {
  for (url in sample) {
    scopus_id = url %>% str_replace('https://api.elsevier.com/content/abstract/scopus_id/','')
    call = paste0(url, apikey, httpAccept_string)
    
    tryCatch({
      json = read_json(call, simplifyVector = FALSE)
      write_json(json, paste0(article_path, scopus_id, ".json"))
    }, error = function(e) {
      print("error")
      print(call)
    }, warning = function(w) {
      print("warning")
      print(call)
    }
    
    )
  }
}

# unless you have multiple APIs, might need to run one sample per week  
# 10k can take 3 hours
for (i in 1:length(batch)) {
  batch_call_try(batch[[i]], apiKey_string)
}


