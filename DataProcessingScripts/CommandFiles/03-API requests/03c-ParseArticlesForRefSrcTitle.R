### Depends on 03b-DownloadArticleJSON.R

#look for cited reference element
#   /abstracts-retrieval-response/item/bibrecord/tail/bibliography/reference/ref-info/ref-sourcetitle
#do our own string matching to confirm the citation

library(tidyverse)
library(dplyr)
library(jsonlite)
library(stringr)

#articles_df <- read_csv("DataProcessingScripts/ProcessedData/cited_by_article_urls.csv")


# create list of all json files
filepath = "DataProcessingScripts/RawData/ScopusAPIArticles"
filelist = list.files(path = filepath, pattern = ".json", full.names = TRUE)

references_df = tibble(scopus_id = character(),
                       ref_num = character(),
                       ref_fulltext = character(),
                       ref_sourcetitle = character())

for (file in filelist) {
  json_data <- fromJSON(file)
  
  # find number of references in the bibliography and continue if references > 0
  num_refs <- as.numeric(json_data$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$`@refcount`)
  if (length(num_refs) > 0) {
    if (num_refs > 0) {
      scopus_id <- NULL
      all_refs <- NULL
      all_full <- NULL
      all_nums <- NULL
      all_source_titles <- NULL
      scopus_id <- json_data$`abstracts-retrieval-response`$coredata$`dc:identifier` %>% str_replace('SCOPUS_ID:','')
      all_refs <- json_data$`abstracts-retrieval-response`$item$bibrecord$tail$bibliography$reference
      all_full <- all_refs$`ref-fulltext` %>% as.character()
      all_nums <- all_refs$`@id` %>% as.character()
      all_source_titles <- all_refs$`ref-info`$`ref-sourcetitle` %>% as.character()
      current_ref_df <- tibble(scopus_id = scopus_id,
                               ref_num = all_nums, 
                               ref_fulltext =  ifelse(length(all_full) == 0, '', all_full),
                               ref_sourcetitle = ifelse(length(all_source_titles) == 0, '', all_source_titles))
      
      # bind per ref list to the overall references df
      references_df = rbind(references_df, current_ref_df)
      
    }
  }  
}

write_csv(references_df, "DataProcessingScripts/ProcessedData/article_refs_source_titles.csv")

source_titles_unique <- references_df %>% dplyr::select(ref_sourcetitle) %>% distinct()

write_csv(source_titles_unique, "DataProcessingScripts/ProcessedData/source_titles_unique.csv")

