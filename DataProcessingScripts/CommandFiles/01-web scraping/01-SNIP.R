# Note: existing project pulls from a distributed list of SNIP scores, but
# this script can be used to pull SNIP from more recent years if desired

# import libraries
library(tidyverse)
library(XML)
library(jsonlite)

path_to_processed_data <- "DataProcessingScripts/ProcessedData/"

# currently only grabs top 1000 SNIP values each year.
# If you want SNIP scores for all journals for all years, set this to something 
# that will always be greater than the number of pages in the search results;
# something like 100 might be safe
max_num_pages <- 2 

# this is the highest value that will work with offset
max_per_page <- 500

# initialize empty tibble
journal <- tibble(title = character(),
                  ISSN = numeric(),
                  snip = numeric(),
                  year = numeric())

generate_url <- function(offset, year) {
  #sample_url <- "https://www.scopus.com/sources?sortField=snip&sortDirection=desc&type=j&year=2012&offset=0&resultsPerPage=500"
  
  #offset should increment by the number of results per page
  offset_val <- offset*max_per_page 
  
  return(paste0("https://www.scopus.com/sources?sortField=snip&sortDirection=desc&type=j&year=",year,"&offset=",offset_val,"&resultsPerPage=",max_per_page))
}

# use a hidden JSON element in the page to extract all results
extract_results <- function(url) {
  return(read_file(url) %>% 
    htmlParse(asText = TRUE) %>%
    getNodeSet('//*[@id="resultsJson"]') %>% xmlValue() %>% fromJSON(flatten=T))
}

# for loop to go through different SNIP values for each year
for (year in 2012:2018) {
  
  offset <- 0
  
  first_url <- generate_url(offset, year)
  results_json <- extract_results(first_url)
  max_offset <- ceiling(results_json$totalResultsCount/max_per_page)
  
  first_results <- results_json$results %>% 
    dplyr::select(title, issn, snip) %>% 
    mutate(year = year) %>% 
    dplyr::rename(ISSN = issn)
  
  # store results from offset 0
  journal <- rbind(journal, first_results)
  
  # have to subtract 1 because offsets start at 0
  end_offset <- min(max_offset, max_num_pages) - 1
  
  if (end_offset > 0) {
    # loop if end of offsets greater than 0
    for (off in 1:end_offset) {
      results <- generate_url(off, year) %>%
        extract_results() %>%
        .$results %>% 
        dplyr::select(title,issn,snip) %>% 
        mutate(year = year) %>% 
        dplyr::rename(ISSN = issn)
      
      # store results from offset 0
      journal <- rbind(journal, results)
    }
    
  }
  
}

# find anything with multiple snip values in same year and take the smaller of the two values
journal <- journal %>% 
  group_by(title, ISSN, year) %>% 
  summarise(snip=min(snip)) %>% 
  ungroup()

# spread the snip values into different years
journal_years <- spread(journal, year, snip, fill = NA, convert = FALSE) %>%
  rename(Journal_title = title)

write_csv(journal_years, paste0(path_to_processed_data,"snip_indicators.csv"))
