### NOTE: Script depends on journal lists built by
### 01-web scraping/02-OAListEditorialBoard.R and
### 02-process raw data/01-JournalList.R

### CREATE DIRECTORIES
dir.create("DataProcessingScripts/RawData/ScopusAPIJsons/", showWarnings = TRUE, recursive = TRUE)
dir.create("DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDuke/", showWarnings = TRUE, recursive = TRUE)
dir.create("DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDukeOA/", showWarnings = TRUE, recursive = TRUE)
dir.create("DataProcessingScripts/RawData/ScopusAPIJsonsCitedByDukeAmp", showWarnings = TRUE, recursive = TRUE)

### IMPORT LIBRARIES
library(tidyverse)
library(readxl)
library(jsonlite)

# set values for constant url parameters
header_string = "https://api.elsevier.com/content/search/scopus?query="
apiKey_string = "&apiKey=ADD-API-KEY"
count_string = "&count="
max_count = "200"
cursor_string = "&cursor="
httpAccept_string = "&httpAccept=application/json"

# set start and end year
start = 2012
end = 2018

# set list of affiliation ids
af_id = c("60008724", "60005200", "60030982", "60004654", "60008176", "60076653", "60020096", 
          "60018749", "60013629", "60076806", "60116608", "60113147", "60116609", "60108877")

# build url based on the type of search (authored or cited) and two search criteria
build_url = function(search1, search2, cursor, type) {
  # create url for journals with papers authored by duke
  if (type == "authored") {
    pubyear = search1
    af_id = search2
    af_string_front = "(af-id("
    af_string_end = "))"
    pubyear_string_front = "+AND+pubyear+is+"
    
    URL = paste0(header_string, af_string_front, af_id, af_string_end, pubyear_string_front, pubyear, 
                 apiKey_string, count_string, max_count, cursor_string, cursor, httpAccept_string)
  }
  
  # create url for diff types of journals cited by duke
  # note: "search2" comes from journal title lists, which are already HTML escaped
  if (type == "cited" | type == "citedoa" | type == "citedamp") {
    af_id = search1
    title = search2
    af_string_front = "af-id("
    af_string_end = ")"
    title_string_front = "+AND+refsrctitle(%22"
    title_string_end = "%22)"
    
    URL = paste0(header_string, af_string_front, af_id, af_string_end, title_string_front, title, title_string_end, 
                 apiKey_string, count_string, max_count, cursor_string, cursor, httpAccept_string)
  }
  return(URL)
}

# determine the number of additional requests
calc_calls = function(results) {
  if (results %% 200 == 0) { calls = results/200 - 1
  } else { calls = floor(results/200) }
  return(calls)
}

# make request to api for authored or cited papers and save json file
get_elsevier = function(search1, search2, type) {
  url = build_url(search1, search2, "*", type)
  json = read_json(url, simplifyVector = FALSE) 
  
  # write the json file to different folders based on type
  if (type == "authored") { write_json(json, paste0('DataProcessingScripts/RawData/ScopusAPIJsons/', search1, "-", search2, "-1", ".json")) }
  if (type == "cited") { write_json(json, paste0('DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDuke/', search1, "-", search2, "-1", ".json")) }
  if (type == "citedoa") { write_json(json, paste0('DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDukeOA', search1, "-", search2, "-1", ".json")) }
  if (type == "citedamp") { write_json(json, paste0('DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDukeAmp', search1, "-", search2, "-1", ".json")) }
  
  total_results = as.numeric(json$`search-results`$`opensearch:totalResults`)
  
  # determine the number of additional requests
  if (total_results > 200) {
    calls = calc_calls(total_results)
    next_url = json$`search-results`$link[[3]]$`@href`
    
    # make additional requests and save json
    for (i in 1:calls) {
      json = read_json(next_url, simplifyVector = FALSE)
      
      if (type == "authored") { write_json(json, paste0('DataProcessingScripts/RawData/ScopusAPIJsons/', search1, "-", search2, "-", i+1, ".json")) }
      if (type == "cited") { write_json(json, paste0('DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDuke/', search1, "-", search2, "-", i+1, ".json")) }
      if (type == "citedoa") { write_json(json, paste0('DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDukeOA', search1, "-", search2, "-", i+1, ".json")) }
      if (type == "citedamp") { write_json(json, paste0('DataProcessingScripts/RawData/ScopusAPIJsonsCitedbyDukeAmp', search1, "-", search2, "-", i+1, ".json")) }
      
      next_url = json$`search-results`$link[[3]]$`@href`
    }
  }
}

# make request to api by year and affiliation id to get authored papers
for (year in start:end) {
  sapply(af_id, get_elsevier, search1 = year, type = "authored")
}

# list of active journal titles
all_journals = read_csv("DataProcessingScripts/ProcessedData/active_journals.csv")
titles = as.character(all_journals$EncodedTitle)

# make request to api by id and title to get cited by papers
for (id in af_id) {
  sapply(titles, get_elsevier, search1 = id, type = "cited")
}

# list of open access titles
open_access = read_csv("DataProcessingScripts/ProcessedData/open_access_titles.csv")
oa_titles = as.character(open_access$EncodedTitle)

# make request to api by id and oa title to get cited by
for (id in af_id) {
  sapply(oa_titles, get_elsevier, search1 = id, type = "citedoa")
}

# list of titles containing ampersand character
ampersand = read_csv("DataProcessingScripts/ProcessedData/and_titles.csv")
amp_titles = as.character(ampersand$EncodedTitle)

# make request to api by id and amp title to get cited by
for (id in af_id) {
  sapply(amp_titles, get_elsevier, search1 = id, type = "citedamp")
}

# Search Guide: https://dev.elsevier.com/tips/ScopusSearchTips.htm
# Template URL: https://api.elsevier.com/content/search/scopus?query=(af-id(60008724))+AND+pubyear+is+2012&apiKey=d57d760380b6a31b2231c38a94946a37&count=200&start=0&httpAccept=application/json
# Template URL: https://api.elsevier.com/content/search/scopus?query=af-id(60008724)+AND+refsrctitle(%22Atmospheric%20Environment%22)&apiKey=3996a37d2f280e84cc9ac92bd1bf1635&count=200&cursor=*&httpAccept=application/json
