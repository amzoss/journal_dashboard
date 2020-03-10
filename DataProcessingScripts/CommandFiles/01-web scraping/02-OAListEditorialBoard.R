# import libraries
library(rvest)
library(tidyverse)

path_to_raw_data <- "DataProcessingScripts/RawData/"

# keep track of duke editors as we find them
duke_editors <- tibble("author" = as.character(),
                       "institute" = as.character(), 
                       "journal" = as.character(), 
                       "ISSN" = as.character())

# extract list of names of journals on the open access list
# this isn't precise, but should be able to filter out problems later
journal_link_elements <- read_html("https://www.elsevier.com/about/open-science/open-access/open-access-journals") %>%
  html_nodes("main p a") 

journal_urls <- journal_link_elements %>% 
  html_attr("href") %>%
  modify(function(x){sub("/$","",x)}) %>%
  modify(function(x){sub("^/journals","https://www.elsevier.com/journals",x)})

journal_names <- journal_link_elements %>%
  html_text()

journals <- tibble("url" = journal_urls, "name" = journal_names) %>%
  mutate("ISSN" = "")

# scrape names by url
for (i in 1:nrow(journals)) {
  board_members <- tibble("author" = as.character(), "institute" = as.character(), "journal" = as.character(), "ISSN" = as.character())
  journal_link <- journals[i,"url"] %>% unlist()
  
  # follow journal link and try to get ISSN
  issn <- read_html(journal_link) %>%
    html_node(".keyword span") %>%
    html_text()
  
  journals[i,"ISSN"] <- issn

  tmp <- try(eb_link <- paste0(journal_link,"/editorial-board") %>%
               read_html(),silent=TRUE)
  if (typeof(tmp) == "list"){
    editor <- eb_link %>%
      html_nodes(".publication-editor")
    
    if (length(editor) > 0) {
      
      for (j in 1:length(editor)) {
        auth <- editor[j] %>% html_nodes(".publication-editor-name") %>% html_text() %>% str_trim()
        insti <- editor[j] %>% html_nodes(".publication-editor-affiliation") %>% html_text()
        insti <- ifelse(length(insti)==0,"", insti)
        board_members <- bind_rows(board_members, list("author" = auth, "institute" = insti, "journal" = journals[i,"name"] %>% unlist(), "ISSN" = issn))
      }
      
      #TODO: lower case institution and search for " duke " instead? Could be missing all caps, typos...
      board_members <- board_members %>%
        dplyr::filter(str_detect(institute, "Duke") &
                        !institute %in% c("Duke-National University of Singapore (NUS) Graduate Medical School, Singapore, Singapore",
                                          "Duke-NUS Medical School, Singapore, Singapore",
                                          "Duke-NUS Medical School Centre for Cognitive Neuroscience, Singapore, Singapore",
                                          "Singapore General Hospital, Department of Emergency Medicine; SingHealth Services, Data Analytics, Health Services Research Center (HSRC); Duke-NUS, Health Services and Systems Research, Singapore, Singapore",
                                          "National Neuroscience Institute, Duke NUS Medical School, Singapore, Singapore"
                        ))
      duke_editors <- bind_rows(duke_editors, board_members)
      
    }
    
  }
  else {
    closeAllConnections()
  }
  
}

open_access <- journals %>% dplyr::select(name, ISSN) %>% rename("journal" = "name") %>% drop_na(ISSN)
  
write_csv(duke_editors, paste0(path_to_raw_data,"duke_editorial_board_oa.csv"))
write_csv(open_access, paste0(path_to_raw_data,"open_access_journal.csv"))
