# import libraries
library(rvest)
library(dplyr)
library(stringr)

# initialize variables

path_to_raw_data <- "DataProcessingScripts/RawData/"

duke_editors <- tibble("author" = as.character(),
                       "institute" = as.character(),
                       "journal" = as.character(),
                       "ISSN" = as.character())

# scrape journals available on elsevier for years earlier than 2016
# 22 webpages for journals published from 2012 to 2016
# check results to identify number of pages
# https://www.elsevier.com/catalog?producttype=journals&cat0=&publicationyear=2016&publicationyear=2015&publicationyear=2014&publicationyear=2013&publicationyear=2012&q=&search=1&imprintname=&categoryrestriction=&sort=datedesc
num_result_pages <- 22

for (num in 1:num_result_pages){
  titles <- read_html(paste0("https://www.elsevier.com/catalog?page=",num,"&author=&cat0=&categoryrestriction=&imprintname=&producttype=journals&publicationyear=2016&publicationyear=2015&publicationyear=2014&publicationyear=2013&publicationyear=2012&q=&series=&sort=datedesc")) %>%
    html_nodes(".listing-products-info-text-title a")

  # catch exception when the webpage doesn't exist
  if (length(titles) > 0) {
    
    journal_urls <- titles %>%
      html_attr("href")
    
    journal_names <- titles %>%
      html_text()
    
    slug <- journal_urls %>%
      tibble::enframe() %>%
      dplyr::select(-name) %>%
      mutate(value = str_remove_all(value, "https://")) %>%
      separate(value,as.character(c(1:10)), sep="/") %>%
      mutate_all(function(x){ifelse(str_detect(x,c("www","elsevier")) | str_length(x)==0 | x == "journals", NA, x)}) %>%
      lapply(as.list) %>%
      transpose() %>%
      lapply(function(x){x[!is.na(x)]}) %>%
      lapply(function(x){x[1]}) %>%
      unlist()
    
    journals <- tibble("url" = journal_urls, "name" = journal_names, "slug" = slug) %>%
      mutate("ISSN" = "")

    # extract all authors and institutions on the editorial board for each journal
    for (i in 1:nrow(journals)) {
      board_members <- tibble("author" = as.character(), "institute" = as.character(), "journal" = as.character(), "ISSN" = as.character())
      
      journal_link <- journals[i,"url"] %>% unlist()
      
      tmp1 <- try( issn <- read_html(journal_link) %>%
                    html_node(".keyword span") %>%
                    html_text(), silent=TRUE)
      
      if (!is.na(tmp1)){
        if (typeof(tmp1) == "character" & str_length(tmp1) == 9){
          journals[i,"ISSN"] <- issn
        }
      }
      
      tmp <- try(link <- paste0("https://www.journals.elsevier.com/",journals[i,"slug"],"/editorial-board") %>%
                   read_html(),silent=TRUE)
      if (typeof(tmp) == "list"){
        editor <- link %>%
          html_nodes(".publication-editor")
        
        if (length(editor) > 0) {
          
          for (j in 1:length(editor)) {
            auth <- editor[j] %>% html_nodes(".publication-editor-name") %>% html_text() %>% str_trim()
            insti <- editor[j] %>% html_nodes(".publication-editor-affiliation") %>% html_text()
            insti <- ifelse(length(insti)==0,"", insti)
            board_members <- bind_rows(board_members, list("author" = auth, "institute" = insti, "journal" = journals[i,"name"] %>% unlist(), "ISSN" = issn))
          }
          
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
        
  }

}

# export data
write_csv(duke_editors, paste0(path_to_raw_data,"duke_editorial_board.csv"))
