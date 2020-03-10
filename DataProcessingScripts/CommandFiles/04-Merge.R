# Depends on files from web scraping, raw data processing, and API requests scripts

### Import libraries
library(readxl)
library(tidyverse)

# path to Shiny directory
shiny_dir <- "ShinyDashboard/"

### Load downloads from COUNTER data

# import journal name, number of PDF downloads, and sum of downloads from each year from xlsx
data <- read_excel("DataProcessingScripts/RawData/Elsevier COUNTER data + pricing for Data+ 20190322.xlsx", 1) %>%
  filter(Title != "Total") %>%
  mutate(downloads.2012 = `40909`+`40940`+`40969`+`41000`+`41030`+`41061`+`41091`+`41122`+`41153`+`41183`+`41214`+`41244`) %>%
  mutate(downloads.2013 = `41275`+`41306`+`41334`+`41365`+`41395`+`41426`+`41456`+`41487`+`41518`+`41548`+`41579`+`41609`) %>%
  mutate(downloads.2014 = `41640`+`41671`+`41699`+`41730`+`41760`+`41791`+`41821`+`41852`+`41883`+`41913`+`41944`+`41974`) %>%
  mutate(downloads.2015 = `42005`+`42036`+`42064`+`42095`+`42125`+`42156`+`42186`+`42217`+`42248`+`42278`+`42309`+`42339`) %>%
  mutate(downloads.2016 = `42370`+`42401`+`42430`+`42461`+`42491`+`42522`+`42552`+`42583`+`42614`+`42644`+`42675`+`42705`) %>%
  mutate(downloads.2017 = `42736`+`42767`+`42795`+`42826`+`42856`+`42887`+`42917`+`42948`+`42979`+`43009`+`43040`+`43070`) %>%
  mutate(downloads.2018 = `43101`+`43132`+`43160`+`43191`+`43221`+`43252`+`43282`+`43313`+`43344`+`43374`+`43405`+`43435`) %>%
  dplyr::select("Title", "Print ISSN", "Online ISSN", "Reporting period PDF", "downloads.2012", "downloads.2013", "downloads.2014", "downloads.2015", "downloads.2016", "downloads.2017", "downloads.2018") %>%
  rename("PDF.downloads" = "Reporting period PDF") %>%
  mutate(PDF.downloads = as.numeric(PDF.downloads),
         downloads.max = apply(.[5:11], 1, function(x) max(x, na.rm=TRUE)),
         `Print ISSN` = str_remove_all(`Print ISSN`, "-"),
         `Online ISSN` = str_remove_all(`Online ISSN`, "-")) %>%
  pivot_longer(c(`Print ISSN`,`Online ISSN`), names_to = "ISSN.type", values_to="ISSN", values_drop_na = TRUE) %>%
  group_by(ISSN) %>%
  mutate(duplicate = n()>1) %>%
  ungroup() 

### merge in list prices

list_price <- read_csv("DataProcessingScripts/ProcessedData/list_price.csv", col_types = cols(
  .default = col_character(),
  Price = col_double(),
  library = col_character(),
  order = col_integer(),
  missing_price = col_logical(),
  umbrella_title = col_logical(),
  final_price = col_character(),
  F_Title = col_character(),
  F_ISSN = col_character(),
  F_Price = col_double(),
  F_Count = col_integer()
))

# merge PDF downloads data with list prices, watching for ISSN duplicates
all <- full_join(data, list_price, by = c("ISSN" = "ISSN")) %>%
  dplyr::filter(!is.na(Price)) %>%
  dplyr::filter(duplicate == FALSE | is.na(duplicate) | ISSN.type == "Print ISSN") %>%
  mutate(Title.x = ifelse(is.na(Title.x),Title.y,Title.x)) %>%
  dplyr::select(-Title.y, -duplicate) %>%
  rename("Title" = "Title.x")

### merge in SNIP values

# import snip indicators from 2012 to 2016
snip <- read_excel("DataProcessingScripts/RawData/SNIP_IPP_SJR_complete_1999_2016_v2.xlsx", 1) %>%
  dplyr::select("Source Title", "2012 SNIP", "2013 SNIP", "2014 SNIP", "2015 SNIP", "2016 SNIP", "Print ISSN", "E-ISSN") %>%
  rename("Title" = "Source Title") %>%
  mutate_at(vars(contains("SNIP")), as.numeric) %>%
  mutate(median.snip = apply(.[2:6], 1, function(x) median(x, na.rm=TRUE))) %>%
  dplyr::select(Title, median.snip, "Print ISSN", "E-ISSN") %>%
  dplyr::filter(!is.na(median.snip)) %>%
  pivot_longer(c(`Print ISSN`,`E-ISSN`), names_to = "ISSN.type", values_to="ISSN", values_drop_na = TRUE) %>%
  group_by(ISSN) %>%
  mutate(duplicate = n()>1) %>%
  ungroup() 

# merge in SNIP data, watching for ISSN duplicates
all <- left_join(all, snip, by = c("ISSN"="ISSN")) %>%
  dplyr::select(-Title.y, -ISSN.type.y, -duplicate) %>%
  rename("Title" = "Title.x",
         "ISSN.type" = "ISSN.type.x")

### Merge in editorial board data

editorial_board <- read_csv("DataProcessingScripts/RawData/duke_editorial_board.csv")
editorial_board_oa <- read_csv("DataProcessingScripts/RawData/duke_editorial_board_oa.csv")

editors <- bind_rows(editorial_board, editorial_board_oa) %>% 
  mutate(ISSN = str_remove(ISSN, "-"),
         journal = str_replace(journal, "Genes and Diseases", "Genes & Diseases")) %>%
  distinct() %>%
  group_by(ISSN) %>%
  summarize(editors = T)

all <- left_join(all, editors, by = c("ISSN" = "ISSN")) %>%
  mutate(editors = replace_na(editors, FALSE))


### Merge in manually-determined subject categories

journal_categories <- read_csv("DataProcessingScripts/ProcessedData/journal_categories.csv") %>%
  group_by(ISSN) %>%
  summarise(
    Title=min(Title),
    subj.BUS = sum(BUS),
    subj.HUM = sum(HUM),
    subj.LAW = sum(LAW),
    subj.MED = sum(MED),
    subj.NSEG = sum(NSEG),
    subj.SSDG = sum(SSDG)
  ) %>%
  ungroup() 

# merge by title and filter out the ones without helpful information
all <- all %>%
  left_join(journal_categories, by = c("ISSN" = "ISSN")) %>%
  dplyr::select(-Title.y,) %>%
  rename("Title" = "Title.x")

### Merge in Duke publications

publish <- read_csv("DataProcessingScripts/ProcessedData/authored_by_duke_journals.csv", na=c("","NA","NULL")) %>%
  dplyr::select(-open_access) %>%
  rename("Title" = "journal_name") %>%
  group_by(Title, P.ISSN, E.ISSN, year) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = Freq) %>%
  pivot_longer(c(`P.ISSN`,`E.ISSN`), names_to = "ISSN.type", values_to="ISSN", values_drop_na = TRUE) %>%
  group_by(ISSN) %>%
  summarise(Published.2012 = sum(`2012`),
            Published.2013 = sum(`2013`),
            Published.2014 = sum(`2014`),
            Published.2015 = sum(`2015`),
            Published.2016 = sum(`2016`),
            Published.2017 = sum(`2017`),
            Published.2018 = sum(`2018`)) %>%
  ungroup() %>%
  mutate_at(vars(starts_with("Published")), ~replace_na(.,0)) %>%
  mutate(Published.mean = apply(.[2:8], 1, function(x) mean(x, na.rm=TRUE)))
  

# merge published data
all <- left_join(all, publish, by = c("ISSN" = "ISSN")) 

# import reviewers data
reviews <- read_csv("DataProcessingScripts/RawData/duke_researcher_journal_reviews.csv")
reviews$reviewers <- TRUE
colnames(reviews)[1] <- "Title"
colnames(reviews)[2] <- "Num.reviews"

# merge reviewers and change into logical variable
all <- left_join(all, reviews, by = c("Title"="Title"))
all$reviewers[is.na(all$reviewers)] <- FALSE

# final adjustments
all <- all %>%
  dplyr::select(Title, ISSN, Price, Type, PDF.downloads, downloads.2012, downloads.2013, downloads.2014, 
                downloads.2015, downloads.2016, downloads.2017, downloads.2018, downloads.max, median.snip, editors, 
                subj.BUS, subj.HUM, subj.LAW, subj.MED, subj.NSEG, subj.SSDG, Published.2012, Published.2013,
                Published.2014, Published.2015, Published.2016, Published.2017, Published.2018, Published.mean,
                reviewers)

write_csv(all, paste0(shiny_dir,"all_data_new.csv"))
