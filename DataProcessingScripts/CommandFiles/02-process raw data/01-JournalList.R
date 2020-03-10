### NOTE: Script depends on 01-web scraping/02-OAListEditorialBoard.R

### IMPORT LIBRARIES
library(readxl)
library(tidyverse)

### IMPORT DATA FILES
price.DUL <- read_excel("DataProcessingScripts/RawData/Elsevier COUNTER data + pricing for Data+ 20190322.xlsx", 2) %>%
  mutate(library = "DUL",
         order = row_number())
price.MCL <- read_excel("DataProcessingScripts/RawData/Elsevier COUNTER data + pricing for Data+ 20190322.xlsx", 3) %>%
  mutate(library = "MCL",
         order = row_number())
price.Ford <- read_excel("DataProcessingScripts/RawData/Elsevier COUNTER data + pricing for Data+ 20190322.xlsx", 4) %>%
  mutate(library = "Ford",
         order = row_number())

FC <- read_excel("DataProcessingScripts/RawData/Elsevier COUNTER data + pricing for Data+ 20190322.xlsx", 5, skip=1)  %>%
  rename("Title" = "Full Title")

price.FC <- read_excel("DataProcessingScripts/RawData/Elsevier Freedom Collection 2019 list price.xlsx", 1,
                       skip = 2) %>%
  dplyr::select(-...4) %>%
  dplyr::filter(!is.na(USD)) %>%
  rename("Title" = "Full Title",
         "Final.Net.Price" = "USD") %>%
  mutate(ISSN = str_remove_all(ISSN, "-"))

### CLEAN DATA FILES

# produced a combined table of title prices
temp.prices <- bind_rows(price.DUL,price.MCL,price.Ford) %>%
  mutate(
    missing_price = is.na(`Final Net Price`),
    umbrella_title = str_starts(ISSN,"F"),
    final_price = ifelse(missing_price == FALSE & umbrella_title == FALSE, "original", "umbrella")
  ) %>%
  rename("Final.Net.Price" = "Final Net Price") %>%
  split(., .$final_price)

# for "umbrella" titles where one price represents a small group of titles,
# tag each individual title with the name of the umbrella title
# and divide the price evenly across the individual titles
# then join back to titles that had individual pricing
final.prices <- temp.prices$umbrella %>%
  mutate(
    "F_Title"=ifelse(umbrella_title, Title,NA),
    "F_ISSN"= ifelse(umbrella_title, ISSN,NA),
    "F_Price"= ifelse(umbrella_title, Final.Net.Price,NA)
  ) %>%
  fill(starts_with("F_")) %>%
  dplyr::filter(!umbrella_title) %>%
  group_by(F_ISSN) %>%
  mutate(F_Count = n(),
         Final.Net.Price = F_Price/F_Count) %>%
  ungroup() %>%
  bind_rows(temp.prices$original) %>%
  mutate(ISSN = str_remove_all(ISSN, "-"))

# original title character substitutions; switched to URLencode, but have not tested API calls
#spec_char = rbind(c(" ", "%20"), c("&", "%26"), c("\\+", "%2B"), c("/", "-"), c("’", "'")
#spec_char = rbind(c(" ", "%20"), c("&", "%26"), c("\\+", "%2B"), c("/", "-"), c("’", "'"), c("'", "%27"))

# combine FC included list with FC prices for 2019
# limit to Active FC titles
# limit to titles where we have prices
FC.combined <- FC %>% 
  dplyr::filter(Status=="Active") %>%
  full_join(price.FC, by=c("ISSN" = "ISSN")) %>%
  dplyr::filter(!is.na(Final.Net.Price)) %>%
  mutate(Title.x = ifelse(is.na(Title.x), Title.y, Title.x)) %>%
  dplyr::select(-Title.y) %>%
  rename("Title" = "Title.x")

### MERGE DATA FILES

# combine Freedom (filtered to active) and Subscriptions
# remove duplicates based on ISSN (keep Subscribed version and whatever was the higher price)
# trim spaces on title
# Encode non-alphanumeric characters in title
subs_and_freedom <- bind_rows(
  FC.combined %>%
    mutate(Type = "Freedom"),
  final.prices %>%
    mutate(Type = "Subscribed")
  ) %>%
  group_by(ISSN) %>%
  mutate(duplicate = n()>1,
         Final.Net.Price = max(Final.Net.Price)) %>%
  ungroup() %>%
  dplyr::filter(duplicate == FALSE | Type == "Subscribed") %>%
  dplyr::select(-duplicate) %>%
  mutate(Title = str_trim(Title)) %>%
  rowwise() %>%
  mutate(EncodedTitle = URLencode(Title, reserved=TRUE)) %>%
  ungroup() 
  
write_csv(subs_and_freedom %>% dplyr::select(Title, ISSN, Type, EncodedTitle), 
          "DataProcessingScripts/ProcessedData/active_journals.csv")

### OPEN ACCESS JOURNALS
open_access <- read_csv("DataProcessingScripts/RawData/open_access_journal.csv") %>%
  mutate(journal = str_trim(journal)) %>%
  rowwise() %>%
  mutate(EncodedTitle = URLencode(journal, reserved=TRUE)) %>%
  ungroup() %>%
  mutate(ISSN = str_remove_all(ISSN, "-"))

write_csv(open_access, "DataProcessingScripts/ProcessedData/open_access_titles.csv")

open_access <- open_access %>%
  rename("Title" = "journal") %>%
  mutate(Final.Net.Price = 0,
         Type = "Open")

# blend Subscriptions and Freedom Collection with Open Access
# eliminate duplicates by ISSN 
#   (set price to 0 and type to Open if Open is available but keep all other information)
list_price <- bind_rows(subs_and_freedom, open_access) %>%
  group_by(ISSN) %>%
  mutate(duplicate = n()>1,
         Final.Net.Price = min(Final.Net.Price),
         NewType = ifelse("Open" %in% Type, "Open", Type)) %>%
  ungroup() %>%
  dplyr::filter(duplicate == FALSE | Type != "Open") %>%
  mutate(Type = NewType) %>%
  dplyr::select(-duplicate, -NewType) %>%
  rename("Price" = "Final.Net.Price")

write_csv(list_price,"DataProcessingScripts/ProcessedData/list_price.csv")


# JOURNALS CONTAINING AMPERSAND

# detect ampersands and replace with "and"
and_title <- subs_and_freedom %>%
  dplyr::filter(str_detect(EncodedTitle,"%26")) %>%
  mutate(EncodedTitle = str_replace_all(EncodedTitle, "%26", "and"))

write_csv(and_title %>% dplyr::select(Title, ISSN, Type, EncodedTitle), "DataProcessingScripts/ProcessedData/and_titles.csv")
