### Start with active journal list and manually add discipline information

# import libraries
library(tidyverse)
library(readxl)

# import the list of active Elsevier titles and their subjects
data <- read_excel("DataProcessingScripts/RawData/titles_with_subject.xlsx", 
                   1, 
                   .name_repair = "universal") %>%
  dplyr::select("Full.Title", "ISSN", "DUL.Discipline.Groups") %>%
  mutate(DUL.Discipline.Groups = str_replace(DUL.Discipline.Groups, "SSG", "SSDG")) %>% 
  distinct() %>%
  mutate(count = 1) %>%
  spread(DUL.Discipline.Groups, count, fill = NA, convert = FALSE) %>%
  rename("Title" = "Full.Title")

# import unclassified journals
uncat <- read_csv("DataProcessingScripts/RawData/titles_with_subject_supplement.csv")

data <- bind_rows(data, uncat)


# manual changes of journals with multiple/wrong categories
data$SSDG[which(data$Title=="L'Anthropologie")] = NA
data$NSEG[which(data$Title=="Accident Analysis & Prevention")] = NA
data$BUS[which(data$Title=="Accident Analysis & Prevention")] = NA
data$LAW[which(data$Title=="Accident Analysis & Prevention")] = NA
data$LAW[which(data$Title=="International Review of Law and Economics")] = 1
data$SSDG[which(data$Title=="International Review of Law and Economics")] = NA
data$LAW[which(data$Title=="International Journal of Law and Psychiatry")] = 1
data$NSEG[which(data$Title=="International Journal of Law and Psychiatry")] = NA
data$LAW[which(data$Title=="Computer Law & Security Review")] = 1
data$NSEG[which(data$Title=="Computer Law & Security Review")] = NA

# export data
data <- data %>% mutate(Title = str_trim(Title))
write_csv(data, "DataProcessingScripts/ProcessedData/journal_categories.csv")
