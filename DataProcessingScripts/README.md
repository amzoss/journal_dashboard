# Data Processing Scripts

## How to use

The scripts are organized in order of dependency, and scripts should include information in a comment at the top that indicates any dependency on other files. Attempts have been made to organize scripts so that parameters that need to be adjusted are included near the top of the file. Otherwise, files can be opened in R and run file by file to produce the correct output for the Shiny Dashboard. 

NOTE: Some files included (01-SNIP.R, 03a-ReparseCitedByForArticleURLs.R, 03b-DownloadArticleJSON.R, 03c-ParseArticlesForRefSrcTitle.R) produce output that is not currently being used in the dashboard. Furthermore, the final 04-Merge.R script containes some raw data processing not conducted in external scripts.

All files assume that the top-level directory ("journal_dashboard") is the working directory in R. One easy way to set up the code is to create a new project in RStudio and build the project directly from the GitHub clone URL for this repository. (Create New Project --> Version Control --> Git --> paste in GitHub URL as Repository URL)

## Maintenance of these scripts

This project is not being actively maintained, but the developer ([Angela Zoss](https://library.duke.edu/about/directory/staff/angela.zoss)) is happy to answer questions and review issues.
