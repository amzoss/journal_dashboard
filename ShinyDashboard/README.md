# Journal Dashboard

This is a Shiny app that loads in a merged data file of journal metrics and allows users to filter, visualize, and export subsets of the data.

## How to use

This dashboard is a Shiny app. Shiny is an R package and also a run-time environment, so Shiny apps can only be run on a computer or server with Shiny installed. I tend to run Shiny locally for testing, using the Run App functionality in RStudio, but for sharing I publish the app to [shinyapps.io](https://www.shinyapps.io/). You can also host your own Shiny server at your institution.

All files assume that the top-level directory ("journal_dashboard") is the working directory in R. One easy way to set up the code is to create a new project in RStudio and build the project directly from the GitHub clone URL for this repository. (Create New Project --> Version Control --> Git --> paste in GitHub URL as Repository URL)

## Settings

Text throughout the dashboard is controlled by the settings.xlsx file, allowing users to easily customize the dashboard display. Simply edit the Excel file in Excel to change the text. Save the file and reload the app to see the changes.

## Maintenance of these scripts

This project is not being actively maintained, but the developer ([Angela Zoss](https://library.duke.edu/about/directory/staff/angela.zoss)) is happy to answer questions and review issues.
