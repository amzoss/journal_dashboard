# Duke Libraries Journal Usage Dashboard
R scripts to prepare journal usage data and generate a dashboard

## Process Documentation

* Web Scraping
    * Purpose: Obtain the list of open access journals from Elsevier and the list of Duke researchers on editorial boards of all journals on Elsevier using web scraping. Optionally scrape SNIP values from the search interface.
    * Method: Concatenate the base URL and the journal stub to generate a link for detailed data. Go to the webpage that contains the data we need and select elements of interest inside the HTML.
    * Code Folder: DataProcessingScripts/CommandFiles/01-web scraping
    * Code Files: 01-SNIP.R, 02-OAListEditorialBoard.R, 03-EditorialBoard.R

	
* Processing Raw Data
    * Purpose: Combine raw for titles in collection, open access titles, price lists, and subject classifications into datasets ready to merge by title.
    * Method: Load files that contain title, price, and subject information. Clean up "umbrella" titles - small bundles that have a single price listed for a group of separate titles. Reconcile duplicates across collection types. Subset titles for later API searching. Merge data files containing manual journal classifications and format for later data blending.
    * Code Folder: DataProcessingScripts/CommandFiles/02-process raw data
    * Code Files: 01-JournalList.R, 02-SubjectCategories.R


* API Searching
    * Purpose: Obtain information on journals with publications or citations by Duke researchers, using the Scopus API. 
    * Method:
        * For publications by Duke authors, iterate over a set of years and a set of affiliation IDs to capture all Duke pubications for a time period in JSON format. Extract a list of the years and sources of those publications for later blending. 
        * For citations by Duke authors, use URL-encoded journal title to search the "reference source title" field of journals, also specifying the Duke affiliation ID. Retreive and parse JSON for Duke publications that have citations to the titles of interest.
        * NOTE: The title matching with the reference source title is imprecise enough that we have chosen not to use this citation data. For example, journals with generic names like "Medicine" match with many many more titles than they should. This folder also includes an early attempt to iterate through all publications by Duke authors generated from the original publications process, grab article URLs, request article JSONs, and parse for reference source titles directly. The next step would be to create a way of matching the titles that show up in this field to the formal journal name and/or ISSN. 
    * Code Folder: DataProcessingScripts/CommandFiles/03-API requests
    * Code Files: 01-DownloadPublicationsCitations.R, 02-CleanPublicationsCitations.R, 03a-ReparseCitedByForArticleURLs.R, 03b-DownloadArticleJSON.R, 03c-ParseArticlesForRefSrcTitle.R
	

* Data Merging
    * Purpose: Merge all data collected to create a general spreadsheet containing all variables that will serve as the database for the dashboard.
    * Method: With a base of the titles that we have pricing information form, merge in data from downloads (COUNTER data), using ISSN to match across datasets wherever possible. Export a file into the dashboard directory.
    * Code Folder: DataProcessingScripts/CommandFiles
    * Code Files: 04-Merge.R
	

* Shiny App
    * Purpose: Build a dashboard that visualize changes in the selected pool of journals based on user inputs for usage criteria (subject, highest yearly download, total citations, existence of publication, snip value, editorial activity).
    * Method: Use RShiny widgets to create sliders and checkboxes to enable users to set levels for various variables in the merged dataset. Add tooltips that explain how each criterion was calculated. Create a commit change button that submits the changes and causes visualizations to change dynamically, as well as a button to reset the filters to the original configuaraion. Display total price and table for selected and non-selected journals, and create download buttons for both. Create basic plots of the features of the selected titles. NOTE: text throughout the dashboard is controlled by the settings.xlsx file, allowing users to easily customize the dashboard display.
    * Code Folder: ShinyDashboard
    * Code Files: app.R, all_data_new.csv, settings.xlsx

## Maintenance of these scripts

This project is not being actively maintained, but the developer ([Angela Zoss](https://library.duke.edu/about/directory/staff/angela.zoss)) is happy to answer questions and review issues.
