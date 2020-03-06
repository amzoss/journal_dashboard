#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### IMPORT LIBRARIES
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(plotly)
#devtools::install_github("ijlyttle/bsplus")
library(bsplus)
library(readxl)

### IMPORT DATA

# Reads in data, pivoting the subject columns into a single column named "subject"
# Note: subject columns in data file should start with "subj"
data <- read.csv("all_data.csv", header = TRUE, encoding = 'UTF-8') %>%
    gather(., starts_with('subj'), key = "subject", value = "number") %>%
    filter(!is.na(number)) %>%
    dplyr::select(-number)

# Reads in labels for subject columns from the settings file
disciplines <- read_xlsx('settings.xlsx',sheet='disciplines')

# Joins labels for subjects into the data
data <- data %>% left_join(disciplines, by = c("subject" = "abbreviation")) %>%
    dplyr::filter(display==TRUE)

# Pull out the list of subjects to use in the filter
disp_display_list <- sort(unique(data$discipline.group))

# Create a nested list to store text settings for dashboard
text_settings <- read_xlsx('settings.xlsx',sheet='text_for_dashboard') %>%
    mutate(label = replace_na(label, ""))

labels <- lapply(split(text_settings[,-1], text_settings[1]), unlist)

# Set up defaults for the dashboard filters
filter_defaults <- list(
    "discbox" = disp_display_list,
    "downloads" = 0,
    "snip" = 0,
    "pubedit" = "No Activity Filter",
    "collbox" = c("Freedom", "Subscribed", "Open")
)

# Define UI for application
ui <- fluidPage(
    
        theme = shinytheme("cosmo"),
        title = labels$title["label"],
        titlePanel(strong(labels$title["label"])),
        
        # provide users with instructions on the selection bar
        span(helpText(strong(labels$instructions["label"]))),
        
        # create row of subject area and usage criteria settings
        fluidRow(
            # subject filter
            column(2,
                   em(labels$discipline_filter["instruction_text"]),
                   checkboxGroupInput(
                       inputId = "discbox", 
                       label = strong(labels$discipline_filter["label"]),
                       choices = disp_display_list,
                       selected = filter_defaults$discbox
                   )
            ),
            # downloads filter
            column(2,
                   em(labels$download_filter["instruction_text"]),
                   numericInput(
                       inputId = "downloads", 
                       label = paste0(labels$download_filter["label"],
                                      " (Range: ",
                                      round(min(data$downloads.max, na.rm = T), 0),
                                      "-",
                                      format(round(max(data$downloads.max, na.rm = T), 0), big.mark = ",", scientific = F),
                                      ")"), 
                       min = round(min(data$downloads.max, na.rm = T), 0), 
                       max = round(max(data$downloads.max, na.rm = T), 0), 
                       value = filter_defaults$downloads,
                       #step = signif(max(data$downloads.max, na.rm = T)/6,1)
                       step = 1
                   ) %>%
                       shinyInput_label_embed(
                           shiny_iconlink() %>%
                               bs_embed_tooltip(
                                   title = labels$download_filter["extra_info_hover"], 
                                   placement = "left"
                               )
                       ),
                   use_bs_tooltip()
            ),
            # SNIP filter
            column(2,
                   em(labels$snip_filter["instruction_text"]),
                   sliderInput(
                       inputId = "snip", 
                       label = labels$snip_filter["label"], 
                       min = round(min(data$median.snip, na.rm = T), 0), 
                       max = round(max(data$median.snip, na.rm = T), 0), 
                       value = filter_defaults$snip
                   ) %>%
                       shinyInput_label_embed(
                           shiny_iconlink() %>%
                               bs_embed_tooltip(
                                   title = labels$snip_filter["extra_info_hover"], 
                                   placement = "left"
                               )
                       ),
                   use_bs_tooltip()
            ),
            # publication/editorial board filter
            column(2,
                   em(labels$pubedit_filter["instruction_text"]),
                   radioButtons(
                       inputId = "pubedit", 
                       label = labels$pubedit_filter["label"],
                       choices = c("No Activity Filter",
                                   "Publication Activity Only", 
                                   "Editorial Activity Only", 
                                   "Both Publication and Editorial Activity", 
                                   "Either Publication or Editorial Activity"),
                       selected = filter_defaults$pubedit
                   )     
            ),
            # collection type filter
            column(2,
                   em(labels$collection_filter["instruction_text"]),
                   checkboxGroupInput(
                       inputId = "collbox", 
                       label = strong(labels$collection_filter["label"]),
                       choices = c("Freedom", "Subscribed", "Open"),
                       selected = filter_defaults$collbox
                   )
            )
        ),
        
        # create action buttons to commit changes to filters or reset filters
        # note: reset filters button resets the filters, then the changes must be committed
        fluidRow(
            column(2,
                   actionButton(
                       inputId = "commitbox",
                       label = labels$commit_button["label"]
                   )
            ),
            column(2,
                   actionButton(
                       inputId = "reset_input",
                       label = labels$reset_button["label"])
            )
        ),
        
        br(),
        br(),
        
        # create tabs for different visualization panels
        tabsetPanel(type = "tabs",
                    tabPanel(labels$table_tabset_name["label"], 
                             tags$head(tags$style(".bigtext {font-size: 50px; font-style: bold}")),
                             tags$head(tags$style(".bignum {color: blue; font-size: 50px; font-style: bold; line-height: 1}")),
                             tags$head(tags$style(".medtext {font-size: 25px; font-style: bold; margin-top: 25px}")),
                             p(labels$selected_price_heading["label"], class="bigtext"),
                             span(textOutput("text1"), class="bignum"),
                             p(labels$selected_table_heading["label"], class="medtext"),
                             dataTableOutput("table1"),
                             downloadButton(outputId = "download1", label = labels$selected_download_label["label"]),
                             br(),
                             br(),
                             p(labels$non_selected_price_heading["label"], class="bigtext"),
                             span(textOutput("text5"), class="bignum"),
                             p(labels$non_selected_table_heading["label"], class="medtext"),
                             dataTableOutput("table2"),
                             downloadButton(outputId = "download2", label = labels$non_selected_download_label["label"])),
                    tabPanel(labels$journal_coll_tabset_name["label"], plotlyOutput("horbar")),
                    tabPanel(labels$journal_coll_subj_tabset_name["label"], plotlyOutput("horbar2")),
                    tabPanel(labels$price_subj_tabset_name["label"], plotlyOutput("pricebar")),
                    tabPanel(labels$down_title_tabset_name["label"], plotlyOutput("cumdownloads")),
                    tabPanel(labels$pub_title_tabset_name["label"], plotlyOutput("cumpubl"))
        )
    )
    


### SERVER FUNCTION
server <- function(input, output, session) {

    data_subset <- eventReactive(input$commitbox==TRUE, {
        
        # set subset to all data at first
        data_filt <- data

        # filter dataset based on subject area checkbox
        data_filt <- data_filt %>%
            dplyr::filter(discipline.group %in% input$discbox)
        
        # filter dataset based on download slider
        if (input$downloads > 0) {
            data_filt <- data_filt %>% dplyr::filter(downloads.max >= input$downloads)
        }
        
        # filter dataset based on snip filter
        if (input$snip > 0) {
            data_filt <- data_filt %>% dplyr::filter(median.snip >= input$snip)
        }
        
        # filter dataset based on publication/editorial filter
        if (input$pubedit == "Publication Activity Only") {
            data_filt <- data_filt %>% dplyr::filter(Published.mean > 0)
        }
        else if (input$pubedit == "Editorial Activity Only") {
            data_filt <- data_filt %>% dplyr::filter(editors == TRUE | reviewers == TRUE)
        }
        else if (input$pubedit == "Both Publication and Editorial Activity") {
            data_filt <- data_filt %>% dplyr::filter(Published.mean > 0) %>% dplyr::filter(editors == TRUE | reviewers == TRUE)
        }
        else if (input$pubedit == "Either Publication or Editorial Activity") {
            data_filt <- data_filt %>% dplyr::filter(Published.mean > 0 | editors == TRUE | reviewers == TRUE)
        }
        
        # filter dataset based on collection type filter
        data_filt <- data_filt %>%
            dplyr::filter(Type %in% input$collbox)
        
        # return the filtered dataframe
        return(data_filt)
    })
    
    data_diff <- eventReactive(input$commitbox, {
        # find the excluded journals based on data_subset
        data_diff = setdiff(data, data_subset())
        return(data_diff)
    })
    
    output$text1 <- renderText({
        # sum up the total price and format
        total_price = format(sum(data_subset()[,c("Price")]), big.mark = ",", scientific = F)
        paste0("$", total_price)
    })
    
    output$table1 <- renderDataTable({
        # take data subset and make some formatting changes for display
        data_selected = data_subset() %>%
            mutate(Price = round(Price, 0)) %>%
            mutate(Price = format(Price, big.mark = ",", scientific = F)) %>%
            mutate(Price = paste0("$", Price)) %>%
            mutate(downloads.max = round(downloads.max, 0)) %>%
            mutate(Published.mean = round(Published.mean, 2)) %>%
            mutate(median.snip = round(median.snip, 1))
        
        data_selected[,c("Title", "Price", "downloads.max", "Published.mean", "editors", "reviewers", "median.snip", "discipline.group", "discipline.name","Type")]
    })
    
    output$text5 <- renderText({
        # sum up the total price and format
        total_price2 = format(sum(data_diff()[,c("Price")]), big.mark = ",", scientific = F)
        paste0("$", total_price2)
    })
    
    output$table2 <- renderDataTable({
        # take excluded journals and make some formatting changes for display
        data_nonselect = data_diff() %>%
            mutate(Price = round(Price, 0)) %>%
            mutate(Price = format(Price, big.mark = ",", scientific = F)) %>%
            mutate(Price = paste0("$", Price)) %>%
            mutate(downloads.max = round(downloads.max, 0)) %>%
            mutate(Published.mean = round(Published.mean, 2)) %>%
            mutate(median.snip = round(median.snip, 1))
        
        data_nonselect[,c("Title", "Price", "downloads.max", "Published.mean", "editors", "reviewers", "median.snip", "discipline.group", "discipline.name", "Type")]
    })
    
    output$horbar <- renderPlotly({
        if (nrow(data_subset()) > 0) {
            subset_spread <- data_subset() %>% group_by(Type) %>% summarise(count = n()) %>% 
                pivot_wider(names_from = Type, values_from = count)
            plot_ly(data = subset_spread, y=1, x = ~Subscribed, type = 'bar', orientation = 'h', name = 'Subscribed', text = ~paste0(labels$journal_coll_chart_sub_series["label"], ': ', Subscribed), textposition = 'auto') %>%
                add_trace(x = ~Freedom, name = 'Freedom', text = ~paste0(labels$journal_coll_chart_free_series["label"], ': ', Freedom), textposition = 'auto') %>%
                add_trace(x = ~Open, name = 'Open', text = ~paste0(labels$journal_coll_chart_open_series["label"], ': ', Open), textposition = 'auto') %>%
                layout(xaxis = list(title = labels$journal_coll_chart_x["label"]), 
                       yaxis = list(title = labels$journal_coll_chart_y["label"], showticklabels = FALSE),
                       barmode = 'stack',
                       showlegend = FALSE)
        }
    })
    
    output$horbar2 <- renderPlotly({
        if (nrow(data_subset()) > 0) {
            subject_order <- data_subset() %>% group_by(discipline.group, display) %>% summarise(count = n()) %>%
                dplyr::filter(display == TRUE) %>%
                arrange(count) %>% dplyr::select(discipline.group) %>% unlist()
            subset_spread2 <- data_subset() %>% group_by(Type, discipline.group, display) %>% summarise(count = n()) %>% 
                pivot_wider(names_from = Type, values_from = count) %>%
                dplyr::filter(display == TRUE) %>%
                ungroup() %>%
                mutate(discipline.group = factor(discipline.group, levels = subject_order))
                
            plot_ly(data = subset_spread2, y=~discipline.group, x = ~Subscribed, type = 'bar', orientation = 'h', name = 'Subscribed', text = ~paste0(labels$journal_coll_subj_chart_sub_series["label"], ': ', Subscribed), textposition = 'auto') %>%
                add_trace(x = ~Freedom, name = 'Freedom', text = ~paste0(labels$journal_coll_subj_chart_free_series["label"], ': ', Freedom), textposition = 'auto') %>%
                add_trace(x = ~Open, name = 'Open', text = ~paste0(labels$journal_coll_subj_chart_open_series["label"], ': ', Open), textposition = 'auto') %>%
                layout(xaxis = list(title = labels$journal_coll_subj_chart_x["label"]), 
                       yaxis = list(title = labels$journal_coll_subj_chart_y["label"]),
                       barmode = 'stack',
                       showlegend = FALSE)
        }
    })
    
    output$pricebar <- renderPlotly({
        if (nrow(data_subset()) > 0) {
            subset_count <- data_subset() %>% group_by(discipline.group, display) %>% summarise(Price = sum(Price)) %>% 
                dplyr::filter(display == TRUE) %>%
                ungroup() %>%
                mutate(discipline.group = fct_reorder(as_factor(discipline.group), Price)) %>%
                mutate(PriceText = paste0("$", format(Price, big.mark = ",", scientific = F, trim=T)))
            
            plot_ly(data = subset_count, y=~discipline.group, x = ~Price, type = 'bar', orientation = 'h', text = ~PriceText, textposition = 'auto') %>%
                layout(xaxis = list(title = labels$price_subj_chart_x["label"]), 
                       yaxis = list(title = labels$price_subj_chart_y["label"]),
                       showlegend = FALSE)
        }
    })
    
    output$cumdownloads <- renderPlotly({
        if (nrow(data_subset()) > 0) {
            subset_downloads <- data_subset() %>% 
                mutate(PDF.downloads = replace_na(PDF.downloads, 0)) %>%
                arrange(desc(PDF.downloads)) %>%
                mutate(cum.PDF = cumsum(PDF.downloads),
                       order = row_number(),
                       pct_downloads = cum.PDF/max(cum.PDF, na.rm=T),
                       pct_titles = order/max(order, na.rm=T))
            fifty_pct_dwnls <- subset_downloads %>% dplyr::filter(pct_downloads > .5) %>%
                dplyr::filter(pct_downloads == min(pct_downloads)) %>% dplyr::select(cum.PDF) %>% unlist()
            last_journal <- nrow(subset_downloads)
            plot_ly(data = subset_downloads, x = ~order, y = ~cum.PDF, type="scatter", mode="markers", 
                    text = ~paste0(Title, "\n", 
                                   labels$down_title_chart_hover_label_price["label"], ": ", "$", Price, "\n", 
                                   labels$down_title_chart_hover_label_down["label"], ": ", PDF.downloads, "\n", 
                                   labels$down_title_chart_hover_label_rank["label"], ": ", order), 
                    hoverinfo = "text") %>%
                layout(xaxis = list(title = labels$down_title_chart_x["label"]), 
                       yaxis = list(title = labels$down_title_chart_y["label"]),
                       shapes = list(
                           list(type = "line",
                                x0 = 0, x1 = last_journal, xref = "x",
                                y0 = fifty_pct_dwnls, y1 = fifty_pct_dwnls, yref = "y")),
                       annotations = list(
                           list(showarrow = F,
                                x = last_journal,
                                y = fifty_pct_dwnls,
                                text = labels$down_title_chart_50["label"],
                                xanchor = "right",
                                yshift = 10
                       )))
        }
    })
    
    output$cumpubl <- renderPlotly({
        if (nrow(data_subset()) > 0) {
            subset_published <- data_subset() %>% 
                mutate(Published.2018 = replace_na(Published.2018, 0)) %>%
                arrange(desc(Published.2018)) %>%
                mutate(cum.publ = cumsum(Published.2018),
                       order = row_number(),
                       pct_publ = cum.publ/max(cum.publ, na.rm=T),
                       pct_titles = order/max(order, na.rm=T))
            fifty_pct_publ <- subset_published %>% dplyr::filter(pct_publ > .5) %>%
                dplyr::filter(pct_publ == min(pct_publ)) %>% dplyr::select(cum.publ) %>% unlist()
            last_journal <- nrow(subset_published)
            plot_ly(data = subset_published, x = ~order, y = ~cum.publ, type="scatter", mode="markers", 
                    text = ~paste0(Title, "\n", 
                                   labels$pub_title_chart_hover_label_price["label"], ": ", "$", Price, "\n", 
                                   labels$pub_title_chart_hover_label_pub["label"], ": ", Published.2018, "\n",
                                   labels$pub_title_chart_hover_label_rank["label"], ": ", order), 
                    hoverinfo = "text") %>%
                layout(xaxis = list(title = labels$pub_title_chart_x["label"]), 
                       yaxis = list(title = labels$pub_title_chart_y["label"]),
                       shapes = list(
                           list(type = "line",
                                x0 = 0, x1 = last_journal, xref = "x",
                                y0 = fifty_pct_publ, y1 = fifty_pct_publ, yref = "y")),
                       annotations = list(
                           list(showarrow = F,
                                x = last_journal,
                                y = fifty_pct_publ,
                                text = labels$pub_title_chart_50["label"],
                                xanchor = "right",
                                yshift = 10
                           )))
        }
    })
    
    output$download1 <- downloadHandler (
        # download button for selected journals
        filename = function() {
            paste0(labels$download_filename_selected["label"], ".csv")
        },
        content = function(file) {
            write.csv(data_subset(), file, row.names = FALSE)
        }
    )
    
    output$download2 <- downloadHandler (
        # download button for non-selected journals
        filename = function() {
            paste0(labels$download_filename_unselected["label"], ".csv")
        },
        content = function(file) {
            write.csv(data_diff(), file, row.names = FALSE)
        }
    )
    
    observeEvent(input$reset_input, {
        # reset filters based on specified defaults
        updateCheckboxGroupInput(session, "discbox", selected = filter_defaults[["discbox"]])
        updateNumericInput(session, "downloads", value = filter_defaults[["downloads"]])
        updateSliderInput(session, "snip", value = filter_defaults[["snip"]])
        updateRadioButtons(session, "pubedit", selected = filter_defaults[["pubedit"]])
        updateCheckboxGroupInput(session, "collbox", selected = filter_defaults[["collbox"]])
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
