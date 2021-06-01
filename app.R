#####################################
### Petro Tolochko & Arne Langlet ###
###        ERC Maripoldata        ###
#####################################

### Auto Install Required Packages ###
# list.of.packages <- c("tidyverse", "ggthemes", "tidygraph",
#                       "shinyWidgets", "DescTools",
#                       "scales", "shiny", "igraph", "ggthemes",
#                       "widyr", "visNetwork", "RColorBrewer", "tidytext", "countrycode", "shinythemes)
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)>0) {install.packages(new.packages)}

require(tidyverse)
require(ggthemes)
require(tidygraph)
library(shinyWidgets)
require(scales)
library(igraph)
library(ggraph)
require(widyr)
require(visNetwork)
require(tidytext)
require(RColorBrewer)
require(DescTools)
require(shiny)
require(countrycode)
library(readxl)
library(cowplot)
require(gridExtra)
require(shinythemes)
require(kableExtra)
library(png)
library(openxlsx)



default_background_color <- "#f5f5f2"

# Reading Data #------------------------------------------------------------
data <- read_excel("BBNJ_database_2021.xlsx")

### clean data 
names(data)[names(data) == "Thematic Group"] <- "topic"
names(data)[names(data) == "Year"] <- "year"
names(data)[names(data) == "Region"] <- "region"
names(data)[names(data) == "Keywords (Author and plus)"] <- "Keywords"


#### prepare keywords list to select input from

words <- data %>% filter(!is.na(Keywords)) %>%
    select(Keywords) %>% 
    as.list()

words$Keywords <- gsub(";", ",", words$Keywords)


words <- words$Keywords %>% str_split(", ") %>% 
    unlist()




### add "all" rows 
#data <- data %>% add_row(topic = "All")

    
#----------------------------------------------------

manual <- "This MARIPOLDATA Marine Biodiversity Literature Dashboard 
serves to give an overview about relevant BBNJ literature." 

erc <- "This Dashboard is part of the MARIPOLDATA project that has received funding from the European Research Council (ERC) 
under the European Union's Horizon 2020 research and innovation programme (grant agreement No 804599)."

version <- paste0("Version 1, last updated: ", Sys.Date())



# Page Layout #-------------------------------------------------------------
ui <- fluidPage(
    setBackgroundColor(default_background_color),
    sidebarLayout(
        sidebarPanel(
            tags$a(href="https://www.maripoldata.eu",
                   tags$img(src='maripol.png', height='120', width='150')),
            tags$a(href="https://politikwissenschaft.univie.ac.at/",
                   tags$img(src='Politikwissenschaft_en_4c.png', height='120', width='360')),
            
            titlePanel('Biodiversity Beyond National Jurisdiction Literature Dashboard'),
            tags$a(href="https://www.un.org/bbnj/",
                   "Link to the Biodiversity Beyond National Jurisdiction (BBNJ) negotiations."),
            tabPanel("Manual",
                     textOutput(outputId = "manual")),
            # selectInput(inputId = "topic",
            #             label = "Choose a topic (multiple possible):",
            #        #     selected = "general BBNJ",
            #             multiple = TRUE,
            #             choices = c(sort(unique(data$topic)))),
            selectInput(inputId = "year",
                        label = "Choose a year (multiple possible):",
                #        selected = "",
                        multiple = TRUE, 
                # selectize = FALSE,
                        choices = c(sort(unique(data$year)))),
            
            selectInput(inputId = "region",
                        label = "Choose a region (multiple possible):",
                        #        selected = "",
                        multiple = TRUE, 
                        # selectize = FALSE,
                        choices = c(sort(unique(data$region)))),
            
            selectInput(inputId = "words",
                        label = "Choose Keywords (multiple possible):",
                        #        selected = "",
                        multiple = TRUE,
                        # selectize = FALSE,
                        choices = c(sort(unique(words)))),
            
            tabPanel("ERC",
                     tags$a(href="https://erc.europa.eu/",
                            tags$img(src='Logo_E.png', height='120', width='260')),
                     textOutput(outputId = "erc")),
            tabPanel("Version",
                     textOutput(outputId = "version"))
        ),
        mainPanel(navbarPage(title = "",
                             
                             tabPanel(h4("BBNJ Literature Overview"),
                                      tabsetPanel(
                                          tabPanel("MGRs",
                                                   dataTableOutput(outputId = "mgr")),
                                              tabPanel("ABMTs/MPAs",
                                                       dataTableOutput(outputId = "abmt")),
                                                       tabPanel("EIAs",
                                                                dataTableOutput(outputId = "eia")),
                                                                tabPanel("CB&TT",
                                                                         dataTableOutput(outputId = "cbtt")),
                                                                         tabPanel("Crosscutting",
                                                                                  dataTableOutput(outputId = "cc")),
                                          tabPanel("Other",
                                                   dataTableOutput(outputId = "other"))
                                      ))
        ))
    ))




server <- function(input, output) {
    # Dashboard General Description
    output$manual <- renderText({
        HTML(manual)
    })
    
    # ERC Header
    output$erc <- renderText({
        HTML(erc)
    })
    
    # Version Header
    output$version <- renderText({
        HTML(version)
    })
    
    
    ########################## keywords section to be finished
    
    # words2 <- data %>% filter(!is.na(Keywords)) %>%
    #     select(Keywords) %>%
    #     as.list()
    # 
    # input <- words2$Keywords %>% str_split(", ")
    # 
    # 
    # y <- for (i in input){ for (j in i) {print(j %in% input$words)}}
    ############################
    
    output$mgr <- renderDataTable({
       my_table <- if((length(input$year) == 0) & (length(input$region) == 0)) {
            data %>% 
                dplyr::filter(topic == "MGRs") %>% 
                select(show) %>% 
               print(HTML())
        } else if((length(input$region) == 0)) {
            data %>%
                dplyr::filter(topic == "MGRs" & year %in% input$year) %>% 
                select(show) 
        } else if((length(input$year) == 0)) {
            data %>%
                dplyr::filter(topic == "MGRs" & region %in% input$region) %>% 
                select(show)
        } else if((length(input$words) == 0)) {
            data %>%
                dplyr::filter(topic == "MGRs" & region %in% input$region & year %in% input$year) %>% 
                select(show) 
        } else {
        data %>% 
            dplyr::filter(topic == "MGRs" & year %in% input$year & region %in% input$region) %>% 
            select(show) }
       return(my_table)
        }, escape = FALSE)

    output$abmt <- renderDataTable({
        my_table <- (
            if(((length(input$year) == 0)) & ((length(input$region) == 0))) {
                data %>% 
                    dplyr::filter(topic == "ABMTs/MPAs") %>% 
                    select(show)
            } else if((length(input$region) == 0)) {
                data %>%
                    dplyr::filter((topic == "ABMTs/MPAs") & (year %in% input$year)) %>% 
                    select(show)
            } else if((length(input$year) == 0)) {
                data %>%
                    dplyr::filter((topic == "ABMTs/MPAs") & (region %in% input$region)) %>% 
                    select(show)
            } else { 
                data %>% 
                    dplyr::filter((topic == "ABMTs/MPAs") & (year %in% input$year) & (region %in% input$region)) %>% 
                    select(show)}
        )
        return(my_table)
        
        }, escape = FALSE)
    
    output$eia <- renderDataTable({
        my_table <- if((length(input$year) == 0) & (length(input$region) == 0)) {
            data %>%
                dplyr::filter(topic == "EIAs") %>%
                select(show)
        } else if((length(input$region) == 0)) {
            data %>%
                dplyr::filter(topic == "EIAs" & year %in% input$year) %>%
                select(show)
        } else if((length(input$year) == 0)) {
            data %>%
                dplyr::filter(topic == "EIAs" & region %in% input$region) %>%
                select(show)
        } else {
            data %>% 
                dplyr::filter(topic == "EIAs" & year %in% input$year & region %in% input$region) %>% 
                select(show) 
        } 
        return(my_table)
        }, escape = FALSE)
    
    output$cbtt <- renderDataTable({
        my_table <- if((length(input$year) == 0) & (length(input$region) == 0)) {
            data %>% 
                dplyr::filter(topic == "CB&TT") %>% 
                select(show)
        }
        else if((length(input$region) == 0)) {
            data %>%
                dplyr::filter(topic == "CB&TT" & year %in% input$year) %>% 
                select(show)
        }
        else if((length(input$year) == 0)) {
            data %>%
                dplyr::filter(topic == "CB&TT" & region %in% input$region) %>% 
                select(show)
        }
        
        else {
            data %>% 
                dplyr::filter(topic == "CB&TT" & year %in% input$year & region %in% input$region) %>% 
                select(show) 
            }
        return(my_table)
        }, escape = FALSE)
   
    
     output$cc <- renderDataTable({
        my_table <- if((length(input$year) == 0) & (length(input$region) == 0)) {
            data %>% 
                dplyr::filter(topic == "Crosscutting") %>% 
                select(show)
        }
        else if((length(input$region) == 0)) {
            data %>%
                dplyr::filter(topic == "Crosscutting" & year %in% input$year) %>% 
                select(show)
        }
        else if((length(input$year) == 0)) {
            data %>%
                dplyr::filter(topic == "Crosscutting" & region %in% input$region) %>% 
                select(show)
        }
        
        else {
            data %>% 
                dplyr::filter(topic == "Crosscutting" & year %in% input$year & region %in% input$region) %>% 
                select(show) 
        }
        return(my_table)
        }, escape = FALSE)
    
    output$other <- renderDataTable({
        my_table <- if((length(input$year) == 0) & (length(input$region) == 0)) {
            data %>% 
                dplyr::filter(topic == "other") %>% 
                select(show)
        }
        else if((length(input$region) == 0)) {
            data %>%
                dplyr::filter(topic == "other" & year %in% input$year) %>% 
                select(show)
        }
        else if((length(input$year) == 0)) {
            data %>%
                dplyr::filter(topic == "other" & region %in% input$region) %>% 
                select(show)
        }
        
        else {
            data %>% 
                dplyr::filter(topic == "other" & year %in% input$year & region %in% input$region) %>% 
                select(show) 
        }
        return(my_table)
        },escape = FALSE)
    
}
# Run the application 
shinyApp(ui = ui, server = server)
