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
            
            titlePanel('Marine Biodiversity Country Dashboard'),
            tags$a(href="https://www.un.org/bbnj/",
                   "Link to the Biodiversity Beyond National Jurisdiction (BBNJ) negotiations."),
            tabPanel("Manual",
                     textOutput(outputId = "manual")),
            selectInput(inputId = "topic",
                        label = "Choose a Thematic Group",
                        selected = "general BBNJ",
                        choices = c("general BBNJ", sort(unique(data$`Thematic Group`)))),
            tabPanel("ERC",
                     tags$a(href="https://erc.europa.eu/",
                            tags$img(src='Logo_E.png', height='120', width='260')),
                     textOutput(outputId = "erc")),
            tabPanel("Version",
                     textOutput(outputId = "version")),
        ),
        mainPanel(navbarPage(title = "",
                             
                             tabPanel(h4("BBNJ Literature Overview"),
                                      tabsetPanel(
                                          tabPanel("List of Publications",
                                                   htmlOutput(outputId = "title"))
                                      ))
        ))
    ))




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$title <- renderText({
        data %>% filter(`Thematic Group` == "input$topic") %>% 
            select(Source) %>% 
            print()
    })
    
    
}
# Run the application 
shinyApp(ui = ui, server = server)
