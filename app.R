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
library(stringi)




default_background_color <- "#f5f5f2"


#default_background_color <- "#006994"


# Reading Data #------------------------------------------------------------
data <- read_excel("BBNJ_database_2022.xlsx")

data2 <- read_excel("BBNJ_database_2022.xlsx", sheet = "Assessments")
data3 <- read_excel("BBNJ_database_2022.xlsx", sheet = "BBNJ Draft Texts")

intro <- read_lines("intro.txt")
intro <- paste(intro, collapse = " ")

meth <- read_lines("metho.txt")
meth <- paste(meth, collapse = " ")

disclaimer <- read_lines("disclaimer.txt")
disclaimer <- paste(disclaimer, collapse = " ")

packages <- c("ABMTs/MPAs", "MGRs", "CB&TT", "EIAs", "Crosscutting", "Other")

### clean data 
names(data)[names(data) == "Thematic Group"] <- "theme"
names(data)[names(data) == "Package"] <- "topic"
names(data)[names(data) == "Package 2"] <- "Package2"


names(data)[names(data) == "Year"] <- "year"
names(data)[names(data) == "Region"] <- "region"
names(data)[names(data) == "Keywords (Author and plus)"] <- "Keywords"
#names(data)[names(data) == "show"] <- "Literature"

data$Literature <- paste0(data$show,"<p> Abstract: ", data$Abstract)

data$topic[data$topic == "general BBNJ"] <- "Crosscutting"
data$topic[data$topic == "BBNJ general"] <- "Crosscutting"
data$topic[data$topic == "crosscutting"] <- "Crosscutting"
data$topic[data$topic == "other"] <- "Other"

data$topic[data$topic == "MGRs (MSR)"] <- "MGRs"

data$topic[data$topic == "ABMTs"] <- "ABMTs/MPAs"
data$topic[data$topic == "AMBTs"] <- "ABMTs/MPAs"

data$topic[data$topic == "MPAs"] <- "ABMTs/MPAs"
data$topic[data$topic == "MPAs (undermining)"] <- "ABMTs/MPAs"

data$topic[data$topic == "ABMTS/MPAs"] <- "ABMTs/MPAs"
data$topic[data$topic == "ABMTs; MPAs"] <- "ABMTs/MPAs"
data$topic[data$topic == "MPAs/EIAs"] <- "EIAs/MPAs"

data$topic <- as.factor(data$topic)
data$topic <- ordered(data$topic, levels = c("MGRs", "ABMTs/MPAs", "EIAs", "CB&TT", "Crosscutting","EIAs/MPAs", "Other"))

data$Journal <- str_to_title(data$Journal)


data$Journal <- str_replace(data$Journal, "&", "And")    
data$Journal <- str_replace(data$Journal, "-", ": ")    

data$theme[data$theme == "other"] <- "Other"
data$theme[data$theme == "crosscutting"] <- "Crosscutting"
data$theme[data$theme == "fishing"] <- "fisheries"


data$theme[data$theme %in% packages] <- NA
#data$theme <- data$theme %>% replace_na("General BBNJ")
data$theme <- str_to_lower(data$theme)

#### prepare keywords list to select input from

data$Keywords <- str_to_lower(data$Keywords)
data$Keywords <- gsub("[\r\n]", " ", data$Keywords)
data$Keywords <- gsub(" - ", " ", data$Keywords)
data$Keywords <- gsub("-", " ", data$Keywords)

data$Keywords <- gsub(";", ",", data$Keywords)
data$Keywords <- gsub(":", ",", data$Keywords)
data$Keywords <- gsub(",", ", ", data$Keywords)

data$Keywords <- gsub("\\s*\\([^\\)]+\\)", "", as.character(data$Keywords))



data$Keywords <- gsub("\\baccess and benefit sharing\\b", "access and benefit sharing", data$Keywords )
data$Keywords <- gsub("\\baccess and  benefit sharing\\b", "access and benefit sharing", data$Keywords )
data$Keywords <- gsub("\\bbenefit-sharing\\b", "benefit sharing", data$Keywords )
data$Keywords <- gsub("\\bbenefit  sharing\\b", "benefit sharing", data$Keywords )

data$Keywords <- gsub("antarctica", "antarctic", data$Keywords )

data$Keywords <- gsub("antarctic treaty system (ats)", "antarctic treaty", data$Keywords )
data$Keywords <- gsub("antarctic treaty system", "antarctic treaty", data$Keywords )

data$Keywords <- gsub("2030 agenda andsdgs", "2030 agenda and sdgs", data$Keywords )

data$Keywords <- gsub("\\ba maritime power\\b", "maritime power", data$Keywords )

data$Keywords <- gsub("marine areas beyond national jurisdiction", "areas beyond national jurisdiction", data$Keywords )
data$Keywords <- gsub("areasbeyond national jurisdiction", "areas beyond national jurisdiction", data$Keywords )
data$Keywords <- gsub("abnj", "areas beyond national jurisdiction", data$Keywords )
data$Keywords <- gsub("areas beyond national jurisdiction areas beyond national jurisdiction", "areas beyond national jurisdiction", data$Keywords )

data$Keywords <- gsub("abmt", "area based management tools", data$Keywords )



#data$Keywords <- gsub("\\bbnj\\b", "biological diversity of areas beyond national jurisdiction (bbnj)", data$Keywords )
data$Keywords <- gsub("\\bbnj\\b", "bbnj", data$Keywords )

data$Keywords <- gsub("bbnj", "bbnj", data$Keywords )
data$Keywords <- gsub("biodiversity beyond national jurisdiction", "bbnj", data$Keywords )
data$Keywords <- gsub("biological diversity beyond national jurisdiction", "bbnj", data$Keywords )
data$Keywords <- gsub("marine biodiversity beyond national jursidiction", "bbnj", data$Keywords )

data$Keywords <- gsub("marine  biological  diversity", "marine biological diversity", data$Keywords )
data$Keywords <- gsub("marine biodiversity", "marine biological diversity", data$Keywords )

data$Keywords <- gsub("biodiversity  beyond national jurisdiction", "bbnj", data$Keywords )
data$Keywords <- gsub("biodiversity beyond areas of national jurisdiction", "bbnj", data$Keywords )
data$Keywords <- gsub("biodiversity beyond national  jurisdiction", "bbnj", data$Keywords )
data$Keywords <- gsub("biodiversity in areas beyond national jurisdiction", "bbnj", data$Keywords )
data$Keywords <- gsub("biological diversity of areas beyond national jurisdiction", "bbnj", data$Keywords )
data$Keywords <- gsub("biological diversity of areas beyond national jurisdiction agreement", "bbnj", data$Keywords )
data$Keywords <- gsub("marine biological diversity of areas beyond national jurisdiction", "bbnj", data$Keywords )

data$Keywords <- gsub("bbnj", "biological diversity of areas beyond national jurisdiction - bbnj", data$Keywords )
data$Keywords <- gsub("marine biological diversity of areas beyond national jurisdiction - bbnj", "biological diversity of areas beyond national jurisdiction - bbnj", data$Keywords )


data$Keywords <- gsub("common heritage of humankind", "common heritage of mankind", data$Keywords )

data$Keywords <- gsub("cbd", "convention on biological diversity", data$Keywords )
data$Keywords <- gsub(" convention on biological diversity", "convention on biological diversity", data$Keywords )
data$Keywords <- gsub("convention on biological  diversity", "convention on biological diversity", data$Keywords )


data$Keywords <- gsub("commission for the conservation of antarctic marine living resources", "ccamlr", data$Keywords )

data$Keywords <- gsub("\\bcoastal state\\b", "coastal states", data$Keywords )

data$Keywords <- gsub("deep seabed areas", "deep seabed area", data$Keywords )



data$Keywords <- gsub("ecologically or biologically significant area ebsa", "ecologically or biologically significant area", data$Keywords )
data$Keywords <- gsub("ecologically or biologically significant marine areas", "ecologically or biologically significant area", data$Keywords )
data$Keywords <- gsub("ebsas", "ecologically or biologically significant area", data$Keywords )
data$Keywords <- gsub("ebsa", "ecologically or biologically significant area", data$Keywords )
data$Keywords <- gsub("ecologically or biologically significant area", "ecologically or biologically significant areas", data$Keywords )
data$Keywords <- gsub("ecologically or biologically significant areass", "ecologically or biologically significant areas", data$Keywords )
data$Keywords <- gsub("ecologically or biologically significant areas", "ecologically or biologically significant areas - ebsas", data$Keywords )

data$Keywords <- gsub("economic exclusive zone", "exclusive economic zone", data$Keywords )

data$Keywords <- gsub("\\benvironmental impact assessments\\b", "environmental impact assessment", data$Keywords )
data$Keywords <- gsub("\\benvironmental impact assessment\\b", "environmental impact assessments", data$Keywords )
data$Keywords <- gsub("\\beia\\b", "environmental impact assessments", data$Keywords )

data$Keywords <- gsub("\\bmarine protected areas (mpas)\\b", "marine protected area", data$Keywords )
data$Keywords <- gsub("\\bmarine protected areas\\b", "marine protected area", data$Keywords )
data$Keywords <- gsub("\\bmarine protected area\\b", "marine protected areas", data$Keywords )
data$Keywords <- gsub("\\bmpa\\b", "marine protected areas", data$Keywords )
data$Keywords <- gsub("\\bmpas\\b", "marine protected areas", data$Keywords )

data$Keywords <- gsub(" high seas", "high seas", data$Keywords )

data$Keywords <- gsub("ecosystem  approach to fisheries management eafm", "ecosystem approach to fisheries management", data$Keywords )


data$Keywords <- gsub("\\binternational legally binding instrument\\b", "ilbi", data$Keywords )
data$Keywords <- gsub("\\binternational legally binding instrument (ilbi)\\b", "ilbi", data$Keywords)
data$Keywords <- gsub("\\bilbi\\b", "internationally legally binding instrument - ilbi", data$Keywords)


data$Keywords <- gsub("\\bitpgrfa\\b", "international treaty for plant genetic resources for food and agriculture", data$Keywords )


data$Keywords <- gsub("\\binternational  instrument\\b", "international instrument", data$Keywords )
data$Keywords <- gsub("internationalenvironmental law", "international environmental law", data$Keywords )

data$Keywords <- gsub("\\bknowledge  transfer\\b", "knowledge transfer", data$Keywords )

data$Keywords <- gsub("\\bkeywords plus\\b", "", data$Keywords )

data$Keywords <- gsub("international united nations", "united nations", data$Keywords)
data$Keywords <- gsub("international law of the sea ", "unclos", data$Keywords)
data$Keywords <- gsub("law of the sea convention (losc)", "unclos", data$Keywords)
data$Keywords <- gsub("united nations convention on the law of the sea (losc)", "unclos", data$Keywords)
data$Keywords <- gsub("united nations convention on the law of the sea (unclos) governance frameworks", "unclos", data$Keywords)
data$Keywords <- gsub("united nations convention on the law of the sea (unclos) system", "unclos", data$Keywords)
data$Keywords <- gsub("law of the sea  ", "unclos", data$Keywords)
data$Keywords <- gsub("law of the sea (unclos)", "unclos", data$Keywords)
data$Keywords <- gsub("law of the sea convention", "unclos", data$Keywords)
data$Keywords <- gsub("law of the sea convention (losc)", "unclos", data$Keywords)
data$Keywords <- gsub("law of thesea convention", "unclos", data$Keywords)
data$Keywords <- gsub("law of thesea convention", "unclos", data$Keywords)
data$Keywords <- gsub("unclos system", "unclos", data$Keywords)
data$Keywords <- gsub("unclos iii", "unclos", data$Keywords)
data$Keywords <- gsub("united nations convention on the law of the sea", "unclos", data$Keywords)

data$Keywords <- gsub("losc", "unclos", data$Keywords)

data$Keywords <- gsub("unclos", "united nations convention on the law of the sea - unclos", data$Keywords )
data$Keywords <- gsub("international united nations convention on the law of the sea (unclos) ", "united nations convention on the law of the sea - unclos", data$Keywords )

data$Keywords <- gsub("implementation agreement", "implementing agreement", data$Keywords )

data$Keywords <- gsub("\\bocean   governance\\b", "ocean governance", data$Keywords )
data$Keywords <- gsub("\\boceans governance\\b", "ocean governance", data$Keywords )

data$Keywords <- gsub("\\bospar commission\\b", "ospar", data$Keywords )

data$Keywords <- gsub("\\boutstanding  universal value\\b", "outstanding universal value", data$Keywords )
data$Keywords <- gsub("\\bperspectiveareas\\b", "perspective areas", data$Keywords )

data$Keywords <- gsub("\\b iii\\b", "", data$Keywords )

data$Keywords <- gsub("\\bmarine genetic resources (mgrs)\\b", "marine genetic resources", data$Keywords )
data$Keywords <- gsub("\\bmgr\\b", "marine genetic resources", data$Keywords )
data$Keywords <- gsub("\\bmarine genetic resources\\b", "marine genetic resources - mgrs", data$Keywords )


data$Keywords <- gsub("\\bmarine spatial planning msp\\b", "marine spatial planning", data$Keywords )

data$Keywords <- gsub("\\brfmo\\b", "regional fisheries management organizations", data$Keywords )


data$Keywords <- gsub("rio + 20", "rio+20", data$Keywords )

data$Keywords <- gsub("\\bscience policy interfaces\\b", "science policy interface", data$Keywords )

data$Keywords <- gsub("\\bseabirds\\b", "seabird", data$Keywords )
data$Keywords <- gsub("\\bseabird\\b", "seabirds", data$Keywords )

data$Keywords <- gsub("\\bseamounts\\b", "seamount", data$Keywords )
data$Keywords <- gsub("\\bseamount\\b", "seamounts", data$Keywords )

data$Keywords <- gsub("\\bsustainable  use.\\b", "sustainable use", data$Keywords )

data$Keywords <- gsub("\\b technology transfer\\b", "technology transfer", data$Keywords )

data$Keywords <- gsub("\\b(wio)\\b", "western indian ocean", data$Keywords)

data$Keywords[is.na(data$Keywords)]  <- "-"


words <- data %>% filter(!is.na(Keywords)) %>%
    select(Keywords) %>% 
    as.list()


words <- words$Keywords %>% str_split(", ") %>%
    unlist()

words <- trimws(words, which = c("left"))
words <- trimws(words, which = c("right"))

words <- stri_enc_toutf8(words)


words <- words %>% unique() %>% 
  sort()


  


### add "all" rows 
#data <- data %>% add_row(topic = "All")

    
#----------------------------------------------------

manual <- "This MARIPOLDATA BBNJ Governance Database 
serves to give an overview about relevant BBNJ literature." 

erc <- "<p><p> This database is part of the MARIPOLDATA project that has received funding from the European Research Council (ERC) 
under the European Union's Horizon 2020 research and innovation programme (grant agreement No 804599)."

version <- paste0(disclaimer, 
                  erc,
                  "<p> Version 1, last updated 29.07.2021")



# Page Layout #-------------------------------------------------------------
ui <- fluidPage(
    setBackgroundColor(default_background_color),
    sidebarLayout(
        sidebarPanel(
            tags$a(href="https://www.maripoldata.eu",
                   tags$img(src='maripol2.png', height='120', width='120')),
            tags$a(href="https://politikwissenschaft.univie.ac.at/",
                   tags$img(src='Politikwissenschaft_en_4cc.png', height='120', width='320')),
            
            titlePanel('BBNJ Governance Literature Database'),
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
            
            # selectInput(inputId = "region",
            #             label = "Choose a region (multiple possible):",
            #             #        selected = "",
            #             multiple = TRUE, 
            #             # selectize = FALSE,
            #             choices = c(sort(unique(data$region)))),
            
            
            ### if region is not selected
            #input$region <- c(),
            
            
            selectInput(inputId = "words",
                        label = "Choose Keywords (multiple possible):",
                        #        selected = "",
                        multiple = TRUE,
                        # selectize = FALSE,
                        choices = c(sort(unique(words)))),
            
            tabPanel("ERC",
                     tags$a(href="https://erc.europa.eu/",
                            tags$img(src="Logo_E2.png", height='120', width='260'))
                     #,textOutput(outputId = "erc") 
                     ),
            tabPanel("Version",
                     htmlOutput(outputId = "version"))

        ),
        mainPanel(navbarPage(title = "",
                             tabPanel(h4("About"),
                                      htmlOutput("intro")),
                             tabPanel(h4("Methodology"),
                                      htmlOutput("meth")),
                             tabPanel(h4("BBNJ Governance Literature Database"),
                                      tabsetPanel(
                                          tabPanel("All",
                                                   dataTableOutput(outputId = "all")),
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
                                      )),
                             tabPanel(h4("Global Assessments & Reports"),
                                      dataTableOutput(outputId = "ass")),
                             tabPanel(h4("BBNJ Draft Texts"),
                                      dataTableOutput(outputId = "drafts")),
                             tabPanel(h4("Statistics"),
                                      tabsetPanel(
                                          tabPanel("Thematic Distribution Element",
                                                   plotOutput(outputId = "pack")),
                                          tabPanel("Regional Distribution", 
                                             plotOutput(outputId = "geo")),
                                        tabPanel("Yearly Distribution",
                                             plotOutput(outputId = "time"))
                                        ,
                                        tabPanel("Source (Journal)",
                                                 tableOutput(outputId = "journal"))
                                      ))
        ))
    ))




server <- function(input, output) {
    
    
        # Dashboard General Description
    output$manual <- renderText({
        HTML(manual)
    })
    
    # ERC Header
    # output$erc <- renderText({
    #     HTML(erc)
    # })
    
    # Version Header
    output$version <- renderText({
        HTML(version)
    })
    
    output$intro <-renderUI({
        HTML(intro)
    })
    output$meth <-renderUI({
        HTML(meth)
    })

    
#selection_words <- "conservation"    

       
    output$all <- renderDataTable({
      selection_words <- paste(input$words,collapse = '|')
        my_table <- (
          if(((length(input$year) == 0)) & ((length(input$region) == 0)) & 
             is.null(selection_words)) {
            data %>% 
              select(Literature)
          } else if((length(input$region) == 0) & (length(input$year) == 0)) {
            data %>%
              dplyr::filter((str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
              select(Literature)    
          } else if((length(input$region) == 0) & (!is.null(selection_words))) {
            data %>%
              dplyr::filter((year %in% input$year) &
                              (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
              select(Literature)
          } else if((length(input$region) == 0) & (is.null(selection_words))) {
            data %>%
              dplyr::filter((year %in% input$year)) %>% 
              select(Literature)
          } else if((length(input$year) == 0) & (!is.null(selection_words))) {
            data %>%
              dplyr::filter((region %in% input$region) & 
                              (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
              select(Literature)
          } else if((length(input$year) == 0) & (is.null(selection_words))) {
            data %>%
              dplyr::filter((region %in% input$region)) %>% 
              select(Literature)
          } else if ((length(selection_words) == 0)) {
            data %>%
              dplyr::filter((region %in% input$region) & 
                              (year %in% input$year)) %>% 
              select(Literature)
          } else { 
            data %>% 
              dplyr::filter((year %in% input$year) & (region %in% input$region) &
                              (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
              select(Literature)})
        
        return(my_table)
        
    }, escape = FALSE)
    
    
    
    
    output$mgr <- renderDataTable({
      selection_words <- paste(input$words,collapse = '|')
      
        my_table <- (
            if(((length(input$year) == 0)) & ((length(input$region) == 0)) & 
               length(selection_words) == 0) {
                data %>% 
                    dplyr::filter(topic == "MGRs" | Package2 == "MGRs") %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (length(input$year) == 0)) {
                data %>%
                    dplyr::filter((topic == "MGRs") &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)    
            } else if((length(input$region) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "MGRs") & (year %in% input$year) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "MGRs") & (year %in% input$year)) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "MGRs") & (region %in% input$region) & 
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "MGRs") & (region %in% input$region)) %>% 
                    select(Literature)
            } else if ((length(selection_words) == 0)) {
                data %>%
                    dplyr::filter((topic == "MGRs") & (region %in% input$region) & 
                                      (year %in% input$year)) %>% 
                    select(Literature)
            } else { 
                data %>% 
                    dplyr::filter((topic == "MGRs") & (year %in% input$year) & (region %in% input$region) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)}
        )
        return(my_table)
        
    }, escape = FALSE)
    
    
    
    
    
    output$abmt <- renderDataTable({
      selection_words <- paste(input$words,collapse = '|')
      
        my_table <- (
            if(((length(input$year) == 0)) & ((length(input$region) == 0)) & 
               length(selection_words) == 0) {
                data %>% 
                    dplyr::filter(topic == "ABMTs/MPAs" | Package2 == "ABMTs/MPAs") %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (length(input$year) == 0)) {
                data %>%
                    dplyr::filter((topic == "ABMTs/MPAs") &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)    
            } else if((length(input$region) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "ABMTs/MPAs") & (year %in% input$year) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "ABMTs/MPAs") & (year %in% input$year)) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "ABMTs/MPAs") & (region %in% input$region) & 
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "ABMTs/MPAs") & (region %in% input$region)) %>% 
                    select(Literature)
            } else if ((length(selection_words) == 0)) {
                data %>%
                    dplyr::filter((topic == "ABMTs/MPAs") & (region %in% input$region) & 
                                      (year %in% input$year)) %>% 
                    select(Literature)
            } else { 
                data %>% 
                    dplyr::filter((topic == "ABMTs/MPAs") & (year %in% input$year) & (region %in% input$region) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)}
        )
        return(my_table)
        
        }, escape = FALSE)
    
    
    
    
    output$eia <- renderDataTable({
      selection_words <- paste(input$words,collapse = '|')
      
        my_table <- (
            if(((length(input$year) == 0)) & ((length(input$region) == 0)) & 
               length(selection_words) == 0) {
                data %>% 
                    dplyr::filter(topic == "EIAs" | Package2 == "EIAs") %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (length(input$year) == 0)) {
                data %>%
                    dplyr::filter((topic == "EIAs") &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)    
            } else if((length(input$region) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "EIAs") & (year %in% input$year) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "EIAs") & (year %in% input$year)) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "EIAs") & (region %in% input$region) & 
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "EIAs") & (region %in% input$region)) %>% 
                    select(Literature)
            } else if ((length(selection_words) == 0)) {
                data %>%
                    dplyr::filter((topic == "EIAs") & (region %in% input$region) & 
                                      (year %in% input$year)) %>% 
                    select(Literature)
            } else { 
                data %>% 
                    dplyr::filter((topic == "EIAs") & (year %in% input$year) & (region %in% input$region) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)}
        )
        return(my_table)
        
    }, escape = FALSE)
    
    
    
    
    
    output$cbtt <- renderDataTable({
      selection_words <- paste(input$words,collapse = '|')
      
        my_table <- (
            if(((length(input$year) == 0)) & ((length(input$region) == 0)) & 
               length(selection_words) == 0) {
                data %>% 
                    dplyr::filter(topic == "CB&TT" | Package2 == "CB&TT") %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (length(input$year) == 0)) {
                data %>%
                    dplyr::filter((topic == "CB&TT") &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)    
            } else if((length(input$region) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "CB&TT") & (year %in% input$year) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "CB&TT") & (year %in% input$year)) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "CB&TT") & (region %in% input$region) & 
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "CB&TT") & (region %in% input$region)) %>% 
                    select(Literature)
            } else if ((length(selection_words) == 0)) {
                data %>%
                    dplyr::filter((topic == "CB&TT") & (region %in% input$region) & 
                                      (year %in% input$year)) %>% 
                    select(Literature)
            } else { 
                data %>% 
                    dplyr::filter((topic == "CB&TT") & (year %in% input$year) & (region %in% input$region) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)}
        )
        return(my_table)
        
    }, escape = FALSE)
    
    
   
    
    output$cc <- renderDataTable({
      selection_words <- paste(input$words,collapse = '|')
      
        my_table <- (
            if(((length(input$year) == 0)) & ((length(input$region) == 0)) & 
               length(selection_words) == 0) {
                data %>% 
                    dplyr::filter(topic == "Crosscutting" | Package2 == "Crosscutting") %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (length(input$year) == 0)) {
                data %>%
                    dplyr::filter((topic == "Crosscutting") &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)    
            } else if((length(input$region) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "Crosscutting") & (year %in% input$year) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "Crosscutting") & (year %in% input$year)) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "Crosscutting") & (region %in% input$region) & 
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "Crosscutting") & (region %in% input$region)) %>% 
                    select(Literature)
            } else if ((length(selection_words) == 0)) {
                data %>%
                    dplyr::filter((topic == "Crosscutting") & (region %in% input$region) & 
                                      (year %in% input$year)) %>% 
                    select(Literature)
            } else { 
                data %>% 
                    dplyr::filter((topic == "Crosscutting") & (year %in% input$year) & (region %in% input$region) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)}
        )
        return(my_table)
        
    }, escape = FALSE)
    
    
    
    
    
    output$other <- renderDataTable({
      selection_words <- paste(input$words,collapse = '|')
      
        my_table <- (
            if(((length(input$year) == 0)) & ((length(input$region) == 0)) & 
               length(selection_words) == 0) {
                data %>% 
                    dplyr::filter(topic == "Other" | Package2 == "other") %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (length(input$year) == 0)) {
                data %>%
                    dplyr::filter((topic == "Other") &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)    
            } else if((length(input$region) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "Other") & (year %in% input$year) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$region) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "Other") & (year %in% input$year)) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (!is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "Other") & (region %in% input$region) & 
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)
            } else if((length(input$year) == 0) & (is.null(selection_words))) {
                data %>%
                    dplyr::filter((topic == "Other") & (region %in% input$region)) %>% 
                    select(Literature)
            } else if ((length(selection_words) == 0)) {
                data %>%
                    dplyr::filter((topic == "Other") & (region %in% input$region) & 
                                      (year %in% input$year)) %>% 
                    select(Literature)
            } else { 
                data %>% 
                    dplyr::filter((topic == "Other") & (year %in% input$year) & (region %in% input$region) &
                                      (str_detect(str_split(data$Keywords, ", "), selection_words))) %>% 
                    select(Literature)}
        )
        return(my_table)
        
    }, escape = FALSE)
    
    output$ass <- renderDataTable({
        my_table <- (
            data2 %>% select(show)
        )
        return(my_table)
    }, escape = FALSE)
    
    output$drafts <- renderDataTable({
      my_table <- (
        data3 %>% select(show)
      )
      return(my_table)
    }, escape = FALSE) 
  
    output$pack <- renderPlot(
     # breaks <- c("MGRs", "ABMTs/MPAs", "EIAs", "CB&TT", "Crosscutting", "Other")
     #labels <- c("MGRs", "ABMTs/MPAs", "EIAs", "CB&TT", "Crosscutting", "Other")
        data %>% 
            #filter(topic %in% c("ABMTs/MPAs", "MGRs", "CB&TT", "EIAs", "Crosscutting", "Other")) %>%
            count(topic, theme) %>%
            ggplot(aes(x = topic, y =n, stat = 'identity', fill = theme)) +
            geom_bar(stat = "identity") +
            ggtitle("Distribution by Package Element & Specific Topic") +
            theme_tufte() +
            theme(
                plot.title = element_text(size = 20),
                axis.title.x = element_blank(),
                axis.text.x = element_text(size = 15, angle = 90),
                axis.text.y = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                
                legend.text = element_text(size = 13),
                plot.background = element_rect(fill = default_background_color,
                                               color = NA),
                panel.background = element_rect(fill = default_background_color,
                                                color = NA),
                legend.background = element_rect(fill = default_background_color,
                                                 color = NA)) +
            ylab("Number of Publications in Database")
        
    )
    
    output$geo <- renderPlot(
        data %>% count(region) %>%
            ggplot() +
            geom_bar(aes(x = region, y =n), stat = 'identity', fill = 'steelblue') +
            ggtitle("Regional Distribution of Articles") +
            theme_tufte() +
            theme(
                plot.title = element_text(size = 20),
                axis.title.x = element_blank(),
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                legend.text = element_text(size = 13),
                plot.background = element_rect(fill = default_background_color,
                                               color = NA),
                panel.background = element_rect(fill = default_background_color,
                                                color = NA),
                legend.background = element_rect(fill = default_background_color,
                                                 color = NA)) +
            ylab("Number of Publications in Database")
            
    )
    
    output$time <- renderPlot(
        data %>% count(year) %>%
            ggplot() +
            geom_bar(aes(x = year, y =n), stat = 'identity', fill = 'steelblue') +
            ggtitle("Year of Publication") +
            theme_tufte() +
            theme(
                plot.title = element_text(size = 20),
                axis.title.x = element_blank(),
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                legend.text = element_text(size = 13),
                plot.background = element_rect(fill = default_background_color,
                                               color = NA),
                panel.background = element_rect(fill = default_background_color,
                                                color = NA),
                legend.background = element_rect(fill = default_background_color,
                                                 color = NA)) +
            ylab("Number of Publications in Database")
        
    )
    

    output$journal <- renderTable(
        data %>% 
            count(Journal) %>% 
            arrange(desc(n)) %>%
            transmute("Source (Journal)" = Journal, "Number of Publications in Database" = n) 
    )
    
}
# Run the application 
shinyApp(ui = ui, server = server)
