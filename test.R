





input$words <- c("access", "marine genetic resources")
input$words <- paste(input$words,collapse = '|')


(data$Keywords %in% input$words)

  my_table <- (
    if(((length(input$year) == 0)) & ((length(input$region) == 0)) & 
       length(input$words) == 0) {
      data %>% 
        select(Literature)
    } else if((length(input$region) == 0) & (length(input$year) == 0)) {
      data %>%
        dplyr::filter(
          (str_detect(str_split(data$Keywords, ", "), input$words))
          ) %>% 
        select(Literature)    
    } else if((length(input$region) == 0) & (!is.null(input$words))) {
      data %>%
        dplyr::filter((year %in% input$year) &
                        (str_detect(str_split(data$Keywords, ", "), input$words))) %>% 
        select(Literature)
    } else if((length(input$region) == 0) & (is.null(input$words))) {
      data %>%
        dplyr::filter((year %in% input$year)) %>% 
        select(Literature)
    } else if((length(input$year) == 0) & (!is.null(input$words))) {
      data %>%
        dplyr::filter((region %in% input$region) & 
                        (str_detect(str_split(data$Keywords, ", "), input$words))) %>% 
        select(Literature)
    } else if((length(input$year) == 0) & (is.null(input$words))) {
      data %>%
        dplyr::filter((region %in% input$region)) %>% 
        select(Literature)
    } else if ((length(input$words) == 0)) {
      data %>%
        dplyr::filter((region %in% input$region) & 
                        (year %in% input$year)) %>% 
        select(Literature)
    } else { 
      data %>% 
        dplyr::filter((year %in% input$year) & (region %in% input$region) &
                        (str_detect(str_split(data$Keywords, ", "), input$words))) %>% 
        select(Literature)}
  )

my_table
  

#data$Keywords <- gsub(" (sdgs)", "", data$Keywords )
# data$Keywords <- gsub(" (msr)", "", data$Keywords )
# data$Keywords <- gsub(" (mpas)", "", data$Keywords )
# data$Keywords <- gsub(" (mgr)", "", data$Keywords )
# data$Keywords <- gsub(" (mgrs)", "", data$Keywords )
# data$Keywords <- gsub(" (ilbi)", "", data$Keywords )
# data$Keywords <- gsub(" (abmt)", "", data$Keywords )
# data$Keywords <- gsub(" (abmts)", "", data$Keywords )
# 
# data$Keywords <- gsub(" (abnj)", "", data$Keywords )
# data$Keywords <- gsub(" (bbnj)", "", data$Keywords )
# data$Keywords <- gsub(" (cbd)", "", data$Keywords )
# data$Keywords <- gsub(" (ebsas)", "", data$Keywords )
# data$Keywords <- gsub(" (eez)", "", data$Keywords )
# data$Keywords <- gsub("eias", "", data$Keywords )
# data$Keywords <- gsub("eia", "", data$Keywords )
# data$Keywords <- gsub("()", "", data$Keywords )


# 
# data$Keywords <- gsub("abmt", "area-based management tools", data$Keywords )
# 
# data$Keywords <- gsub("area-based management tools (abmts)", "area-based management tools", data$Keywords )
# data$Keywords <- gsub("area-based management tools (abmt)", "area-based management tools", data$Keywords )
# data$Keywords <- gsub("area-based management tools (area-based management tools)", "area-based management tools", data$Keywords )
# 
# 
# words[words=="access and  benefit-sharing (abs)"]<-"access and benefit sharing"
# words[words=="access and benefit  sharing (abs)"]<-"access and benefit sharing"
# words[words=="access and benefit sharing "]<-"access and benefit sharing"
# 
# 
# words[words=="antarctic treaty"]<-"antarctic treaty system"
# words[words=="capacity building"]<-"capacity building "
# 
# words[words=="benefit-sharing"]<-"access and benefit sharing"
# words[words=="benefit sharing"]<-"access and benefit sharing"
# words[words=="bbnj"]<-"biological diversity of areas beyond national jurisdiction (bbnj)"
# words[words=="united nations convention on the law of the sea (unclos) (losc)"]<-"united nations convention on the law of the sea (unclos)"
# words[words=="united nations convention on the law of the sea (locs)"]<-"united nations convention on the law of the sea (unclos)"
# words[words=="united nations convention on the law of the sea"]<-"united nations convention on the law of the sea (unclos)"
# 
# words[words=="antarctic treaty system (ats)"]<-"antarctic treaty system"
# words[words=="biodiversity  beyond national jurisdiction"]<-"biological diversity of areas beyond national jurisdiction (bbnj)"
# words[words=="biodiversity beyond national  jurisdiction"]<-"biological diversity of areas beyond national jurisdiction (bbnj)"
# words[words=="biological diversity of areas beyond national jurisdiction (bbnj) (bbnj)"]<-"biological diversity of areas beyond national jurisdiction (bbnj)"
# 
# words[words=="united nations convention on the law of the sea (losc)"]<-"united nations convention on the law of the sea (unclos)"
# words[words=="access and bene???t sharing"]<-"access and benefit sharing"
# words[words=="convention on biological  diversity"]<-"convention on biological diversity"
# words[words=="access and bene???t sharing" ]<-"access and benefit sharing"  
# 
# words[words=="area-based management tools (area-based management tools)" ]<-"area-based management tools"  
# words[words=="areas beyond national jurisdiction ()" ]<-"areas beyond national jurisdiction"  
# words[words=="convention on biological diversity (cbd)" ]<-"convention on biological diversity"  
# words[words=="ecologically or biologically significant marine areas (ebsas)s" ]<-"ecologically or biologically significant marine areas (ebsas)"  
# words[words=="ecologically or biologically significant marine areas (ebsas) ebsa" ]<-"ecologically or biologically significant marine areas (ebsas)"  
# 
# words[words=="environmental impact assessments ()" ]<-"environmental impact assessments"  
# words[words=="environmental impact assessments (eias)" ]<-"environmental impact assessments"  