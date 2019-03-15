
library(dplyr)
library(rgdal)
library(raster)
library(tidyverse)
library(leaflet)
library(shiny)
library(lazyeval)
library(plyr)
library(timeDate)

#path_to_shp_file is the file path to the shape file
shp_file_reader <- function(path_to_shp_file){
  
  shp <- readOGR(path_to_shp_file)
  shp <- spTransform(shp, CRS("+init=epsg:4326"))
  
  
  return(shp)
  
}

# setwd("~/Desktop/pollution_app")

# main_shape <- shp_file_reader('Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales/Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.shp')
# 
# pollution_data <- readRDS('pollution_data.rds')




ui <- fluidPage(
  
  titlePanel("Welcome to the HS2 pollution visualisation tool!"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput(inputId = 'pol_type', 
                     label = 'Choose your Pollution type',
                     choices = c('Air' = 'air', 
                                 'Noise' = 'noise'),
                     selected = 'air', multiple = FALSE),
      
      uiOutput('ls'),
      
      uiOutput('year'),
      
      uiOutput('hour_time')
      
    ),
    
    mainPanel(
      textOutput("selected_var3"),
      textOutput("selected_var4"),
      textOutput("selected_var1"),
      textOutput("selected_var2"),
      
      
      leafletOutput(outputId = "map", height = 800)
      
    )
  )
)


server <- function(input, output) {
  
  output$selected_var3 <- renderText({
    paste("You have selected", input$pol_type)
  })
  output$selected_var4 <- renderText({
    paste("You have selected", input$lsoa)
  })
  output$selected_var1 <- renderText({ 
    paste("You have selected", input$date_of_year)
    
  })
  output$selected_var2 <- renderText({
    paste("You have selected", input$hours)
  })
  
  
  
  output$year <- renderUI({
    
    req(input$pol_type)
    
    
    
    
    
    
    
    
    x <- pollution_data %>%
      filter(pollution_type %in% input$pol_type,
             grepl(paste(input$lsoa, collapse = '|'), 
                   tolower(lsoa11nm))  ) %>%
      group_by(pollution_type) %>%
      dplyr::summarise(min_date = min(date, na.rm = TRUE), max_date = max(date, na.rm= TRUE))
    
    sliderInput("date_of_year",
                "Dates",
                min = min(x$min_date),
                max = max(x$max_date),
                value= c(min(x$min_date), max(x$max_date)),
                timeFormat="%Y-%m-%d")
  })
  
  
  output$hour_time <- renderUI({
    
    req(input$date_of_year)
    
    y <- pollution_data %>%
      filter(pollution_type %in% input$pol_type,
             date == input$date_of_year,
             grepl(paste(input$lsoa, collapse = '|'), 
                   tolower(lsoa11nm))) %>%
      group_by(pollution_type) %>%
      dplyr::summarise(min_hour = min(hour_time, na.rm = TRUE), max_hour = max(hour_time, na.rm= TRUE))
    
    sliderInput("hours",
                "hours",
                min = min(y$min_hour),
                max = max(y$max_hour),
                value= c(min(y$min_hour), max(y$max_hour)),
                timeFormat="%Y-%m-%d")
  })
  
  output$ls <- renderUI({
    
    req(input$pol_type)
    
    camden <- c('Ealing' = 'ealing', 
                'Camden' = 'camden')
    ealing <- c('Ealing' = 'ealing', 
                'Camden' = 'camden',
                'Hillingdon' = 'hillingdon',
                'Hammersmith and Fulham' = 'hammersmith')
    
    if ('air' %in% input$pol_type) {
      choices <-c('Ealing' = 'ealing', 
                  'Camden' = 'camden',
                  'Hillingdon' = 'hillingdon',
                  'Hammersmith and Fulham' = 'hammersmith')
      
    }else{
      
      choices <- c('Ealing' = 'ealing', 
                   'Camden' = 'camden')
    }
    
    selectizeInput(inputId = 'lsoa', 
                   label = 'Choose your Boroughs', 
                   choices = choices, 
                   selected = 'camden', multiple = TRUE)
    
  })
  
  
  
  
  
  output$map <- renderLeaflet({
    
    
    
    req(input$date_of_year, input$pol_type, input$hours, input$lsoa)
    
    incl.data=TRUE
    df <- subset(pollution_data, min_time == 0 & date == input$date_of_year & pollution_type %in% input$pol_type & hour_time == input$hours & grepl(paste(input$lsoa, collapse = '|'), 
                                                                                                                                                    tolower(lsoa11nm)))
    # icons <- awesomeIcons(
    #   icon = 'ios-close',
    #   iconColor = 'black',
    #   library = 'ion',
    #   markerColor = getColorNoise(df)
    # )
    m <- leaflet() %>% 
      
      addTiles()%>%
      setView(lng = -0.2965, lat = 51.5521, zoom = 12)%>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addAwesomeMarkers(data = df,
                        lng = ~longitude, 
                        lat = ~latitude, 
                        
                        icon = ~awesomeIcons(
                          icon = 'ios-close',
                          iconColor = 'black',
                          library = 'ion',
                          markerColor = colour
                        ),
                        popup = ~paste("pollutionlevel", pollution_level, "<br>",
                                       "location:", location, "<br>",
                                       "pollution type:", pollution_type, "<br>",
                                       "LSOA:", lsoa11nm,
                                       "Colour:", colour),
                        label = ~as.character(pollution_type))%>%
      addPolygons(data = subset(main_shape, grepl(paste(input$lsoa, collapse = '|'), 
                                                  tolower(lsoa11nm))), 
                  color = "#444444", 
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.5)
    m
  })
}



shinyApp(ui, server)

