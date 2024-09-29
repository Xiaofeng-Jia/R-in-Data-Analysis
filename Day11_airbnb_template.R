
library(dplyr)
library(shiny)
library(stringr)
library(leaflet)

asheville = read.csv('/Users/crazycat/Documents/academic/5th year/MAS 627 R/Day 12/publish/asheville.csv') 

ui = fluidPage(
  titlePanel('Where will you stay?'),
  fluidRow(
    column(4,
      radioButtons('room_type', 'Room Type?', choices=c('All', 'Entire home/apt', 'Private room', 'Shared room', 'Hotel room'))
      ),
    column(4,
      sliderInput('price', 'Price Range?', min=0, max=2059, value=c(0, 2059))
      ),
    column(4,
      textInput('desc', 'Description?', placeholder='Enter Keyword')
    )
    
    
  ),
  fluidRow(
    column(12,
           leafletOutput('airbnb_map', height=800))
  )
  
)

server = function(input, output){
  
  
  output$airbnb_map = renderLeaflet({
    
    rows = TRUE
    if (input$roomtype != 'ALL')rows = rows & asheville$room_type == input$room_type
    

    
    asheville[rows, ] %>%
      
      filter(price >= input$price[1],
             price <= input$price[2])%>% 
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(radius=7, opacity=.75, label=~label, popup=~popup,
                       clusterOptions = markerClusterOptions())
  })
  
}

shinyApp(ui, server)