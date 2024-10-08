

library(dplyr)
library(shiny)
library(stringr)
library(leaflet)
library(scales)

asheville = read.csv('c:/users/dxl701/dropbox/current courses/mas 627/day 12/asheville.csv') %>%
  mutate(
    accommodates = factor(accommodates)
  ) %>%
  rename(superhost = host_is_superhost)

ui = fluidPage(
  titlePanel('Where will you stay?'),
  sidebarLayout(
    sidebarPanel(
      radioButtons('room_type', 'Room Type?', choices=c('All', 'Entire home/apt', 'Private room', 'Shared room', 'Hotel room')),
      sliderInput('price', 'Price Range?', min=0, max=10000, value=c(0, 10000)),
      numericInput('minPrice', 'Min Price:', value=0, step=10),
      numericInput('maxPrice', 'Max Price:', value=10000, step=10),
      textInput('desc', 'Description?', placeholder='Enter Keyword'),
      selectInput('price_group', 'Price By?', choices=c('Room Type' = 'room_type', 'Accommodates'='accommodates', 'Superhost'='superhost'))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Explore Asheville', leafletOutput('airbnb_map', height=800)),
        tabPanel('Pricing', plotOutput('price_bars', height=500))
      )
    )
    
  )
)

server = function(input, output){
  
  
  output$airbnb_map = renderLeaflet({
    asheville %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(radius=7, opacity=.75, label=~label, popup=~popup,
                       clusterOptions = markerClusterOptions())
  })
  
  output$price_bars = renderPlot({
    asheville %>%
      group_by_at(input$price_group) %>%
      summarise(avg_price = mean(price)) %>%
      ggplot(aes_string(y=input$price_group, x='avg_price')) + 
      geom_col() +
      labs(y='', x='Average Price', title=str_c('Average Price by ', input$price_group)) +
      scale_x_continuous(labels=dollar) + 
      theme(
        axis.text = element_text(face='bold', size=12)
      )
  })
  
}

shinyApp(ui, server)