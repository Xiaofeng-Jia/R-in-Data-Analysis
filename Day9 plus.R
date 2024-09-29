


library(shiny)
library(leaflet)
library(dplyr)
library(stringr)

d = read.csv('http://data.insideairbnb.com/canada/bc/vancouver/2023-06-10/visualisations/listings.csv') %>%
  mutate(
    link = str_c('https://www.airbnb.com/rooms/', id),
    myPopup = str_c(sep="<br/>", 
                    str_c("<strong>", name, "</strong>"),
                    str_c(room_type, ' for $', price, ' per night!'),
                    str_c('<a href="', link, '">Visit on AirBNB!</a>')
    )
  )


#build map

leaflet(d) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(data=entireHome, radius=3, opacity = 1,
                   label = ~name,
                   popup = ~myPopup,
                   clusterOptions = markerClusterOptions(),
  ) 






ui = fluidPage(
  selectInput('zoom_type', 'Room Type:', choices = unique(d$room_type)),
  leafletOutput('my map')
)


server = function(input, output)(
  
  output$my_map = renderLeaflet(
    d %>%
      fliter(room_type == input),
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(data=entireHome, radius=3, opacity = 1,
                       label = ~name,
                       popup = ~myPopup,
                       clusterOptions = markerClusterOptions(),
      )
    
  )
)
  

shinyApp(ui, server)



