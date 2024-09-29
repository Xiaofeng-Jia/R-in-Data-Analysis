

library(leaflet)
library(dplyr)
library(stringr)

#------------------------------------------------------------------#
# leaflet maps
crimes = read.csv('https://dxl-datasets.s3.amazonaws.com/data/crimes100.csv')


# 1 Basic leaflet map
# At minimum need leaflet() %>% addTiles()
leaflet() %>%
  addTiles()



# 2 Provider tiles
#   leaflet defaults to OpenStreetMap with addTiles()
#   There are many more maps provided by third party providers
#   We can add them with addProviderTiles(providers$PickOneOfThese) instead of addTiles()

#   Preview them here - http://leaflet-extras.github.io/leaflet-providers/preview/

#   Some require an API key. If you try it and it doesn't appear, that's probably why.

leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) 

leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap)  #his favorite


# 3 Adding points
#   addCircles
#   addMarkers (mix with makeIcon())
#   addCircleMarkers() addAwesomeMarkers()
#   each has different options. ?addCircleMarkers

# custom icons possible but optional

custom_icon = makeIcon(
  iconUrl = 'https://www.freepnglogos.com/uploads/pin-png/pin-northlands-college-admissions-1.png',
  iconWidth = 30,
  iconHeight = 35
)

leaflet(crimes) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(radius = 10, color = '#f47321')
  
#  addCircles(radius = 500, color = '#f47321')
  
#  addMarkers(icon = ~custom_icon)

#  addAwesomeMarkers()
  
#  addMarkers()
  
#  addCircleMarkers()
  
#addCircles()

  ...
#Assuming "Longitude" and "Latitude" are longitude and latitude, respectively
  

  
  
  
# 4 Adding labels and popups to points
#   label = appears when you hover
#   popup = appears when you click
#   can use both
#   need ~ in front of variable name
leaflet(crimes) %>%
  addTiles %>%
  addMarkers(icon=~custom_icon)


leaflet(crimes) %>%
  addTiles() %>%
  addMarkers(
    icon = ~custom_icon,
    label = ~Primary.Type,
    popup = ~Description,
  )



# Can write HTML code to customize pop-ups.
#   IMPORTANT - put inside dataset, makes life easier if you filter later!
crimes$myPopup <- paste(sep='<br/>',
                        paste('<b>Crime</b>:', crimes$Primary.Type),
                        paste('<b>Description</b>:', crimes$Description),
                        '<a href="https://home.chicagopolice.org/online-services/online-crime-reporting/">Report a Crime!</a>'
)

leaflet(crimes) %>%
  addTiles() %>%
  addMarkers(
    icon = ~custom_icon,
    label = ~Primary.Type,
    popup = ~myPopup,
  )


leaflet(crimes) %>%
  addTiles %>%
  addMarkers(icon=~custom_icon,
             label=~Primary.Type)


# 5 Coloring points - color points by Solved status
#   define a color palette, associate 'No' with red, 'Yes', with black
#   can use this palette for any variable with Yes/No values  
#   need to add legend manually with addLegend() layer
myPalette = colorFactor(c('red', 'black'), levels=c('No', 'Yes'))


myPalette(crimes$Solved)


leaflet(crimes) %>%
  addProviderTiles(providers$Stamen) %>%
  addCircleMarkers(radius=3, opacity = 1,
             label=~Primary.Type,
             popup=~myPopup,
             color=~myPalette(Solved)) %>%
  addLegend(
    position = 'topright',
    pal = myPalette,
    values = ~Solved
  )



# 6 Clustering Points
# Very useful if you have a lot of data points (otherwise map will be incredibly slow)

leaflet(crimes) %>%
  addProviderTiles(providers$Stamen) %>%
  addCircleMarkers(radius=3, opacity = 1,
                   label=~Primary.Type,
                   popup=~myPopup,
                   color=~myPalette(Solved),
                   clusterOptions = markerClusterOptions()
                   )


# 7 Groups - Turn points on/off
#   Can incorporate filtering of points by...
#     subsetting original data
#     adding points separately with multiple addCircleMarkers() layers
#     assigning each a group name
#     using a addLayerControl() with overlayGroups option to add filtering option to map
solved = subset(crimes, Solved=='Yes')
unsolved = subset(crimes, Solved=='No')

leaflet() %>%
  addProviderTiles(providers$Stamen) %>%
  addCircleMarkers(data=solved, radius=3, opacity = 1,
                   label=~Primary.Type,
                   popup=~myPopup,
                   color=~myPalette(Solved),
                   group = 'Solved Crimes') %>%
  addCircleMarkers(data=unsolved, radius=3, opacity = 1,
                   label=~Primary.Type,
                   popup=~myPopup,
                   color=~myPalette(Solved),
                   group = 'Unsolved Crimes') %>%
  addLayersControl(overlayGroups = c('Solved Crimes', 'Unsolved Crimes'))





#-----------------------#
# Inside Airbnb example #
#-----------------------#
#   Data here - http://insideairbnb.com/get-the-data.html
d = read.csv('http://data.insideairbnb.com/canada/bc/vancouver/2023-06-10/visualisations/listings.csv') %>%
  mutate(
    link = str_c('https://www.airbnb.com/rooms/', id),
    myPopup = str_c(sep="<br/>", 
                    str_c("<strong>", name, "</strong>"),
                    str_c(room_type, ' for $', price, ' per night!'),
                    str_c('<a href="', link, '">Visit on AirBNB!</a>')
                    )
  )

#3
leaflet(d) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(
    label = ~name,
    color = ~myPalette(room_type)
    clusterOptions = markerClusterOptions()
  )


unique(d$room_type)

myPalette = colorFactor(c('red', 'green', 'blue', 'black'), levels = c('Entire home/apt', 'Private room', 'Shared room', 'Hotel room'))



entireHome = subset(d, room_type == 'Entire home/apt')
privateRoom = subset(d, room_type == 'Private room')
sharedRoom = subset(d, room_type == 'Shared room')
hotelRoom = subset(d, room_type == 'Hotel room')



##################
# BUILD MAP HERE #
##################

leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(data=entireHome, radius=3, opacity = 1,
                   label = ~name,
                   popup = ~myPopup,
                   color = ~myPalette(room_type),
                   clusterOptions = markerClusterOptions(),
                   group = 'Entire Home/Apt'
                   ) %>%
  addCircleMarkers(data=privateRoom, radius=3, opacity = 1,
                   label = ~name,
                   popup = ~myPopup,
                   color = ~myPalette(room_type),
                   clusterOptions = markerClusterOptions(),
                   group = 'Private Room'
                   ) %>%
  addCircleMarkers(data=sharedRoom, radius=3, opacity = 1,
                   label = ~name,
                   popup = ~myPopup,
                   color = ~myPalette(room_type),
                   clusterOptions = markerClusterOptions(),
                   group = 'Shared Room'
                   ) %>%
  addCircleMarkers(data=hotelRoom, radius=3, opacity = 1,
                   label = ~name,
                   popup = ~myPopup,
                   color = ~myPalette(room_type),
                   clusterOptions = markerClusterOptions(),
                   group = 'Hotel Room'
                   ) %>%
  addLayersControl(overlayGroups = c('Entire Home/Apt', 'Private Room', 'Shared Room', 'Hotel Room')) %>%
  addLegend(
    position = 'bottomright',
    colors = c('red', 'green', 'blue', 'black'),
    labels = c('Entire Home/Apt', 'Private Room', 'Shared Room', 'Hotel Room')
  )





#%>%
#  addLegend(
#    position = 'bottomright',
#    pal = myPalette,
#    values = ~room_type
#  )

  




