
# END OF DAY 3

library(stringr)
host = read.csv('https://dxl-datasets.s3.amazonaws.com/data/airbnb_broward_full.csv', nrow=500)$host_name

# regex for beginning / end of word
str_subset(host, '')




# Day 4

# YOU SHOULD KNOW:
# tidyr package 
# I am really only doing 2 things in this script
#   1) separate() function to split a column into multiple columns
#   2) pivot_longer() to pivot data from wide to long formats
#   2b) pivot_wider() to go in the opposite directions, but that's a bit less common




# Text / column splitting with tidyr::separate()
# Using jail dataset from above...
# Suppose I want BookDate, LastName, FirstName, DOB, Charge, Address, City, State, Zip, Lat, Lon...
# Plot these crimes on a map
library(tidyr)
jail = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_jail_old.csv')

# Note - this code will run once, and if successful it will fail if you try to run again.
#   Success = data has changed, so original code no longer works as columns change
jail = separate(jail, 'Defendant', c('LastName', 'FirstName'), sep=', ')
jail = separate(jail, 'Location.1', c('Address', 'CSZ', 'LatLon'), sep='\n')
jail = separate(jail, 'CSZ', c('City', 'SZ'), sep=', ')
jail = separate(jail, 'SZ', c('State', 'Zip'), sep=' ')
jail = separate(jail, 'LatLon', c('Lat', 'Lon'), sep=', ')

jail$Lat = as.numeric(str_remove_all(jail$Lat, '\\('))
jail$Lon = as.numeric(str_remove_all(jail$Lon, '\\)'))


# putting map here for fun, we'll discuss leaflet around day 8
library(leaflet)
jail %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(clusterOptions = markerClusterOptions())


# tidyr for reshaping/transposing data
#   See Tidy Data article on Blackboard
zillow = read.csv('https://dxl-datasets.s3.amazonaws.com/data/city_zillow.csv')
miami_revenue = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_revenue.csv')

# Data 1 - Miami Revenues
# Goal: Plot Revenue by Year, or Monthly Revenue for any given FY
rev = pivot_longer(miami_revenue,
                   cols=Oct:Sep, 
                   names_to = 'Month', 
                   values_to = 'Revenue')
barplot(tapply(rev$Revenue, rev$FY, sum))


# Data 2 - Zillow house values
# Goal: Plot values over time for Miami
zillow2 = pivot_longer(zillow,
                       cols=X2000.01.31:X2023.07.31,
                       names_to = 'Date',
                       values_to = 'Value')

zillow2$Date = as.Date(zillow2$Date, 'X%Y.%m.%d')

miami = subset(zillow2, RegionName == 'Miami' & State == 'FL')
plot(miami$Value ~ miami$Date, type='l')




# Realtor example for pivot_wider (kind of advanced)
realtors = read.csv('https://dxl-datasets.s3.amazonaws.com/data/realtors.csv')
realtors = subset(realtors, info != 'Agent Full')
realtors = subset(realtors, info != '5/6/2020 Matrix')
realtors = subset(realtors, !str_detect(info, 'The Information is believed'))
realtors = subset(realtors, !str_detect(info, 'The MLS Information'))
realtors = subset(realtors, !str_detect(info, 'and does not'))
realtors = subset(realtors, !str_detect(info, 'aspx'))

my_cols = c(
  'Name', 'Agency', 'Street', 'City', 'IDs', 'License',
  'Work', 'Fax', 'Cell', 'Email', 'Website'
)

realtors$my_cols = rep(my_cols, 186)
realtors$realtor_num = rep(1:186, each=11)

realtors2 = pivot_wider(
  realtors,
  id_cols = realtor_num,
  names_from = my_cols,
  values_from = info
)

