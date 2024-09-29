
# END OF DAY 3

library(stringr)
host = read.csv('https://dxl-datasets.s3.amazonaws.com/data/airbnb_broward_full.csv', nrow=500)$host_name

# regex for beginning / end of word
# regular expressions

#start of string "^xx"

str_subset(host, '^A')

#end of string "xx$"

str_subset(host, 'a$')



# Day 4

# YOU SHOULD KNOW:
# tidyr package 
# I am really only doing 2 things in this script
#   1) separate() function to split a column into multiple columns
#   2) pivot_longer() to pivot data from wide to long formats
#   2b) pivot_wider() to go in the opposite directions, but that's a bit less common

library(tidyr)
#it's all about restruct your data

ls('package:tidyr')

# Text / column splitting with tidyr::separate()
#   Using jail dataset from above...
#   Suppose I want BookDate, LastName, FirstName, DOB, Charge, Address, City, State, Zip, Lat, Lon...
#   Plot these crimes on a map
jail = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_jail_old.csv')


separate(jail, Defendant, c('Last', 'First'), sep =', ')

jail = separate(jail, Defendant, c('Last', 'First'), sep =', ')

# Note - this code will run once, and if successful it will fail if you try to run again.
#   Success = data has changed, so original code no longer works as columns change

#because the 'defedant' is not exist anymore

#how to keep the original data "remove = F"
separate(jail, Defendant, c('Last', 'First'), sep =', ', remove = FALSE)



jail$Location.1

#"1925 NW 26TH ST\nMIAMI, FL 331428471\n(25.80017, -80.228109)"
separate(jail, Location.1, c('Street', 'CityState', 'LatLng'), sep = '\n')

jail = separate(jail, Location.1, c('Street', 'CityState', 'LatLng'), sep = '\n', remove = FALSE)

##全完了！separate(jail, CityState, c('City', 'StateZip', sep = ', '))
         

jail = separate(jail, CityState, c('City', 'StateZip'), sep = ', ')

jail = separate(jail, StateZip, c('State', 'Zip'), sep = ' ')

separate(jail, LatLng, c('Lat', 'Lng'), sep = ', ')

jail = separate(jail, LatLng, c('Lat', 'Lng'), sep = ', ')


str(jail)

str_remove(jail$Lat, '(')  #This one is WRONG!!!
         
#Error in stri_replace_first_regex(string, pattern, fix_replacement(replacement),  : 
#Incorrectly nested parentheses in regex pattern. (U_REGEX_MISMATCHED_PAREN, context=`(`)

str_remove(jail$Lat, '\\(')
as.numeric(str_remove(jail$Lat, '\\('))

jail$Lat = as.numeric(str_remove(jail$Lat, '\\('))


jail$Lng = as.numeric(str_remove(jail$Lng, '\\)'))

str(jail)


# putting map here for fun, we'll discuss leaflet around day 8
library(leaflet)
jail %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(clusterOptions = markerClusterOptions())





#---------------------------------------

# tidyr for reshaping/transposing data
#   See Tidy Data article on Blackboard


zillow = read.csv('https://dxl-datasets.s3.amazonaws.com/data/city_zillow.csv', nrow = 25)


miami_revenue = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_revenue.csv')

# Data 1 - Miami Revenues
# Goal: Plot Revenue by Year, or Monthly Revenue for any given FY
rev = #...
  
barplot(tapply(rev$Revenue, rev$FY, sum))

#the revenue is in 12 months

pivot_longer(miami_revenue,
             col = Oct:Sep,   #the columns want to pivot
             names_to = 'Month',     #new name
             values_to = 'Revenue')     #value

#what is the logic of this code??

barplot(tapply(miami_revenue$Revenue, miami_revenue$FY, sum))

#Error in plot.new() : figure margins too large




miami_revenue = pivot_longer(miami_revenue,
                             col = Oct:Sep,   #the columns want to pivot
                             names_to = 'Month',     #new name
                             values_to = 'Revenue')     






# Data 2 - Zillow house values
# Goal: Plot values over time for Miami

#plot(zhvi ~ date, type ='1')

zillow2 = #...


zillow2 = pivot_longer(zillow,
                       cols = X2000.01.31:X2023.07.31,
                       names_to = 'Date',
                       values_to = 'Value')
str(zillow2)

zillow2$Date = as.Date(zillow2$Date, 'X%Y.%m.%d')

str(zillow2)


miami = subset(zillow2, RegionName == 'Miami' & State == 'FL')

plot(miami$Value ~ miami$Date, type='l')




# Realtor example for pivot_wider (kind of advanced)
realtors = read.csv('https://dxl-datasets.s3.amazonaws.com/data/realtors.csv')
realtors = subset(realtors, info != '5/6/2020 Matrix')
realtors = subset(realtors, info != 'Agent Full')
realtors = subset(realtors, !str_detect(info, 'aspx'))
realtors = subset(realtors, !str_detect(info, 'The Information is'))
realtors = subset(realtors, !str_detect(info, 'does not represent'))
realtors = subset(realtors, !str_detect(info, ' '))

2046/11

#now can change the longer to wide

#repeat

rep(1:186, each=11)

#create a column like this 

realtors$realtor_id = rep(1:186, each=11)


my_cols = c(
  'Name', 'Agency', 'Street', 'City', 'IDs', 'License',
  'Work', 'Fax', 'Cell', 'Email', 'Website'
)

rep(my_cols, times=186)


realtors$my_cols = rep(my_cols, times=186)

realtors2 = pivot_wider(realtors,
                        id_cols = realtor_id,
                        names_from = my_cols,
                        values_from = info)


