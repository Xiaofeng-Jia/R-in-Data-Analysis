#pdf
install.package('tinytax')





library(tidyr)
library(stringr)
library(dplyr)



# tidyr recap [Day 4]
#   separate()
#   pivot_longer() & pivot_wider()





# Miami Revenue
miami_revenue = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_revenue.csv')

miami_revenue = pivot_longer(miami_revenue, 
                             cols=Oct:Sep,
                             names_to = 'Month',
                             values_to = 'Revenue')

barplot(tapply(miami_revenue$Revenue, miami_revenue$FY, sum), col = '#123456')


sum(miami_revenue$Revenue)



#   Source: https://apps.who.int/gho/data/view.main.RoadTrafficDeathREG?lang=en
road_deaths = read.csv('https://dxl-datasets.s3.amazonaws.com/data/who_road_deaths.csv')






road_deaths = road_deaths[-1, c(1, 2, 6)]
#delete first row, keep 1, 2, 6


names(road_deaths) = c('Region', 'Year', 'Info')

str(road_deaths)


road_deaths = separate(road_deaths, 'Info', c('Rate', 'Interval'), ' \\[')
road_deaths = separate(road_deaths, 'Interval', c('Low', 'High'), '-')






# convert to numbers
road_deaths$Rate = as.numeric(road_deaths$Rate)
road_deaths$Low = as.numeric(road_deaths$Low)

road_deaths$High = as.numeric(str_replace(road_deaths$High, '\\]', ''))

str(road_deaths)

americas = subset(road_deaths, Region == 'Americas')


# Note that this data is in "wide" form but we'd prefer it in "long" form
plot(americas$Rate ~ americas$Year, type='l', ylim=c(12, 20))
lines(americas$Low ~ americas$Year, lty=2)
lines(americas$High ~ americas$Year, lty=2)



#####################
# DAY 5 STARTS HERE #
#######################################################
# dplyr package for data aggregation and manipulation #
#######################################################

# Things you should know:
#   the dplyr package for data manipulation
#   the "big 6" dplyr verbs
#   - select()
#   - filter()
#   - mutate()
#   - group_by()
#   - summarise()
#   - arrange()

#similar to sql

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)

# dplyr uses the "pipe" operator: %>%
#   Take what's on the left, and "pipes" it into function on right
#   When building dplyr "chains", I think of it as just "continue to the next line"


## Useful verbs (package contains others, but these are vital)
#   1 - select - select the columns you want (or drop the ones you don't)
#   2 - filter - filter/subset rows
#   3 - mutate - modify existing or create new variables
#   4 - group_by - group data into subsets
#   5 - summarise - apply a function to all rows or a subsets of rows
#   6 - arrange - order the data






# New Dataset (Iowa liquor?)
sales = read.csv('https://dxl-datasets.s3.amazonaws.com/data/iowa_liquor_sales2023.csv')

sum(sales$sale_qty)

# 1. select:    Select date, storename, city, category, item, unit cost and price, sale quantity, total, and volume

#要什么
sales2 = sales %>%
  select(date, store, city, category, item, unit_cost, unit_price, sale_qty, sale_total, sale_liters)

#by column numbers
sales2 = sales %>%
  select(2, 3, 4, 6, 7)

#改名字
sales2 = sales %>%
  select(date, store, city, category, item, cost = unit_cost, price = unit_price, sale_qty, sale_total, sale_liters)

#不要什么
sales2 = sales %>%
  select(-invoice, -county, -vendor)


# 2. select:    Rename these if needed



# 3. mutate:    Convert date to a date
str(sales2)

sales2 = sales %>% #the data name will only show here,下面不会有
  select(date, store, city, category, item, cost = unit_cost, price = unit_price, sale_qty, sale_total, sale_liters) %>% #to continue to the next line
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date), #，label=T ),
    profit = (price - cost)*sale_qty,
    chain = ifelse(str_detect(store, "AJ'S"), "AJ's", store),
    chain = ifelse(str_detect(chain, 'CASEY'), "CASEY'S", chain)
  ) %>%
  filter(
    sale_total > 20000,
    city == 'IOWA CITY'
  )


#practice for HW1

sales2 = sales %>% #the data name will only show here,下面不会有
  mutate(
    chain = ifelse(str_detect(store, "AJ'S"), "AJ's", store),
    chain = ifelse(str_detect(chain, 'CASEY'), "CASEY'S", chain)
  )



#替换部件

%>%
  filter(
    month == 8,
  )%>%
  filter(sale_total == max(sale_total))


barplot((tapply(sales2$sale_total, sales2$chain, sum)))
  
# 4. mutate:    Add variables for year and month of sale

# 5. mutate:    Add a column for sale profit



# 6. mutate:    Add a column representing Store Chain by grouping similar Stores (We won't actually finish this, but let's see how we would do that...)

str_sort(unique(sales$store))

str_detect(sales$store, 'HY-VEE')

ifelse(str_detect(sales$store, 'HY-VEE'), 'HY-VEE', 
       ifelse(str_detect(sales$store, 'WAL-MART'), 'WAL-MART', sales$store))









# 7. filter:    Filter for orders totaling over $20,000
# 8. filter:    Filter for orders totaling over $20,000 in Iowa City
# 9. filter:    Filter for the largest order placed in August 2023








# 10. summarise: What is the average order quantity, volume, and total?
# 11. summarise: What is the total volume sold in Iowa City so far in 2023?

sales %>%
  group_by(city) %>%
  summarise(
    Total_quantity = sum(sale_qty),
    Total_revenue = sum(sale_total),
    Total_liters = sum(sale_liters)
  )




# 12.group_by + summarise: What is the total volume sold by city so far in 2023?
# 13.group_by + summarise: What is the total volume in each category, by city, sold so far in 2023?
# 14.group_by + summarise: What is the total profit made on American Vodkas in each city?

#sorting things
# 15. arrange:   Return total sales by vendor, sorted high to low
# 16. arrange:   Return total sales by vendor within each city, sorted high to low within city

sales %>%
  group_by(vendor) %>%
  summarise(total_sales = sum(sale_total)) %>%
  arrange(desc(total_sales))




library(dplyr)

# 9/7

sales5 = sales %>%
  select(category, sale_qty, sale_total, sale_liters) %>%
  mutate(
    type = ifelse(str_detect(category, 'VODKA'), 'VODKA',
           ifelse(str_detect(category, 'TEQUI'), 'TEQUILA',
           ifelse(str_detect(category, 'WHISK'), 'TWHISKY', 'OTHERS')))
  )



############################################
# Example 2 - dplyr w/ non-dplyr functions #
############################################

jail = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_jail_old.csv', nrows=10000)

jail = separate(jail, 'Defendant', c('Last', 'First'))
jail = separate(jail, 'Location.1', c('Street', 'CSZ', 'LatLon'), sep='\n')
jail = separate(jail, 'CSZ', c('City', 'SZ'), sep=', ')
jail = separate(jail, 'SZ', c('State', 'Zip'), sep=' ')
jail = separate(jail, 'LatLon', c('Lat', 'Lon'), sep=', ')

str(jail)

jail$DOB = as.Date(jail$DOB, '%m/%d/%Y')
jail$Lat = as.numeric(str_replace(jail$Lat, '\\(', ''))
jail$Lon = as.numeric(str_replace(jail$Lon, '\\)', ''))


# map just for fun, we'll talk about leaflet in a future class
library(leaflet)
leaflet(jail) %>%
  addTiles() %>%
  addCircleMarkers(label=~Charge1, clusterOptions = markerClusterOptions())


#rewrite 

jail = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_jail_old.csv', nrows=10000)

jail2 = jail %>%
  separate('Defendant', c('Last', 'First')) %>%
  separate('Location.1', c('Street', 'CSZ', 'LatLon'), sep='\n') %>%
  separate('CSZ', c('City', 'SZ'), sep=', ') %>%
  separate('SZ', c('State', 'Zip'), sep=' ') %>%
  separate('LatLon', c('Lat', 'Lon'), sep=', ') %>%
  mutate(
    DOB = as.Date(DOB, '%m/%d/%Y'),
    Lat = as.numeric(str_replace(Lat, '\\(', '')),
    Lon = as.numeric(str_replace(Lon, '\\)', ''))
  )


# %>% continue to next line pipes
#合并

jail = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_jail_old.csv', nrows=10000) %>%
  separate('Defendant', c('Last', 'First')) %>%
  separate('Location.1', c('Street', 'CSZ', 'LatLon'), sep='\n') %>%
  separate('CSZ', c('City', 'SZ'), sep=', ') %>%
  separate('SZ', c('State', 'Zip'), sep=' ') %>%
  separate('LatLon', c('Lat', 'Lon'), sep=', ') %>%
  mutate(
    DOB = as.Date(DOB, '%m/%d/%Y'),
    Lat = as.numeric(str_replace(Lat, '\\(', '')),
    Lon = as.numeric(str_replace(Lon, '\\)', ''))
  )




jail = separate(jail, 'Defendant', c('Last', 'First'))
jail = separate(jail, 'Location.1', c('Street', 'CSZ', 'LatLon'), sep='\n')
jail = separate(jail, 'CSZ', c('City', 'SZ'), sep=', ')
jail = separate(jail, 'SZ', c('State', 'Zip'), sep=' ')
jail = separate(jail, 'LatLon', c('Lat', 'Lon'), sep=', ')

