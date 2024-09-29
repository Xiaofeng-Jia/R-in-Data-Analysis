
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


#   Source: https://apps.who.int/gho/data/view.main.RoadTrafficDeathREG?lang=en
road_deaths = read.csv('https://dxl-datasets.s3.amazonaws.com/data/who_road_deaths.csv')

# Delete first row, keep columns 1, 2, and 6, assign names manually
road_deaths = road_deaths[-1, c(1, 2, 6)]
names(road_deaths) = c('Region', 'Year', 'Info')

road_deaths = separate(road_deaths, 'Info', c('Rate', 'Interval'), ' \\[')
road_deaths = separate(road_deaths, 'Interval', c('Low', 'High'), '-')

# convert to numbers
road_deaths$Rate = as.numeric(road_deaths$Rate)
road_deaths$Low = as.numeric(road_deaths$Low)
road_deaths$High = as.numeric(str_replace(road_deaths$High, '\\]', ''))


americas = subset(road_deaths, Region == 'Americas')
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


library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)

# dplyr uses the "pipe" operator: %>%
#   Take what's on the left, and "pipes" it into function on right
#   When building dplyr "chains", I think of it as just "continue to the next line"

sum(sales$sale_qty)
sales$sale_qty %>% sum()

## Useful verbs (package contains others, but these are vital)
#   1 - select - select the columns you want (or drop the ones you don't)
#   2 - filter - filter/subset rows
#   3 - mutate - modify existing or create new variables
#   4 - group_by - group data into subsets
#   5 - summarise - apply a function to all rows or a subsets of rows
#   6 - arrange - order the data

# New Dataset (Iowa liquor?)
sales = read.csv('https://dxl-datasets.s3.amazonaws.com/data/iowa_liquor_sales2023.csv')
# 1. select:    Select date, storename, city, category, item, unit cost and price, sale quantity, total, and volume
# 2. select:    Rename these if needed
# 3. mutate:    Convert date to a date
# 4. mutate:    Add variables for year and month of sale
# 5. mutate:    Add a column for sale profit
# 6. mutate:    Add a column representing Store Chain by grouping similar Stores (We won't actually finish this, but let's see how we would do that...)
# 7. filter:    Filter for orders totaling over $20,000
# 8. filter:    Filter for orders totaling over $20,000 in Iowa City
# 9. filter:    Filter for the largest order placed in August 2023
# 10. summarise: What is the average order quantity, volume, and total?
# 11. summarise: What is the total volume sold in Iowa City so far in 2023?
# 12.group_by + summarise: What is the total volume sold by city so far in 2023?
# 13.group_by + summarise: What is the total volume in each category, by city, sold so far in 2023?
# 14.group_by + summarise: What is the total profit made on American Vodkas in each city?
# 15. arrange:   Return total sales by vendor, sorted high to low
# 16. arrange:   Return total sales by vendor within each city, sorted high to low within city


# 1-6 solved in this chunk:
#   (for chain, I did a huge nested ifelse() to handle all in one. Notice how many close parentheses are at the end...)
sales2 = sales %>%
  select(
    date, store, city, category, 
    item, cost = unit_cost, price = unit_price, 
    sale_qty, sale_total, sale_liters
  ) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    profit = (price-cost)*sale_qty,
    chain = ifelse(str_detect(store, '^AJ'), "AJ'S LIQUOR", 
            ifelse(str_detect(store, '^CASEY'), "CASEY'S",
            ifelse(str_detect(store, '^CVS'), "CVS",
            ifelse(str_detect(store, '^FAREW'), "FAREWAY",
            ifelse(str_detect(store, '^HAWK'), "HAWKEYE", 
            ifelse(str_detect(store, '^HY-V'), "HY-VEE", 
            ifelse(str_detect(store, '^KUM'), "KUM & GO", 
            ifelse(str_detect(store, '^KWIK STAR'), "KWIK STAR", 
            ifelse(str_detect(store, '^SAM'), "SAM'S", 
            ifelse(str_detect(store, '^TARG'), "TARGET", 
            ifelse(str_detect(store, '^TOBAC'), "TOBACCO OUTLET", 
            ifelse(str_detect(store, '^WAL-M'), "WAL-MART", 
            ifelse(str_detect(store, '^WALGRE'), "WALGREENS", 'OTHER'
          )))))))))))))
  )

# for fun, let's barplot total sales by our new chain column
sales2 %>%
  group_by(chain) %>%
  summarise(sales = sum(sale_total)) %>%
  ggplot(aes(y=reorder(chain, sales), x=sales)) + 
    geom_col()


# 7-9 solved below
# 7
sales %>%
  filter(sale_total > 20000)

# 8
sales %>%
  filter(
    sale_total > 20000,
    city == 'IOWA CITY'
    )

# 9
sales %>%
  mutate(
    date = as.Date(date),
    month = month(date)
  ) %>%
  filter(month == 8) %>%
  filter(sale_total == max(sale_total))

# 10
sales %>%
  summarise(
    avg_qty = mean(sale_qty),
    avg_total = mean(sale_total),
    avg_liters = mean(sale_liters)
  )

# 11
sales %>%
  filter(city == 'IOWA CITY') %>%
  summarise(
    total_volume = sum(sale_liters)
  )

# 12
sales %>%
  group_by(city) %>%
  summarise(total_volume = sum(sale_liters))

# 13
sales %>%
  group_by(city, category) %>%
  summarise(total_volume = sum(sale_liters))
  
# 14
sales %>%
  mutate(profit = (unit_price - unit_cost)*sale_qty) %>%
  filter(category == 'AMERICAN VODKAS') %>%
  group_by(category, city) %>%
  summarise(total_profit = sum(profit))

# 15
sales %>%
  group_by(vendor) %>%
  summarise(sales = sum(sale_total)) %>%
  arrange(desc(sales))

# 16
sales %>%
  group_by(city, vendor) %>%
  summarise(sales = sum(sale_total)) %>%
  arrange(city, desc(sales))


############################################
# Example 2 - dplyr w/ non-dplyr functions #
############################################
jail = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_jail_old.csv', nrows=10000)

jail = separate(jail, 'Defendant', c('Last', 'First'))
jail = separate(jail, 'Location.1', c('Street', 'CSZ', 'LatLon'), sep='\n')
jail = separate(jail, 'CSZ', c('City', 'SZ'), sep=', ')
jail = separate(jail, 'SZ', c('State', 'Zip'), sep=' ')
jail = separate(jail, 'LatLon', c('Lat', 'Lon'), sep=', ')

jail$DOB = as.Date(jail$DOB, '%m/%d/%Y')
jail$Lat = as.numeric(str_replace(jail$Lat, '\\(', ''))
jail$Lon = as.numeric(str_replace(jail$Lon, '\\)', ''))


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


# map just for fun, we'll talk about leaflet in a future class
library(leaflet)
leaflet(jail) %>%
  addTiles() %>%
  addCircleMarkers(label=~Charge1, clusterOptions = markerClusterOptions())





