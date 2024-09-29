

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)


# STEP BACK AND RECAP

# Day 1
# [ ], [ , ] and $, subset()


# Day 2
# as.numeric(), as.Date()
#   - as.numeric generally requires you to remove something from the number first (str_remove))
#   - as.Date() or lubridate functions, i.e. mdy(), ymd(), ymd_hms(), etc
# tapply() make pivot tables can be replaced by dplyr


# Day 3
# stringr package for dealing with text
# run ls('package:stringr') to see all functions, str_xxxx are for working with strings
#   str_detect(), str_remove_all(), str_replace_all()

# str_detect() nested within ifelse() (feature engineering or grouping)



# regexp stuff from that day advanced, beyond scope of course
#   ^ for beginning of line
#   $ for end of line
#   These two are worth knowing, the rest you'll figure out if you find yourself in need of something more advance


# Day 4
# tidyr package for restructuring data
#   - separate()
#   - pivot_longer()


# Day 5
# dplyr for efficient data cleaning / organization
#   select()
#   mutate()
#   filter()
#   group_by()
#   summarise()
#   arrange()



# QUICK DAY 5 RECAP
# EXAMPLE #
# Zillow with dplyr
zillow = read.csv('https://dxl-datasets.s3.amazonaws.com/data/Sale_Prices_State.csv') 


# "Base R" solution (means no packages)
zillow = pivot_longer(
  zillow,
  cols=X2008.03:X2017.12, 
  names_to = 'Date', 
  values_to = 'Price'
)




zillow$Date = str_remove_all(zillow$Date, 'X')

zillow = separate(zillow, 'Date', c('Year', 'Month'), '\\.')


zillow$Year = as.numeric(zillow$Year)
zillow$Month = as.numeric(zillow$Month)


zillow$Date = zillow$Year + zillow$Month/12

zillow = subset(zillow, !is.na(Price))


zillow = zillow[order(zillow$RegionName, zillow$Year, zillow$Month), ]

florida = subset(zillow, RegionName == 'Florida')
plot(florida$Price ~ florida$Date, type='l')

#加date
zillow$Date = as.Date(str_c(zillow$Date, ' .01'), format= 'X%Y.%m,%d')



zillow = read.csv('https://dxl-datasets.s3.amazonaws.com/data/Sale_Prices_State.csv') 


#pipe ttogether
zillow2 = zillow %>%
  pivot_longer(
    cols=X2008.03:X2017.12, 
    names_to = 'Date', 
    values_to = 'Price'
  ) %>%
  mutate(
    Date = str_c(Date, '.01'),
    Date = as.Date(Date, 'X%Y.%m.%d')
  ) %>%
  filter(
    !is.na(Price),
    RegionName == 'Florida'
  )











# DAY 6 - ggplot2
#   - best package on earth for data visualization!!!

#############################################################################################################

# Many reasons for plots
#   - Explore our own data
#   - Explain our data to someone else
#   - Tell a story / presentation
#     * Plots generally better than tables
#   - Different tools for different purposes
#     * explore my own data - quick, ugly plots that nobody sees
#         - base R plot functions
#     * final product, present to someone else - more time and effort, thought given to visual, think about aesthetics, formatting, etc.
#         - ggplot2 package
#
# Base R (no packages)
#   - Not easy to make GOOD plots, many limitations
#   - Hard to incorporate many variables (faceting, coloring, sizing, etc)
#   - Lots of manual work for simple tasks (for example, adding a legend must be done separately and manually)
#   - No unified framework (different plots = different functions = different options)
#


# "gg" = "grammar of graphics"
#   - 6 Grammatical Elements
#     1. Data
#     2. Aesthetics - (what goes where on the plot, for example what's on the x-axis, what's on the y-axis?)
#     3. Geometries - (how should the data look, for example do you want a line, points, bars, etc)

#     4. Facets - (grid of plots, "plot x and y separately for each value of z")
#     5. Coordinates - (JUST SKIP THIS... space in which data is plotted, mostly ignore this, things like polar coordinates or other transformations, rarely use)
#     6. Themes - (all the non-data stuff, like background colors, borders, tick marks, gridlines, fonts, etc)
#
#     1-3 are necessary to see a plot, 4-6 are optional and let us build more complex/custom stuff
#
# All of our plots will look like this:
#   ggplot(DATA, aes(AESTHETICS) ) + 
#     geom_....() + 
#     geom_....() +
#     scale_...() + 
#     theme_...() + 
#     facet_...()
#





library(dplyr)
library(ggplot2)
library(tidyr)

d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/superstore.csv')

# Getting Started (profit by sales scatterplot...)
ggplot(d)
# Nothing, why?

# Adding Aesthetics
ggplot(d, aes(x=Sales, y=Profit)) 


# Still nothing, why?

# Adding geometries
ggplot(d, aes(x=Sales, y=Profit)) + 
  geom_point()

# Need 1) data, 2) aesthetics or aes() and 3) at least one geometry


# Base R... which looks nicer?
plot(d$Sales, d$Profit)


# Aesthetic mappings (basically any time we use a variable in our plot)
#   color by Region, then by Discount
#   ?geom_point to see a list of other aesthetics acceptable 
ggplot(d, aes(x=Sales, y=Profit, color = Region)) +
  geom_point()  

ggplot(d, aes(x=Sales, y=Profit, color = Discount)) +
  geom_point()  



# Assignment vs Aesthetics
#   Assignment - one value for the whole plot, outside of aes function, typically in a geom_...()
#     Example: col='blue' - everything is colored blue
#   Aesthetic - mapped to data, values change based on value of data, must be inside aes()
#     Example: col=Region - the color will depend on the value of Region
#   Be careful if you specify both, one will override the other..

ggplot(d, aes(x=Sales, y=Profit)) +
  geom_point(color = 'pink')

colors() #can see all the colors


# Exercise 1
EconData = read.csv('https://dxl-datasets.s3.amazonaws.com/data/EconomistData.csv')
#   1. Create a scatterplot with CPI on the x axis and HDI on the y axis
#   2. Color the points blue
#   3. Now map the color of the points to the Region variable.
#   4. Make points bigger by setting size to 2.
#   5. Now map the size of the points to the HDI.Rank variable.






# geom_histogram
hist(d$Quantity)

# geom_smooth + some other random things (titles, labels) 
ggplot(d, aes(x=Sales, y=Profit)) +
  geom_point() 

# geom_boxplot?
ggplot(d, aes(x=Region, y=Quantity)) +
  geom_boxplot()

# Barplots: geom_col() or geom_bar()

# geom_bar does counts
ggplot(d, aes(x=Segment)) + 
  geom_bar()


# geom_col for other metrics (have to calculate height first)
barplot(tapply(d$Profit, d$Segment, mean))

# geom_col takes a "y" aesthetic for height of bars if you want something other than counts
# dplyr for style points!



# Exercise II
#   Recreate the scatterplot from Exercise 1
#   Overlay predictions from a linear regression model.
#   Turn off confidence bands.
#   Change the fitted line to the loess method. BONUS: Make it less smooth using the span option.



# Scales
ggplot(d, aes(x=Sales, y=Profit, col=Region)) + 
  geom_point() 




# Facets
ggplot(d, aes(x=Sales, y=Profit)) + 
  geom_point() 



# Practice Faceting
homes = read.csv('https://dxl-datasets.s3.amazonaws.com/data/HomePrices.csv')
#   create a trend line of housing prices over time by state
#   first color by state
#   second facet by state





# Try again...
zillow = read.csv('https://dxl-datasets.s3.amazonaws.com/data/cityZillow.csv')




# Maps
# maps can be drawn with the geom_polygon() geometry.
# Need to have latitude and longitude values to trace the outline of every region
# Not easy to find unless you're working with something relatively standard (state, country, county, maybe census tract, etc)

statesMap = map_data('state') # from maps package, last class...
head(statesMap)

ggplot(statesMap, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill='white', col='grey')


# Comparison with maps package
library(maps)
map('state')


# Florida
flMap = subset(statesMap, region=='florida')
ggplot(flMap, aes(x=long, y=lat)) + 
  geom_polygon(fill='white', col='grey')



# Adding points
# add a new geom_point() that uses the flCities data to add points, sized by population
flCities = subset(us.cities, country.etc=='FL')
ggplot(flMap, aes(x=long, y=lat)) + 
  geom_polygon(fill='white', col='grey') +
  geom_point(data=flCities, aes(x=long, y=lat, size=pop), alpha=.5)
