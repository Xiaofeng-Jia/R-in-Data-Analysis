
# DAY 6 - ggplot2
#   - best package on earth for data visualization!

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
#     2. Aesthetics - (what goes where, for example what's on the x-axis, what's on the y-axis?)
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

sales = read.csv('https://dxl-datasets.s3.amazonaws.com/data/iowa_liquor_sales2023.csv')
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
ggplot(d, aes(x=Sales, y=Profit, color = Discount)) +
  geom_point()  

ggplot(d, aes(x=Sales, y=Profit, color=Region)) +
  geom_point()  



# Assignment vs Aesthetics
#   Assignment - one value for the whole plot, outside of aes function, typically in a geom_...()
#     Example: col='blue' - everything is colored blue
#   Aesthetic - mapped to data, values change based on value of data, must be inside aes()
#     Example: col=Region - the color will depend on the value of Region
#   Be careful if you specify both, one will override the other..
ggplot(d, aes(x=Sales, y=Profit, color=Region)) +
  geom_point()

ggplot(d, aes(x=Sales, y=Profit)) +
  geom_point(color='blue')



# Explore some other geom_...'s...

# geom_histogram
hist(d$Quantity, col = 'darkorange', border = 'black') #this is not bad!

ggplot(d, aes(x = Quantity)) + 
  geom_histogram(binwidth = 1, color = 'black', fill = 'darkorange') 


# geom_smooth + some other random things (titles, labels) 
ggplot(d, aes(x=Sales, y=Profit)) +
  geom_point() +
  geom_smooth()

#regression
ggplot(d, aes(x=Sales, y=Profit)) +
  geom_point() +
  geom_smooth(method = 'lm')

#don't want standard error
ggplot(d, aes(x=Sales, y=Profit)) +
  geom_point() +
  geom_smooth(method = 'lm', se=F, color = 'black')




ggplot(d, aes(x=Sales, y=Profit, color=Segment)) +
  geom_point() +
  geom_smooth(method = 'lm', se=F)

#switch the orders, something will on the top


# geom_boxplot
ggplot(d, aes(x=Segment, y=Quantity)) + geom_boxplot() 

# show all the datapoint
+ geom_jitter()
       
#
+ geom_violin()





# Barplots: geom_col() or geom_bar()

# geom_bar does counts by default
ggplot(d, aes(x=Segment)) + 
  geom_bar()#geom_bar is counts

# === the table function
barplot(table(d$Segment))


# geom_col for other metrics (have to calculate height first)
barplot(tapply(d$Profit, d$Segment, mean))

barplot(tapply(d$Profit, d$Segment, sum))



# geom_col takes a "y" aesthetic for height of bars if you want something other than counts
# dplyr for style points!

d %>%
  group_by(Segment)%>%
  summarise(Total_Profit = sum(Profit)) %>%
  ggplot(aes(x= Segment, y=Total_Profit)) + 
    geom_col()





##########
# Scales #
##########

library(scales)

ggplot(d, aes(x=Sales, y=Profit, color=Region)) + 
  geom_point() +
  scale_x_continuous(
    breaks = c(0, 5000, 10000, 15000, 20000),
    labels = dollar #Error in check_breaks_labels(breaks, labels) : object 'dollar' not found
    ) +
    #= c('$0', '$5K', '$10K', '$15K', '$20K', '$25K')) + # you can use scale_xx to change almost everything, so many functions #continuous for continous variable
  scale_color_manual(values=c('aquamarine1', 'chocolate','dodgerblue4', 'pink'))+
  scale_y_continuous(limits = c(-10000, 10000),
                     breaks = seq(-10000, 10000, 1000)) # = c(0, 10000)
                   
                     


#  scale_color_discrete()






########## 
# Facets #
##########

ggplot(d, aes(x = Sales, y = Profit, color = Segment)) +
  geom_point() +
  facet_wrap(~Region)


d %>%
  group_by(Region, Segment) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ggplot(aes(x = Segment, y = Total_Sales)) +
    geom_col() +
    facet_wrap(~Region)
#ERROR!!!!


# Practice Faceting
homes = read.csv('https://dxl-datasets.s3.amazonaws.com/data/HomePrices.csv')
#   create a trend line of housing prices over time by state
#   first color by state
#   second facet by state


ggplot(homes, aes(x = Date,  y = Home.Value, group = State)) +
  geom_line()

ggplot(homes, aes(x = Date,  y = Home.Value, color = State)) +
  geom_line()


ggplot(homes, aes(x = Date,  y = Home.Value)) +
  geom_line() +
  facet_wrap(~State)








# Try again...
zillow = read.csv('https://dxl-datasets.s3.amazonaws.com/data/city_zillow.csv')





# Maps
# maps can be drawn with the geom_polygon() geometry.
# Need to have latitude and longitude values to trace the outline of every region
# Not easy to find unless you're working with something relatively standard (state, country, county, maybe census tract, etc)
library(maps)

statesMap = map_data('state') 

statesMap = map_data('state', region = 'florida') # from maps package, last class...


statesMap = map_data('state', region = c('florida', 'georgia', 'alabama')) 
#Error in if (exact) { : argument is not interpretable as logical


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