
# August 23 2022

# You should know how to:
#   - Read in and inspect a dataset
#   - THE DOLLAR SIGN ($)
#       - to grab a variable in a dataset
#       - to create new variables in a dataset
#   - LOGICAL COMPARISONS or TRUE/FALSE vectors
#       * things like ==  >  >=  <  <=  !=
#       1) counts
#       2) proportions
#       3) filtering
#   - SQUARE BRACKETS [] TO FILTER
#       1) [rows, columns]
#       2) subset()




# 1 READ IN DATA
# - can read from web link, try from desktop as well
#   - if airbnb isn't working for you, can download here - https://dxl-datasets.s3.amazonaws.com/data/airbnbBroward.csv
d = read.csv('http://data.insideairbnb.com/united-states/fl/broward-county/2023-06-25/visualisations/listings.csv')


# 2 INSPECT OUR DATA
# - str(), head(), names(), [less important but useful at times: dim(), nrow(), ncol()]
# - all semester long, read.csv() then str() then head()...
str(d)
head(d)
names(d)
dim(d)
nrow(d)
ncol(d)


# GRAB COLUMNS FROM THE DATA OR ADD NEW ONES
#   - we know what we have, how do we access it?
#   - $
#   - used to grab variables out of the dataset
#   - or put new variables in the dataset
d$price
d$annual_revenue = d$price*365*.56


# 3 USING CONDITIONS - TRUE/FALSE VECTORS
#   == for equality (two equals signs for comparison, one for assignment)
#   != "not equal", <, <=, >, >=
#   put multiple conditions together: & for "and", | for "or"
#   A LOT more than this, discuss later as needed...

d$room_type == 'Private room'
d$room_type != 'Private room'
d$price > 5000
d$neighbourhood == 'Fort Lauderdale' & d$price > 500


# conditions return TRUE/FALSE
# We can do 3 things with this:
#   sum() to get counts
#   mean() to get proportions
#   include in filters (next..)

#   How many private rooms in data? sum(condition)
sum(d$room_type == 'Private room')

#   What % of properties are private rooms? mean(condition)
mean(d$room_type == 'Private room')


# 4 FILTERING DATA
#   - "square brackets" - [] or [ , ]
#   - subset()
#   - when you see an open square bracket, think to yourself "WHERE"
#       - data[someCondition,] = "data WHERE someCondition"



# 5 FILTERING PRACTICE
# return all properties in Fort Lauderdale
d[ d$neighbourhood == 'Fort Lauderdale' , ]
subset(d, neighbourhood == 'Fort Lauderdale')

# how many properties are in Fort Lauderdale?
sum(d$neighbourhood == 'Fort Lauderdale')

# return all private rooms in Fort Lauderdale
d[ d$neighbourhood == 'Fort Lauderdale' & d$room_type == 'Private room', ]
subset(d, neighbourhood == 'Fort Lauderdale' & room_type == 'Private room')

# how many private rooms are in Fort Lauderdale?
sum(d$neighbourhood == 'Fort Lauderdale' & d$room_type == 'Private room')

# return the most expensive property in the data
subset(d, price == max(price))

# select the price column
d$price

# calculate the average price of property
mean(d$price)

# average price of private rooms
mean( d$price[d$room_type == 'Private room'])

# average price of private rooms in Fort Lauderdale
mean( d$price[d$room_type == 'Private room' & d$neighbourhood == 'Fort Lauderdale'] )


