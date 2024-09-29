
# Day 1

# PURPOSE
#   This file is meant to give you a quick overview 
#   of some very basic functions and syntax that we 
#   will use repeatedly throughout the course


# YOU WILL LEARN:
#   1 Read in and inspect a dataset
#   2 THE DOLLAR SIGN ($)
#       a) to grab a variable in a dataset
#       b) to create new variables in a dataset
#   3 CONDITIONS 
#       * you will hear me call these "TRUE/FALSE vectors", because that's what they create
#       * things like ==  >  >=  <  <=  !=
#       Using conditions for:
#         a) counts
#         b) proportions 
#         c) filtering
#   4 FILTERING
#       a) square brackets
#       b) subset()
#   5 FUNCTIONS
#       - not a core topic for today, but we will add in some functions so you start to see them
#       - sum(), mean(), min(), max(), etc. There's a function for everything, Google as needed

# IF YOU ARE UNCOMFORTABLE WITH ANY OF THE TOPICS ABOVE AFTER TODAY
# COME TALK TO ME



# 1 READ IN DATA AND INSPECT IT
#   - comment: see read_data_in_r.R file for bonus material
#   - can read from web link, try from desktop as well
#   - if airbnb isn't working for you, can download here - https://dxl-datasets.s3.amazonaws.com/data/airbnbBroward.csv
d = read.csv('http://data.insideairbnb.com/united-states/fl/broward-county/2023-06-25/visualisations/listings.csv')

d = read.csv('/Users/crazycat/Downloads/listings.csv')
#   - names(), str(), head(), [less important but useful at times: dim(), nrow(), ncol()]
#   - all semester long: read.csv(), str(), and head()
names(d) #very general tell you all the things in the dataset

names(d)[2]='description'


names(d)


str(d)
head(d) #give you first 6 data by default


dim(d)
nrow(d)
ncol(d)



# 2 THE DOLLAR SIGN ($) 
#   - used to select a variable in the data, or put a new one in the data
#   - name_of_data$name_of_variable


d$price
mean(d$price)
min(d$price)
max(d$price)

#estamate annual revenue

d$price*365*0.64

#how to put it into the dataset

d$annual_revenue = d$price*365*0.64


#how to delete the column

d$annual_revenue = NULL

# 3 CONDITIONS - TRUE/FALSE VECTORS
#   == for equality (two equals signs for comparison, one for assignment)
#   != "not equal", <, <=, >, >=
#   put multiple conditions together: & for "and", | for "or"
#   A LOT more than this, discuss later as needed...

# we use this a lot, especially like when we start filtering. For example, like. Give me all the data for the last 2 months. Give me every property in Fort Lauderdale. Give me every private room for under $80 a night in this neighborhood whatever.

#if I want to check somthing like whether it is NULL, I can use the "is."

is.null()


#true false vectors
d$neighbourhood == 'Hollywood'

d$price<250


# or symbol 

d$neighbourhood == 'Hollywood'|d$neighbourhood == 'Dania Beach'

# and symbol &

d$neighbourhood == 'Hollywood'&d$price<250

# 3 USING CONDITIONS FOR...***IMPORTANT!!!

#   - counts: put condition inside sum(...), "counts" the number of observations meeting that condition
#   - proportions: put condition inside mean(...), returns proportion of observations meeting that condition
#   - filtering: return all observations that meet condition


#1 count (if you add all the things together, it will tell you how many truth do you have)

sum(d$neighbourhood == 'Hollywood')

# which means there are 4500 house in Hollywood

#2 proportions

mean(d$neighbourhood == 'Hollywood')
#same as 
4500/17728

# 4 FILTERING DATA
#   - I will filter in two ways:
#     a) using square brackets
#       * when you see an open square bracket, think to yourself "WHERE"
#       * i.e data[someCondition,] = "data WHERE someCondition is TRUE"
#     b) using the subset() function "it is more convinient"

subset(d, neighbourhood == 'Hollywood')

hollywood = subset(d, neighbourhood == 'Hollywood')


#d[ROWS I WANT, COLUMNS I WANT]

d[d$neighbourhood == 'Hollywood', ]

d$price[d$neighbourhood == 'Hollywood']
mean(d$price[d$neighbourhood == 'Hollywood'])






#----------------------------------------------
# 5 PRACTICE
#   return all properties in Fort Lauderdale

d$neighbourhood == 'Fort Lauderdale'

d[d$neighbourhood == 'Fort Lauderdale']
subset(d, neighbourhood == 'Fort Lauderdale')

#   how many properties are in Fort Lauderdale?


sum(d$neighbourhood == 'Fort Lauderdale')

#4462

#   return all private rooms in Fort Lauderdale

d$neighbourhood == 'Fort Lauderdale' & d$room_type == 'Private room'

subset(d, d$neighbourhood == 'Fort Lauderdale' & d$room_type == 'Private room')

#   how many private rooms are in Fort Lauderdale?

sum(d$neighbourhood == 'Fort Lauderdale' & d$room_type == 'Private room')
#507

#   return the most expensive property in the data

max(d$price)

subset(d, price == max(price))


#   select the price column

d$price

#   calculate the average price of property

mean(d$price)

#   average price of private rooms

mean(d$price[d$room_type == 'Private room'])

mean(subset(d, room_type == 'Private room')[ ,10])

mean(subset(d, room_type == 'Private room')$price)


#   average price of private rooms in Fort Lauderdale

mean(d$price[d$neighbourhood == 'Fort Lauderdale' & d$room_type == 'Private room'])



