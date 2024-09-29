
# Day 2

# By the end of this file, you should be comfortable with:
#   - FILTERING
#       1. square brackets
#       2. subset() function
#   - unique(), table(), tapply() functions

#   - How to convert between data types (and why we need to do that)

#   - install.packages(), library()





#######################
# I'm going to use 2 packages in this script
#   stringr for working with strings
#   lubridate for working with dates

#   We need to  1) install the package (only once, then it's on your computer forever)
#               2) load the package


install.packages('stringr')
library(stringr)

install.packages('lubridate')
library(lubridate)



# Day 2

# 1 READ IN DATA (from Day 1 file..)
d = read.csv('http://data.insideairbnb.com/united-states/fl/broward-county/2023-06-25/visualisations/listings.csv')



# 4 FILTERING DATA
#   1) filter the entire dataset
#   2) filter one column
# - "square brackets" - [] or [ , ]
# - subset()

d[1 , ] #row 1 and everything in columns

d[ ,1 ] #this will be the 1st column "ID"

d[ ,c(1, 2, 3)] #c means combine - keeping things together #means give me columns 1, 2, 3

d[1 ,c(1, 2, 3)]

names(d)

names(d)[6] = 'neighborhood'

d[d$neighbourhood == 'Fort Lauderdale']


# 5 PRACTICE
#   return all properties in Fort Lauderdale

#   how many properties are in Fort Lauderdale?

#   return all private rooms in Fort Lauderdale

#   how many private rooms are in Fort Lauderdale?

#   return the most expensive property in the data

#   select the price column

#   calculate the average price of property

#   average price of private rooms

#   average price of private rooms in Fort Lauderdale








###############################
# unique(), table(), tapply() #
###############################

# unique() - return list of unique values
# Example: What room types are available in the data?

unique(d$room_type)
unique(d$neighbourhood)


# table() - returns value counts in pivot table
# Example: How many properties are of each room type?

table(d$room_type)



# tapply() - basically a pivot table
#          - take one variable, pivot by another, return some value
#          - tapply(NUMERIC COLUMN, GROUP VARIABLE, WHAT YOU WANT TO CALCULATE)

tapply()


#1. What is the average price per night by neighbourhood?

tapply(d$price, d$neighbourhood, mean)


tapply(d$price, d$room_type, mean)

tapply(d$price, d$room_type, max)

tapply(d$price, d$room_type, min)


#2. What is the average price of private rooms by neighborhood?

tapply(d$price[d$room_type == 'Private room'], d$neighbourhood, mean)

#Error in tapply(d$price[d$room_type == "Private room"], d$neighbourhood,  : 
#arguments must have same length

tapply(d$price[d$room_type == 'Private room'], d$neighbourhood[d$room_type == 'Private room'], mean)

#3. Which neighborhood is most popular? Calculate the total number of reviews by neighborhood. 

tapply(d$number_of_reviews, d$neighbourhood, sum)



#4. Can you filter these results for the "most popular"?

max(tapply(d$number_of_reviews, d$neighbourhood, sum))
#it only give me the numbers


reviews = tapply(d$number_of_reviews, d$neighbourhood, sum)
reviews[reviews == max(reviews)]







#########################
# DATA TYPE CONVERSIONS #
#########################
#   as.character()
#   as.numeric()
#   as.Date() vs lubridate functions
#   see here - https://www.statmethods.net/input/dates.html


dates = read.csv('https://dxl-datasets.s3.amazonaws.com/data/dates.csv')

str(dates) # NOTHING is in the right form (though you can argue ID as an integer isn't a big deal...)


#ID
as.character(dates$ID) #in "" which means it is a character now

dates$ID = as.character(dates$ID) #save back to the dataset

#Total

as.numeric(dates$Total) #error: the compurter doesn't know how to deal with dollar sign

dates$Total #get ride of dollar sign and comma

library(stringr)

ls('package: stringr') #print all the functions of a package !!!!not correct


str_remove_all(dates$Total, '[$,]')

as.numeric(str_remove_all(dates$Total, '[$,]')) #without "" it is a number

dates$Total = as.numeric(str_remove_all(dates$Total, '[$,]'))



str_replace_all(dates$Total, '[$,]', '') #replace with blank



#5 different formats of Dates 

#Date1
as.Date(dates$Date1)

dates$Date1 = as.Date(dates$Date1)

dates$Date1[1] + 30 #第一个数据加30天的意思


#Date2

as.Date(dates$Date2, format = '%m / %d / %Y')

dates$Date2 = as.Date(dates$Date2, format = '%m / %d / %Y')


#Date3

dates$Date3

as.Date(dates$Date3, format = '%A, %B %d, %Y')

dates$Date3 = as.Date(dates$Date3, format = '%A, %B %d, %Y')


#Date4
dates$Date4

as.Date(dates$Date4, format = 'Today is June 24 in the year 2023')

as.Date(dates$Date4, format = 'Today is %B %d in the year %Y')

dates$Date4 = as.Date(dates$Date4, format = 'Today is %B %d in the year %Y')



#lubridate

library(lubridate)

ls('package: lubridate') #????

#Date1
dates$Date1

dates$Date1 = ymd(dates$Date1)


#Date2
dates$Date2

dates$Date2 = mdy(dates$Date2)


#Dates3
dates$Date3

mdy(dates$Date3)

dates$Date3 = mdy(dates$Date3)



#date4

dates$Date4

mdy(dates$Date4)

dates$Date4 = mdy(dates$Date4)

#date5
dates$Date5
ymd_hms(dates$Date5)

dates$Date5 = ymd_hms(dates$Date5)



#format (base R / the original way)
format(dates$Date1, format = '%Y-%m')

# lubridate functions
year(dates$Date1)

month(dates$Date1, label = T, abbr = F)

wday(dates$Date1, label = T)

semester(dates$Date1)







# Practice Example - Miami-Dade 311
calls = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami311_2023.csv')

#0. Filter for closed cases

calls = subset(calls, ticket_status == 'CLOSED')

#1. Convert Date to a date variable

calls$ticket_created_date_time = ymd_hms(calls$ticket_created_date_time)


#2. Create a new variable Month, with values corresponding to Month the service call was made.

month(calls$ticket_created_date_time)

calls$month = month(calls$Date)


#3. Calculate the average time to completion by month (tapply...).


tapply(calls$actual_completed_days, calls$month, mean)

avg_time = tapply(calls$actual_completed_days, calls$month, mean)


#4. Wrap barplot() around the tapply() in 3 to visualize this.

barplot(avg_time) 

# Purchase Orders
po = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_purchase_orders2023.csv')

# obtain total spend by month in 2023 for the city of miami

#1. filter the MIAMI cases
subset(po, CITY == 'MIAMI')

po = subset(po, CITY == 'MIAMI')


#2. deal with the month

po$Date = ymd_hms(po$PO_DATE)

month(po$Date)

po$month = month(po$Date)

#3. pivot table

tapply(po$PO_AMOUNT, po$month, sum)




