


library(stringr)
library(lubridate)

########################
# FROM BOTTOM OF DAY 2 #
########################


# Practice Example 1 - Miami-Dade 311
calls = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami311_2023.csv')


calls = read.csv('/Users/crazycat/Downloads/miami311_2023.csv')

View(calls)

#0. Filter for closed cases


calls = subset(calls, ticket_status == 'CLOSED')


#1. Convert Date to a date variable

calls$ticket_created_date_time

calls$Date = ymd_hms(calls$ticket_created_date_time)


#2. Create a new variable Month, with values corresponding to Month the service call was made.

month(calls$Date)

calls$month

calls$month = month(calls$Date)


#3. Calculate the average time to completion by month (tapply...).
avg_time = tapply(calls$actual_completed_days, calls$month, mean)



#4. Wrap barplot() around the tapply() in 3 to visualize this.

barplot(avg_time, col = 'yellow')




####################
# Day 3 START HERE #
####################

# WHAT YOU SHOULD KNOW AT THE END OF THIS SCRIPT
#   stringr package for working with strings
#   Basics of searching for patterns, regular expressions 


#   1) str_subset() : "search for something, return what you find"

#   ***important 2) str_detect() : "search for something, did you find it? TRUE/FALSE"

#   3) str_remove_all() : "search for something, delete it"

#   ***important 4) str_replace_all(): "search for something, replace it"


#   Introduce the ifelse() function for conditional logic

# Use ls('package:somePackageNameHere') to show everything inside that package 

# everything that begins with str_....() is a function for "working with strings"

ls('package:stringr')


###############################
#### Text Manipulation #######
##############################
# Text basics

d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/airbnb_broward_full.csv', nrow=1000)


# just grabbing host name so we have some simple text to work with for the sample code...
host = unique(d$host_name)


str_length(host) # the length of each word
#= Also try checking Zip in service calls data

#the calls data
table(str_length(calls$zip_code))
subset(calls, str_length(zip_code) < 5)



str_to_lower(host)
str_to_upper(host)
str_to_title(host)
str_to_sentence(host)

?str_to_sentence

str_c('Host Name: ', host)

str_c('Host Name: ', host, sep = '------')

paste('Host Name:', host)

#str_c and paste is same


str_c('https://www.airbnb.com/rooms/', d$id)

str_sub(host, 1, 3) #where to start and where to stop

substring(host, 1, 3)
# these two are the same

#in SQL is where to start and how far you want to go



str_sub(calls$ticket_created_date_time, 1, 4) #year

str_sub(calls$ticket_created_date_time, 6, 7) #month




str_sort(host)

str_view(host, 'Doug', match=T) #???find some certain patterns

ls('package:stringr')





#############################
# str_subset and str_detect #
#############################
# str_subset and str_detect
#   str_subset - search for a pattern, return what you found
#   str_detect - search for a pattern, tell me if you found it (TRUE/FALSE vector)



str_subset()    # Returns the value (good for seeing what combinations of that pattern exist)

#e.g. 'A' in the host name
str_subset(host, 'A') 

#ignore case
str_subset(host, regex('a', ignore_case = T)) 


#---------------------------------

str_detect()    # Returns a TRUE/FALSE vector (good for subsetting) only tell you yes or no

str_detect(host, 'A') 

sum(str_detect(host, 'A') )



#######################################
# str_replace() and str_replace_all() #
#######################################
#   Search for a pattern, replace it
#   str_replace() - replaces the first encounter (per string)
#   str_replace_all() - replaces all encounters

str_replace(host, 'a', '-')


str_replace_all(host, regex('a', ignore_case = T), '-')

#refer to the Day2
str_remove_all(host, 'A')

#  I use this most often to remove dollar signs, commas, periods, and spaces (WHICH I NOW SEE I CAN DO SIMPLER WITH str_remove_all()...)
str_replace_all('$20', '$', '') #replace $ into  ' ', which is delete $
str_remove_all('$20', '$')


str_subset(host, 'a$')

str_remove_all('$20', '\\$') #remove special characters???




# Special characters
#   See regex and stringr cheat sheets on Blackboard
#   Try here - https://regex101.com/
str_subset(host, '^A')

str_subset(host, 'a$')

str_subset(host, '(y|ie|ee)$')
?regex # To read about others...


# these can get ridiculous - https://www.reddit.com/r/programminghorror/comments/atfjwp/whats_your_best_worst_regex_command/
# cleaning tweets - https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
tweets = read.csv('https://dxl-datasets.s3.amazonaws.com/data/analytics_tweets.csv')$tweet

tweets
# Can I remove the hyperlinks?

str_remove_all(tweets, 'https://t.co/')

str_remove_all(tweets, 'https:.+')

#str_remove_all(tweets, 'https:.+ ') How to stop at the links


# Can I remove usernames?




# Barplot of most common hashtags (all w/ at least 50 references)? (this was kind of tricky, main point is the regular expression for finding hashtags)
tweets = str_to_lower(tweets)

hashtags = as.character(str_extract_all(tweets, '#[A-z0-9]+', simplify=T))


hashtags = hashtags[hashtags!='']
hashtag_counts = table(hashtags)
hashtag_counts = hashtag_counts[hashtag_counts >= 50]
barplot(hashtag_counts, horiz=F)


# TWO THINGS I USE THIS FOR ALL THE TIME (aside from basic search to see what exists...)
#   1) feature engineering - I have unstructured text that I want to make use of, basically just creating dummy variables with str_detect()
#   2) Standardizing column values when categories are entered in a nonstandard way (for example, combining "FL", "Fl", "fl", "Florida" all into one category) 




#1 str_detect for feature engineering (extracting info to create new columns)
#   What makes a property more/less expensive?
# Maybe a pool? Maybe if its renovated? What else?

# I want to do this 
tapply(d$price, d$pool, mean)


#here are the steps
d$amenities

str_detect(d$amenities, regex('pool', ignore_case = T))


str_view(d$amenities, regex('pool"', ignore_case = T))

d$pool = str_detect(d$amenities, regex('pool"', ignore_case = T))


d$pool


tapply(d$price, d$pool, mean)


# ??? finish it !   lm(number_of_reviews ~ price + )


d$pool = ifelse(
  str_detect(d$amenities, regex('pool"', ignore_case = T)),
  'Yes',
  'No')

barplot(tapply(d$price, d$pool, mean))



#2 str_detect + ifelse() to group categories (reduce # or simply fix data entry errors)

#   location in wounds
#   charge1 in jail
#   PROBABLY DONT READ IN MPUP and LIQUOR WITH ME (unless you clear out your environment...)

mpup = read.csv('https://dxl-datasets.s3.amazonaws.com/data/MPUP.csv')
liquor = read.csv('https://dxl-datasets.s3.amazonaws.com/data/iowa_liquor_sales.csv')

jail = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami_jail_2023.csv')
wounds = read.csv('https://dxl-datasets.s3.amazonaws.com/data/wounds.csv')

#   WOUNDS: Barplot of Average Healing Time by Location
healing_time = tapply(wounds$time, wounds$location, mean)

barplot(healing_time)


condition_arm = str_detect(wounds$location, regex('arm', ignore_case = T))

condition_leg = str_detect(wounds$location, regex('leg', ignore_case = T))

condition_back = str_detect(wounds$location, regex('back', ignore_case = T))


#if you find word "arm", rewrite it 'Arm.

wounds$location = ifelse(condition_arm, 'Arm', wounds$location) #if you find arm, assgin it as "Arm", otherwise leave it alone

wounds$location = ifelse(condition_leg, 'Leg', wounds$location)

wounds$location = ifelse(condition_back, 'Back', wounds$location)

barplot(tapply(wounds$time, wounds$location, mean))




wounds$location = ifelse(condition_arm, 'Arm',
                         ifelse(condition_arm, 'Leg', ))




# two other good examples for practice: 
#   - collapse the number of categories in jail$Charge1
#   - collapse the number of categories in calls$issue_type
str_sort(unique(jail$Charge1))


unique(str_subset(jail$Charge1, 'BURG'))

condition_burg = str_detect(jail$Charge1, 'BURG')

unique(str_subset(jail$Charge1, 'MURD'))

condition_murd = str_detect(jail$Charge1, 'MURD')




jail$Charge1 = ifelse(condition_burg, 'Burglary',
                      ifelse(condition_murd, 'Murder', 'Other'))





str_detect() & ! str_detect() #Murder but not ATT


