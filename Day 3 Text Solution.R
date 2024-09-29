
library(stringr)
library(lubridate)

########################
# FROM BOTTOM OF DAY 2 #
########################


# Practice Example 1 - Miami-Dade 311
calls = read.csv('https://dxl-datasets.s3.amazonaws.com/data/miami311_2023.csv')

#0. Filter for closed cases
calls = subset(calls, ticket_status == 'CLOSED')

#1. Convert Date to a date variable
calls$Date = ymd_hms(calls$ticket_created_date_time)

#2. Create a new variable Month, with values corresponding to Month the service call was made.
calls$Month = month(calls$Date)

#3. Calculate the average time to completion by month (tapply...).
avg_days = tapply(calls$actual_completed_days, calls$Month, mean)
avg_days

#4. Wrap barplot() around the tapply() in 3 to visualize this.
barplot(avg_days)



####################
# Day 3 START HERE #
####################

# WHAT YOU SHOULD KNOW AT THE END OF THIS SCRIPT
#   stringr package for working with strings
#   Basics of searching for patterns, regular expressions 
#   1) str_subset() : "search for something, return what you find"
#   2) str_detect() : "search for something, did you find it? TRUE/FALSE"
#   3) str_remove_all() : "search for something, delete it"
#   4) str_replace_all(): "search for something, replace it"
#   Introduce the ifelse() function for conditional logic

# Use ls('package:somePackageNameHere') to show everything inside that package 
# everything that begins with str_....() is a function for "working with strings"
ls('package:stringr')


###############################
#### Text Manipulation #######
##############################
# Text basics

d = read.csv('https://dxl-datasets.s3.amazonaws.com/data/airbnb_broward_full.csv')

host = unique(d$host_name)

str_length(host) # = Also try checking Zip in service calls data
table(str_length(calls$zip_code))
subset(calls, str_length(zip_code) < 5)

str_to_lower(host)
str_to_upper(host)
str_to_title(host)
str_c('Host Name: ', host)
paste('Host Name:', host)

str_c('https://www.airbnb.com/rooms/', d$id)

str_sub(host, 1, 3)
str_sub(d$last_review, 1, 4)
str_sub(d$last_review, 6, 7)

str_sort(host)
str_view(host, 'Doug', match=T)


#############################
# str_subset and str_detect #
#############################
# str_subset and str_detect
#   str_subset - search for a pattern, return what you found
#   str_detect - search for a pattern, tell me if you found it (TRUE/FALSE vector)

str_subset(host, regex('a', ignore_case=T) )    # Returns the value (good for seeing what combinations of that pattern exist)
str_detect(host, regex('a', ignore_case=T) )    # Returns a TRUE/FALSE vector (good for subsetting)



#######################################
# str_replace() and str_replace_all() #
#######################################
#   Search for a pattern, replace it
#   str_replace() - replaces the first encounter (per string)
#   str_replace_all() - replaces all encounters

str_replace(host, 'a', '-')
str_replace_all(host, regex('a', ignore_case = T), '-')

#  I use this most often to remove dollar signs, commas, periods, and spaces (WHICH I NOW SEE I CAN DO SIMPLER WITH str_remove_all()...)
str_replace_all('$20', '\\$', '')
str_remove_all('$20', '\\$')


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

# Can I remove the hyperlinks?
str_remove_all(tweets, 'http.+$')

# Can I remove usernames?
str_remove_all(tweets, '@\\w+')

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

d$pool = ifelse(str_detect(d$amenities, regex('private pool"', ignore_case=T) ), 'Private Pool',
         ifelse(str_detect(d$amenities, regex('pool"', ignore_case=T)), 'Pool', 'No Pool'))

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

arm_cond = str_detect(wounds$location, regex('arm', ignore_case=T))
back_cond = str_detect(wounds$location, regex('back', ignore_case=T))
leg_cond = str_detect(wounds$location, regex('leg', ignore_case=T))

wounds$location2 = ifelse(arm_cond, 'Arm', wounds$location)
wounds$location2 = ifelse(back_cond, 'Back', wounds$location2)
wounds$location2 = ifelse(leg_cond, 'Leg', wounds$location2)

healing_time2 = tapply(wounds$time, wounds$location2, mean)
barplot(healing_time2)


# two other good examples for practice: 
#   - collapse the number of categories in jail$Charge1
#   - collapse the number of categories in calls$issue_type

