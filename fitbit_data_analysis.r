#### Bellabeat casestudy R Script ####

## Installing and loading common packages and libraries

install.packages('tidyverse')
library(tidyverse)


install.packages('lubridate')
library(lubridate)

install.packages('dplyr')
library(dplyr)

install.packages('ggplot2')
library(ggplot2)

## Importing the datasets (CSV files)
Identifying and importing comprehensive non-redundant datasets

library(readr)
daily_activity_merged <- read_csv("R/fitness_data/daily_activity_merged.csv")
View(daily_activity_merged)

library(readr)
sleep_day_merged <- read_csv("R/fitness_data/sleep_day_merged.csv")
View(sleep_day_merged)

library(readr)
hourlyIntensities_merged <- read_csv("R/fitbit_data/hourlyIntensities_merged.csv")
View(hourlyIntensities_merged)

library(readr)
heartrate_seconds_merged <- read_csv("R/fitbit_data/heartrate_seconds_merged.csv")
View(heartrate_seconds_merged)

library(readr)
weightLogInfo_merged <- read_csv("R/fitbit_data/weightLogInfo_merged.csv")
View(weightLogInfo_merged)

#create dataframe for the daily activity, sleep, intensity, heart rate and weight data. 

daily_activity <- daily_activity_merged
sleep_day <- sleep_day_merged
intensities <- hourlyIntensities_merged
heart_rate <-  heartrate_seconds_merged
weight <- weightLogInfo_merged

###Cleaning and formatting####


# Formatting date and time columns of intensities data
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
View(intensities)

sleep_day <- sleep_day %>%
  rename(date = SleepDay) %>%
  mutate(date = as_date(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

colnames(sleep_day)


daily_activity <- daily_activity %>%
  rename(date = ActivityDate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

colnames(daily_activity)



## Exploring a few key tables/dataframes

head(daily_activity) #Take a look at the daily_activity data
colnames(daily_activity) #Identify all the columns in the daily_activity data

head(sleep_day)
colnames(sleep_day)

head(intensities)
colnames(intensities)

head(heart_rate)
colnames(heart_rate)

*Note both datasets have same the 'Id' field. This can be used to merge the datasets.

#How many observations are there in each dataframe?
nrow(daily_activity)
[1]940
nrow(sleep_day)
[1]413
nrow(intensities)
[1]22099
nrow(heart_rate)
[1]2483658
nrow(weight)
[1]67

# Verifying number of distinct users

n_distinct(daily_activity$Id)
[1] 33
n_distinct(sleep_day$Id)
[1] 24
n_distinct(intensities$Id)
[1]33
n_distinct(heart_rate$Id)
[1]14
n_distinct(weight$Id)
[1]8

--> drop heart rate and weight data frames ('cos low user no.')
[length(unique(daily_activity$Id)) #alternative no. distinct users
[length(unique(sleep_day$Id))] 

## Checking and removing the duplicates and null values
#Duplicates
sum(duplicated(daily_activity))
[1] 0
sum(duplicated(sleep_day))
[1] 3
sum(duplicated(intensities))
[1] 0

#Null values (N/A)
daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()
 
sleep_day <- sleep_day %>%
  distinct() %>%
  drop_na()

intensities <- intensities %>%
  distinct() %>%
  drop_na()


sum(duplicated(sleep_day)) #To check if duplicates are removed 
[1] 0


## Understanding some summary statistics

What are some quick summary statistics we want to know about each data frame?

#For the sleep /daily activity dataframe:
  daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes,
         Calories) %>%
  summary()

#For the sleep day dataframe:
sleep_day %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

#To explore number of activity minutes per category
 daily_activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  summary()
  
What does this tell us about how this sample of people,s activities? 



##Determining the proportion of Users by their activity level

# Merging these two datasets together
$Using id and date as their primary keys to merge daily_activity and daily_sleep to see any correlation between variables.

combined_data <- merge(daily_activity, sleep_day, by=c("Id", "date"))

View(combined_data)
write_csv(combined_data,"combined_data.csv")

#Take a look at how many participants are in this data set.
n_distinct(combined_data$Id)

#Determining daily average
  daily_average <- combined_data %>%
  group_by(Id) %>%
  summarise (mean_steps = mean(TotalSteps), mean_distance = mean(TotalDistance), mean_sleep = mean(TotalMinutesAsleep), mean_calories = mean(Calories))

head(daily_average)
View(daily_average)
#classify users by their daily average steps
user_type <- daily_average %>%
  mutate(user_type = case_when(
    mean_steps < 5000 ~ "sedentary",
    mean_steps >= 5000 & mean_steps < 7499 ~ "lightly active", 
    mean_steps >= 7500 & mean_steps < 9999 ~ "fairly active", 
    mean_steps >= 10000 ~ "very active"
  ))

head(user_type)

#Estimating User type distribution
user_dist <- user_type %>%
  group_by(user_type) %>%
  summarise(subtotal = n()) %>%
  mutate(total = sum(subtotal)) %>%
  group_by(user_type) %>%
  summarise(total_percent = subtotal / total) %>%
  mutate(labels = scales::percent(total_percent))

user_dist$user_type <- factor(user_dist$user_type , levels = c("very active", "fairly active", "lightly active", "sedentary"))


head(user_dist)

 
 ## Plotting a few explorations
  
- What is the relationship between steps taken in a day and sedentary minutes? steps taken vs Calories burnt?
- How could this help inform the customer segments that we can market to? 
  (e.g. position this more as a way to get started in walking more? Or to measure steps that you are already taking?)
  
  
ggplot(data=combined_data, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point()+ geom_smooth()+ labs(title="Relation between Total Steps and SedentaryMinutes")

ggplot(data=combined_data, aes(x=TotalSteps, y=Calories)) + geom_point()+ geom_smooth() + labs(title="Correlation between Total Steps and Calories")


- What is the relationship between minutes asleep and time in bed? 
  You might expect it to be almost completely linear - are there any unexpected trends?
  
ggplot(data=combined_data, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()

ggplot(data=combined_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes))+ geom_point() + geom_smooth() + labs(title="Minutes Asleep vs. Sedentary Minutes")

- What could these trends tell you about how to help market this product? 
  Or areas where you might want to explore further?

## visualization of intensities

ggplot(data=intensities, aes(x=time, y=AverageIntensity)) + geom_histogram(stat = "identity", fill='black') +
  theme(axis.text.x = element_text(angle = 90)) + labs(title="Average Intensity vs. Time")

