### Cyclistic_2020_Analysis ###

# This analysis is for case study 1 from the Google Data Analytics Certificate (Cyclistic).  It’s originally based on the case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). We will be using the Divvy dataset for the case study. The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle data attributes
library(dbplyr) #also help wrangle data attributes
library(ggplot2)  #helps visualize data
setwd('/Users/kristinafrazier/Documents/data_projects/cyclistic/data/CSVs') 
getwd() #displays your working directory

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
d2020_010203 <- read_csv("Divvy_Trips_2020_Q1.csv")
d2020_04 <- read_csv("202004-divvy-tripdata.csv")
d2020_05 <- read_csv("202005-divvy-tripdata.csv")
d2020_06 <- read_csv("202006-divvy-tripdata.csv")
d2020_07 <- read_csv("202007-divvy-tripdata.csv")
d2020_08 <- read_csv("202008-divvy-tripdata.csv")
d2020_09 <- read_csv("202009-divvy-tripdata.csv")
d2020_10 <- read_csv("202010-divvy-tripdata.csv")
d2020_11 <- read_csv("202011-divvy-tripdata.csv")
d2020_12 <- read_csv("202012-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(d2020_010203)
colnames(d2020_04)
colnames(d2020_05)
colnames(d2020_06)
colnames(d2020_07)
colnames(d2020_08)
colnames(d2020_09)
colnames(d2020_10)
colnames(d2020_11)
colnames(d2020_12)

# Inspect the dataframes and look for inconsistency
str(d2020_010203)
str(d2020_04)
str(d2020_05)
str(d2020_06)
str(d2020_07)
str(d2020_08)
str(d2020_09)
str(d2020_10)
str(d2020_11)
str(d2020_12)


# Convert ride_id, rideable_type, start_station_id, and end_station_id to character so that they can stack correctly
d2020_010203 <-  mutate(d2020_010203, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)
                   ,start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(end_station_id)) 
d2020_04 <-  mutate(d2020_04, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)
                   ,start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(end_station_id)) 
d2020_05 <-  mutate(d2020_05, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)
                   ,start_station_id = as.character(start_station_id)
                   ,end_station_id = as.character(end_station_id))
d2020_06 <-  mutate(d2020_06, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)
                    ,start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id)) 
d2020_07 <-  mutate(d2020_07, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)
                    ,start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id))
d2020_08 <-  mutate(d2020_08, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)
                    ,start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id)) 
d2020_09 <-  mutate(d2020_09, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)
                    ,start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id))
d2020_10 <-  mutate(d2020_10, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)
                    ,start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id)) 
d2020_11 <-  mutate(d2020_11, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)
                    ,start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id))
d2020_12 <-  mutate(d2020_12, ride_id = as.character(ride_id)
                    ,rideable_type = as.character(rideable_type)
                    ,start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(d2020_010203, d2020_04, d2020_05, d2020_06, d2020_07, d2020_08, d2020_09, d2020_10, d2020_11, d2020_12)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (2) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (3) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Finally, remove all null data rows
all_trips_v2 <- all_trips_v2[rowSums(is.na(all_trips_v2)) != ncol(all_trips_v2), ] 

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create csv files that we will visualize in Excel, Tableau, or any presentation software

# Average ride length by day of week by rider type
duration <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(duration, file = '/Users/kristinafrazier/Documents/data_projects/cyclistic/data/avg_ride_length.csv')

# Average ride length by rider type
avg_ridelength <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
write.csv(avg_ridelength, file = '/Users/kristinafrazier/Documents/data_projects/cyclistic/data/avg_ride_length_rider.csv')

# Count of trips by day of week by rider type
require(dplyr)
counts <- all_trips_v2 %>% count(member_casual, day_of_week)
write.csv(counts, file = '/Users/kristinafrazier/Documents/data_projects/cyclistic/data/trip_counts.csv')