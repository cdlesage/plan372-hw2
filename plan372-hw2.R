library(tidyverse)
library(lubridate)
data = read_csv("restaurant_inspections.csv")
#Histogram of all inspection scores (Q1)
ggplot(data, aes(x=SCORE)) +
  geom_histogram(bins = 50) 

# Most of the scores are in the 90s but there is one outlier with a score of 0    
min(data$SCORE)
which(data$SCORE == 0)

# Plot of scores over time
# Q2

# Finding average score for each date first
data_years = group_by(data, DATE_) %>%
  summarize(SCORE=mean(SCORE))
# Then plotting with that data
ggplot(data_years, aes(x=DATE_, y=SCORE)) +
  geom_col()

# Looking at how cities are written differently (Q3)
unique(data$CITY)

# Recoding city names (Q3)
data$CITY = recode(data$CITY, "Raleigh"="RALEIGH", "FUQUAY-VARINA"="FUQUAY VARINA", "Fuquay Varina"="FUQUAY VARINA", "Fuquay-Varina"="FUQUAY VARINA",
                               "Apex"="APEX", "Zebulon"="ZEBULON", "Holly Springs"="HOLLY SPRINGS", "HOLLY SPRING"="HOLLY SPRINGS", "Cary"="CARY", "Morrisville"="MORRISVILLE", "MORRISVILE"="MORRISVILLE", "Garner"="GARNER", "Wake Forest"="WAKE FOREST")

# Grouping data by city, averaging the inspection scores for each (Q3)
cities = group_by(data, CITY) %>%
  summarize(
    SCORE=mean(SCORE)
  )


# Grouping data by inspector (Q4)
inspectors = group_by(data, INSPECTOR) %>%
  summarize(
    SCORE=mean(SCORE)
  )

# Sample size per city (Q5)
cities_samplesize = group_by(data, CITY) %>%
  summarize(
    SampleSize=n()
  )

# Sample size per inspector (Q5)
inspectors_samplesize = group_by(data, INSPECTOR) %>%
  summarize(
    SampleSize=n()
  )

# Grouping data by facility with average scores for each (Q6)
facility = group_by(data, FACILITYTYPE) %>%
  summarize(
    SCORE=mean(SCORE)
  )

sum(is.na(data$FACILITYTYPE))


# Filtering the data (Q7)
restaurant_data = filter(data, FACILITYTYPE == "Restaurant")

# Histogram of restaurants (Q7)
ggplot(restaurant_data, aes(x=SCORE)) +
  geom_histogram(bins = 50) 


# Plot of scores over time for restaurants (Q7)
# Finding average score for each date
restaurant_data_years = group_by(restaurant_data, DATE_) %>%
  summarize(SCORE=mean(SCORE))

ggplot(restaurant_data_years, aes(x=DATE_, y=SCORE)) +
  geom_col()


# Looking at how cities are written differently (Q7)
unique(restaurant_data$CITY)

# Recoding RTP acronym
data$CITY = recode(restaurant_data$CITY, "RTP"="RESEARCH TRIANGLE PARK")


# Grouping restaurant data by city, averaging the inspection scores for each (Q7)
restaurant_data_cities = group_by(restaurant_data, CITY) %>%
  summarize(
    SCORE=mean(SCORE)
  )

# Grouping restaurant data by inspector (Q7)
restaurant_data_inspectors = group_by(restaurant_data, INSPECTOR) %>%
  summarize(
    SCORE=mean(SCORE)
  )

# Sample size per city (Q7)
restaurant_data_cities_samplesize = group_by(restaurant_data, CITY) %>%
  summarize(
    SampleSize=n()
  )

# Sample size per inspector (Q7)
restaurant_data_inspectors_samplesize = group_by(restaurant_data, INSPECTOR) %>%
  summarize(
    SampleSize=n()
  )








