library(tidyverse)
library(lubridate)
data = read_csv("restaurant_inspections.csv")
#Histogram of all inspection scores
ggplot(data, aes(x=SCORE)) +
  geom_histogram(bins = 50) 

# Most of the scores are in the 90s but there is one outlier with a score of 0    
min(data$SCORE)
which(data$SCORE == 0)

# Plot of scores over time, showing that newer restaurants have higher scores
# Answering Question 2
ggplot(data, aes(x=DATE_, y=SCORE)) +
  geom_col()

# Looking at how cities are written differently
unique(data$CITY)

# Recoding city names 
data$CITY = recode(data$CITY, "Raleigh"="RALEIGH", "FUQUAY-VARINA"="FUQUAY VARINA", "Fuquay Varina"="FUQUAY VARINA", "Fuquay-Varina"="FUQUAY VARINA",
                               "Apex"="APEX", "Zebulon"="ZEBULON", "Holly Springs"="HOLLY SPRINGS", "HOLLY SPRING"="HOLLY SPRINGS", "Cary"="CARY", "Morrisville"="MORRISVILLE", "MORRISVILE"="MORRISVILLE", "Garner"="GARNER")

# Grouping data by city, averaging the inspection scores for each 
cities = group_by(data, CITY, SCORE) %>%
  summarize(
    SCORE=mean(SCORE)
  )




