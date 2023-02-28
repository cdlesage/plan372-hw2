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
ggplot(data, aes(x=DATE_, y=SCORE)) +
  geom_col()
