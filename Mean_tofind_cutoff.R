
library(tidyverse)

life <- read_csv("life-expectancy.csv")

avg_1900_onwards <- life |>
  filter(Year >= 1900) |>
  summarise(avg_life_exp = mean(`Period life expectancy at birth`, na.rm = TRUE))

print(avg_1900_onwards)
