
library(tidyverse)

# 1. read the csv
journalists_raw <- read_csv("journalists.csv", show_col_types = FALSE)

#Keep only the colums we are interested in. Death year and death place 
journalists_small <- journalists_raw |>
  select(`ontology/deathYear`, `ontology/deathPlace_label`)

# Top places of death (mas frecuentes)
# This shit is to get rid  NA :  x == x  es FALSE/NA onlyo cuando x es NA
top_places <- journalists_small |>
  filter(`ontology/deathPlace_label` == `ontology/deathPlace_label`) |>
  filter(`ontology/deathPlace_label` != "") |>
  group_by(`ontology/deathPlace_label`) |>
  summarise(n = n()) |>
  arrange(desc(n))

top_places

#  Top years
top_years <- journalists_small |>
  filter(`ontology/deathYear` == `ontology/deathYear`) |>
  group_by(`ontology/deathYear`) |>
  summarise(n = n()) |>
  arrange(desc(n))

top_years










