library(tidyverse)


journalists_lifespan <- read_csv('journalists.csv') |>
  select(
    "ontology/birthYear",
    "ontology/deathYear",
    "ontology/deathPlace_label"
  ) |>
  rename(
    birth_year  = 'ontology/birthYear',
    death_year  = 'ontology/deathYear',
    death_place = 'ontology/deathPlace_label'
  ) |>
  mutate(
    birth_year = as.numeric(birth_year),
    death_year = as.numeric(death_year)
  ) |>
  mutate(life_length = death_year - birth_year)

young_journalists <- journalists |>
  filter(life_length <= 50) |>
  na.omit() |>
  left_join(read_csv("region_country_mapping.csv"), by = 'death_place') |>
  filter(country != "India") |>         #  ignore India
  select(country, life_length, -death_place) |>
  # group_by(country) |>
  # summarise(avg_life_span = mean(life_length))

journalists_country_lifespan <- journalists |>
  na.omit() |>
  left_join(read_csv("region_country_mapping.csv"), by = 'death_place') |>
  filter(country != "India") |>         #  ignore India
  select(country, life_length, -death_place) |>
  group_by(country) |>
  summarise(avg_life_span = mean(life_length))


journalists <- read_csv('journalists.csv') |>
  select("title", 
    "ontology/occupation_label", 
    "ontology/birthYear", 
    "ontology/deathYear", 
    'ontology/birthPlace_label',
    'ontology/deathPlace_label'
  ) |>
  rename(
    'occupation' = 'ontology/occupation_label',
    'birth_year' = 'ontology/birthYear', 
    'death_year' = 'ontology/deathYear', 
    'birth_place' = 'ontology/birthPlace_label', 
    'death_place' = 'ontology/deathPlace_label'
  ) |>
  mutate(
    birth_year = as.numeric(birth_year), 
    death_year = as.numeric(death_year), 
  ) |>  
  mutate(
    bd_coincide = (birth_place == death_place),
    life_length = death_year - birth_year
  ) |>
  filter(death_year >= 1900)|>
  print(n= 705)
 
bd_place_data <- read_csv('journalists.csv') |>
  select("title", 'ontology/birthPlace', 'ontology/birthPlace_label', 'ontology/deathPlace', 'ontology/deathPlace_label', 'ontology/country', 'ontology/country_label')|>
  rename('birthPlace' = 'ontology/birthPlace_label', 'deathPlace' = 'ontology/deathPlace_label' )|>
  mutate(
    bd_coincide = as.integer(birthPlace == deathPlace),
  bd_differ = as.integer(birthPlace != deathPlace)) 

bd_place <- bd_place_data |>
  pivot_longer(
    cols = c(bd_coincide, bd_differ),
    names_to = "birth_death",
    values_to = "coincide_or_differ"
  ) |>
  group_by(birth_death) |>
  summarise(total = sum(coincide_or_differ, na.rm=TRUE))

#   risk: young age, mobility, violent death
sum_data <- left_join(joournalists_lifespan, journalists_country, young_journalists)


