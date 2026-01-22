# Calculate the journalist life length aligned with historical eras
library(tidyverse)
library(dplyr)


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

life_length_plot <- read_csv('journalists.csv') |>
  select("title", "ontology/occupation_label", "ontology/birthYear", "ontology/deathYear")|>
  na.omit()|>
  rename('ontology_birthYear' = 'ontology/birthYear', 'ontology_deathYear' = 'ontology/deathYear' )|>
  mutate(ontology_birthYear = as.numeric(ontology_birthYear), ontology_deathYear = as.numeric(ontology_deathYear))|>
  mutate(life_length = ontology_deathYear - ontology_birthYear)|>
  filter(life_length > 0)


read_csv('journalists.csv') |>
  select("title", "ontology/occupation_label", "ontology/birthYear", "ontology/deathYear")|>
  rename('ontology_birthYear' = 'ontology/birthYear', 'ontology_deathYear' = 'ontology/deathYear' )|>
  mutate(ontology_birthYear = as.numeric(ontology_birthYear), ontology_deathYear = as.numeric(ontology_deathYear))|>
  mutate(life_length = ontology_deathYear - ontology_birthYear)|>
  group_by(ontology_deathYear)|>
  filter(life_length > 0 & ontology_deathYear > 1900)|>
  summarise(avr_life_span = mean(life_length))

ggplot(average_life_span) +
  aes(x = ontology_deathYear, y = avr_life_span) +
  geom_line(color = "#697fb3ff", linewidth = 1) +
  geom_vline(
    xintercept = c(1918, 1945, 1991, 2001),
    linetype = "dashed",
    color = "grey40"
  ) +
  annotate(
    "text",
    x = c(1914, 1940, 1965, 1996, 2010),
    y = max(average_life_span$avr_life_span, na.rm = TRUE),
    label = c("Pre-WWI", "Pre–WWII", "Cold War", "Post–Cold War", "Post-2001"),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    x = "Year of death",
    y = "Average journalist lifespan (years)",
    title = "Journalists’ average lifespan across historical eras",
    subtitle = "DQ: Are there any distinct differences in the life lengths of different journalists?\nAverage lifespan trends aligned with major geopolitical periods"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave('his_eras_avr_life_span.png', width = 10, height = 5)

