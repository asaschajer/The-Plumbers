# Calculate the journalist life length
library(tidyverse)
library(readr)

life_length_plot <- read_csv('journalists.csv') |>
  select("title", "ontology/occupation_label", "ontology/birthYear", "ontology/deathYear")|>
  na.omit()|>
  rename('ontology_birthYear' = 'ontology/birthYear', 'ontology_deathYear' = 'ontology/deathYear' )|>
  mutate(ontology_birthYear = as.numeric(ontology_birthYear), ontology_deathYear = as.numeric(ontology_deathYear))|>
  mutate(life_length = ontology_deathYear - ontology_birthYear)|>
  filter(life_length > 0)|>
  ggplot()+
  aes(x = life_length)+
  labs(x = "Life lengths (Yrs)", y = 'Number of journalists', title = 'Differences in life lengths among journalists', subtitle = 'DQ: Are there any distinct differences in the life lengths of different journalists?' )+
  theme(plot.title = element_text(face = "bold", size = 14))+
  geom_bar(position = 'dodge')

ggsave('life_length.png', plot = life_length_plot, width = 10, height = 5)

active_years <- read_csv('journalists.csv') |>
  select("title", "ontology/deathYear", 'ontology/activeYears', 'ontology/activeYearsEndDate', 'ontology/activeYearsStartDate', 'ontology/activeYearsEndYear', 'ontology/activeYearsStartYear') |>
  rename(deathYear = "ontology/deathYear", 'endYear' = 'ontology/activeYearsEndYear')

death_on_duty_data <- active_years |>
    mutate(
    died_on_duty = as.integer(endYear == deathYear),
  lived = as.integer(endYear != deathYear)) |>
  pivot_longer(
    cols = c(died_on_duty, lived),
    names_to = "death_on_duty",
    values_to = "on_duty_or_not"
  ) |>
  group_by(death_on_duty) |>
  summarise(total = sum(on_duty_or_not, na.rm=TRUE))


ggplot(death_on_duty_data) +
  aes(x = death_on_duty, y = total / sum(total)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = scales::label_percent()) +
    labs (
    title = 'Journalists deceased on duty',
    subtitle = 'DQ: How often do journalists die while still being officially employed?',
    y = 'Number of journalists', # whose birth and death locations differ/coincide
    x = ''
  ) +
  scale_x_discrete(labels = c('Died on duty', 'Died after retirement')) +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14))

ggsave('died_on_duty.png')

