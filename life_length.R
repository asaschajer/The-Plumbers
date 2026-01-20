# Calculate the journalist life length
library(tidyverse)
library(readr)

life_length_plot <- read_csv('journalists.csv') |>
  select("title", "ontology/occupation_label", "ontology/birthYear", "ontology/deathYear")|>
  na.omit()|>
  rename('ontology_birthYear' = 'ontology/birthYear', 'ontology_deathYear' = 'ontology/deathYear' )|>
  mutate(ontology_birthYear = as.numeric(ontology_birthYear), ontology_deathYear = as.numeric(ontology_deathYear))|>
  mutate(life_length = ontology_deathYear - ontology_birthYear)|>
  ggplot()+
  aes(x = life_length)+
  labs(x = "Life lengths (Yrs)", y = 'Number of journalists', title = 'Differences in life lengths among journalists', subtitle = 'DQ: Are there any distinct differences in the life lengths of different journalists?' )+
  theme(plot.title = element_text(face = "bold", size = 14))+
  geom_bar(position = 'dodge')

ggsave('life_length.png', plot = life_length_plot)
