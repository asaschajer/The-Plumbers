# journalist birth place and death place
library(tidyverse)


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

ggplot(bd_place) +
  aes(x = birth_death, y = total) +
  labs (
    title = 'Comparison of birth place and death place',
    subtitle = 'DQ: How often do journalists die in a different country or location than \nwhere they were born?',
    y = 'Number of journalists', # whose birth and death locations differ/coincide
    x = ''
  ) +
  scale_x_discrete(labels = c('Locations coincide', 'Locations differ')) +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14))+
  geom_col(position = 'dodge', fill = "#697fb3ff")

ggsave('Comparison_of_birth_place_and_death_place_BAR_PLOT.png')


# bd_place <- bd_place |>
#   rowwise() |>
#   mutate(
#     bd_coincide = as.integer(
#       any(
#         strsplit(`ontology/birthPlace`, " \\| ")[[1]] %in%
#         strsplit(`ontology/deathPlace`, " \\| ")[[1]]
#       )
#     )
#   ) |>
#   ungroup()

