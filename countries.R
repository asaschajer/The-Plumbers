library(tidyverse)

journalists <- read_csv('journalists.csv') |>
  select(
    "ontology/birthYear",
    "ontology/deathYear",
    "ontology/deathPlace_label"
  ) |>
  rename(
    'birth_year' = 'ontology/birthYear',
    'death_year' = 'ontology/deathYear',
    'death_place' = 'ontology/deathPlace_label'
  ) |>
  mutate(
    birth_year = as.numeric(birth_year),
    death_year = as.numeric(death_year)
  ) |>
  mutate(life_length = death_year - birth_year)


young_journalists <- journalists |>
  filter(life_length <= 50) |>
  na.omit() |>
  left_join(read_csv("region_country_mapping.csv"), by='death_place') |>
  select(country, life_length, -death_place)|>
  group_by(country)|>
  summarise(mean(life_length))
  print(young_journalists)


# avr_life_span_plot <- read_csv('journalists.csv') |>
#   mutate(life_length = ontology_deathYear - ontology_birthYear)|>
#   filter(life_length > 0 & ontology_deathYear > 1900)|>
#   select(`ontology/deathPlace_label`, life_length)|>
#   # mutate(type_lifespan= ifelse(life_length <= 50,"Low_lifespan","High_lifespan"))|>
#   filter(life_length <= 50)

  # ggplot(data=avr_life_span_plot)+
  # aes(x = `ontology/deathPlace_label`, y = life_length, fill=type_lifespan)+
  # labs(x = "Countries", y = "Average journalist's life span", title = "Average life span per country")+
  # theme(plot.title = element_text(face = "bold", size = 14))+
  # geom_line(color = "#697fb3ff")

# ggsave('avr_life_span.png', plot = avr_life_span_plot, width = 10, height = 5)



  