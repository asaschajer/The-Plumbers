library(tidyverse)

journalists <- read_csv('journalists.csv') |>
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
  group_by(country) |>
  summarise(avg_life_span = mean(life_length))


print(young_journalists)

# Bar graph country on x-axis, average life span on y-axis
ggplot(young_journalists, aes(x = country, y = avg_life_span)) +
  geom_col() +
  labs(
    x = "Country",
    y = "Average life span (years)",
    title = "Average life span of journalists who died at age 50 or younger"
  ) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
