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
  # filter(life_length > 50) |>
  na.omit() |>
  left_join(read_csv("region_country_mapping.csv"), by = 'death_place') |>
  # filter(country != "India") |>         #  ignore India
  mutate(
    english_speaking = case_match(country,
      "US" ~ TRUE,
      "UK" ~ TRUE,
      "Australia" ~ TRUE,
      .default = FALSE
    )
  ) |>
  select(country, english_speaking, life_length, -death_place) |>
  group_by(english_speaking) |>
  summarise(avg_life_span = mean(life_length), n_observations = n())

print(young_journalists)

# Bar graph country on x-axis, average life span on y-axis
ggplot(young_journalists, aes(x = avg_life_span, y = english_speaking)) +
  # arrange(desc())
  geom_col(fill= "#697fb3ff") +
  # coord_cartesian(xlim = c(20, 50), expand = FALSE)+
  # arrange( desc(country)) +
  labs(
    x = "Average life span (years)",
    y = "Country",
    title ="Average life span of journalists who died at age 50 or younger by country" 
  ) +
  theme(axis.text.x = element_text(hjust = 1), plot.title = element_text(face = "bold", size = 14))

ggsave('Average_lifespan_per_country.png', plot = avr_life_span_plot)

