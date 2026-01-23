library(tidyverse)

countries_wanted <- c(
  "France","Canada","Israel","Norway","United Kingdom","United States",
  "Italy","Germany","Netherlands","Colombia","Australia","Spain","Russia","Brazil"
)

journalists <- read_csv("journalists.csv") |>
  select(all_of(c("ontology/birthYear", "ontology/deathYear", "ontology/birthPlace_label"))) |>
  rename(
    birth_year  = "ontology/birthYear",
    death_year  = "ontology/deathYear",
    birth_place = "ontology/birthPlace_label"
  ) |>
  mutate(
    birth_year  = suppressWarnings(as.numeric(birth_year)),
    death_year  = suppressWarnings(as.numeric(death_year)),
    life_length = death_year - birth_year
  ) |>
  filter(!is.na(life_length), !is.na(birth_place))

mapping <- read_csv("all_country_mapping.csv")
names(mapping) <- str_trim(names(mapping))

mapping <- mapping |>
  distinct(birth_place, birth_country)   # <-- fixes many-to-many join warning

avg_life_by_birth_country_10plus <- journalists |>
  left_join(mapping, by = "birth_place") |>
  mutate(
    country = case_when(
      birth_country == "US" ~ "United States",
      birth_country == "UK" ~ "United Kingdom",
      TRUE ~ birth_country
    ),
    country = str_trim(country)
  ) |>
  filter(!is.na(country)) |>
  filter(str_to_lower(country) != "india") |>
  group_by(country) |>
  summarise(
    n = n(),
    avg_life_length = mean(life_length, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(n >= 10) |>
  filter(country %in% countries_wanted)

life_by_country <- read_csv("life-expectancy.csv") |>
  filter(Year >= 1900) |>
  mutate(Entity = str


