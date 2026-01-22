library(tidyverse)


journalists <- read_csv("journalists.csv") |>
  select(
    "ontology/birthYear",
    "ontology/deathYear",
    "ontology/deathPlace_label"
  ) |>
  rename(
    birth_year  = "ontology/birthYear",
    death_year  = "ontology/deathYear",
    death_place = "ontology/deathPlace_label"
  ) |>
  mutate(
    birth_year  = as.numeric(birth_year),
    death_year  = as.numeric(death_year),
    life_length = death_year - birth_year
  ) |>
  filter(!is.na(life_length), !is.na(death_place))


mapping <- read_csv("all_country_mapping.csv")
names(mapping) <- str_trim(names(mapping))


avg_life_by_country <- journalists |>
  left_join(mapping, by = "death_place") |>
  mutate(
    country = case_when(
      death_country == "US" ~ "United States",
      death_country == "UK" ~ "United Kingdom",
      TRUE ~ death_country
    ),
    country = str_trim(country)   
  ) |>
  filter(!is.na(country)) |>
  filter(str_to_lower(country) != "india") |>
  group_by(country) |>
  summarise(
    avg_life_length = mean(life_length, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(avg_life_length))

print(avg_life_by_country)


ggplot(avg_life_by_country, aes(x = reorder(country, avg_life_length), y = avg_life_length)) +
  geom_col() +
  labs(
    x = "Country",
    y = "Average life length (years)",
    title = "Average life length of journalists by country (excluding India)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

