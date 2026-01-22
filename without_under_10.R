library(tidyverse)


journalists <- read_csv("journalists.csv") |>
  select(all_of(c("ontology/birthYear", "ontology/deathYear", "ontology/birthPlace_label"))) |>
  rename(
    birth_year  = "ontology/birthYear",
    death_year  = "ontology/deathYear",
    birth_place = "ontology/birthPlace_label"
  ) |>
  mutate(
    birth_year  = as.numeric(birth_year),
    death_year  = as.numeric(death_year),
    life_length = death_year - birth_year
  ) |>
  filter(!is.na(life_length), !is.na(birth_place))


mapping <- read_csv("all_country_mapping.csv")
names(mapping) <- str_trim(names(mapping))


if (!("birth_place" %in% names(mapping)) && ("birthPlace" %in% names(mapping))) {
  mapping <- mapping |> rename(birth_place = birthPlace)
}
if (!("birth_country" %in% names(mapping)) && ("country" %in% names(mapping))) {
  mapping <- mapping |> rename(birth_country = country)
}


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
  arrange(desc(avg_life_length))

print(avg_life_by_birth_country_10plus)


ggplot(avg_life_by_birth_country_10plus, aes(x = reorder(country, avg_life_length), y = avg_life_length)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Birth country",
    y = "Average life length (years)",
    title = "Average life length of journalists by birth country (n ≥ 10, India excluded)"
  )

