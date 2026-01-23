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
    birth_year  = as.numeric(birth_year),
    death_year  = as.numeric(death_year),
    life_length = death_year - birth_year
  ) |>
  filter(!is.na(life_length), !is.na(birth_place))

mapping <- read_csv("all_country_mapping.csv")
names(mapping) <- str_trim(names(mapping))

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
  filter(country %in% countries_wanted) |>
  arrange(desc(avg_life_length))


life_by_country <- read_csv("life-expectancy.csv") |>
  filter(Year >= 1900) |>
  mutate(Entity = str_trim(Entity)) |>
  filter(Entity %in% countries_wanted) |>
  filter(str_to_lower(Entity) != "india") |>
  group_by(country = Entity) |>
  summarise(
    lifeexp_years = mean(`Period life expectancy at birth`, na.rm = TRUE),
    .groups = "drop"
  )


comparison <- inner_join(
  avg_life_by_birth_country_10plus |> select(country, journalists_years = avg_life_length),
  life_by_country,
  by = "country"
)

plot_data <- bind_rows(
  comparison |>
    select(country, years = journalists_years) |>
    mutate(measure = "Journalists (avg life length)"),
  comparison |>
    select(country, years = lifeexp_years) |>
    mutate(measure = "Population (avg life expectancy, 1900+)")
)

ggplot(plot_data, aes(x = country, y = years, fill = measure)) +
  geom_col(position = "dodge") +
  labs(
    x = "Country",
    y = "Years",
    title = "Journalists vs Population Life Expectancy (selected countries)"
  )+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))


