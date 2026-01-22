library(tidyverse)

# ---------------------------
# Journalists: avg life length by country (NO India)
# ---------------------------

journalists <- read_csv("journalists.csv") |>
  select("ontology/birthYear", "ontology/deathYear", "ontology/deathPlace_label") |>
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

journalist_by_country <- journalists |>
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
  summarise(journalists_years = mean(life_length, na.rm = TRUE), .groups = "drop")

# ---------------------------
# Life expectancy: mean since 1900 (NO India)
# ---------------------------

countries_wanted <- c(
  "Afghanistan","Angola","Argentina","Australia","Austria","Azerbaijan",
  "Bangladesh","Belarus","Belgium","Bosnia and Herzegovina","Brazil","Canada",
  "Chile","China","Colombia","Cuba","Czechia","Dominican Republic","Egypt",
  "Finland","France","Georgia","Germany","Greece","Guam","Guatemala","Guyana",
  "Haiti","Hungary","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy",
  "Jamaica","Japan","Kyrgyzstan","Latvia","Lebanon","Libya","Lithuania","Macao",
  "Malta","Mexico","Moldova","Myanmar","Netherlands","New Zealand","Nicaragua",
  "Nigeria","North Macedonia","Norway","Pakistan","Paraguay","Philippines","Poland",
  "Portugal","Romania","Russia","Serbia","Slovenia","Somalia","South Africa",
  "Spain","Sri Lanka","Suriname","Sweden","Switzerland","Syria",
  "Trinidad and Tobago","Turkey","Ukraine","United Kingdom","United States",
  "Uruguay","Venezuela","Vietnam"
)

life_by_country <- read_csv("life-expectancy.csv") |>
  filter(Year >= 1900) |>
  mutate(Entity = str_trim(Entity)) |>
  filter(Entity %in% countries_wanted) |>
  filter(str_to_lower(Entity) != "india") |>
  group_by(country = Entity) |>
  summarise(lifeexp_years = mean(`Period life expectancy at birth`, na.rm = TRUE), .groups = "drop")

# ---------------------------
# Join + plot two bars per country
# ---------------------------

comparison <- inner_join(journalist_by_country, life_by_country, by = "country")

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
    title = "Journalists vs Population Life Expectancy (India excluded)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
