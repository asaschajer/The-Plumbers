library(tidyverse)

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

life <- read_csv("life-expectancy.csv")

mean_by_country_1900 <- life |>
  filter(Year >= 1900) |>
  filter(Entity %in% countries_wanted) |>
  group_by(Entity) |>
  summarise(
    mean_life_exp = mean(`Period life expectancy at birth`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mean_life_exp))

ggplot(mean_by_country_1900, aes(x = reorder(Entity, mean_life_exp), y = mean_life_exp)) +
  geom_col() +
  labs(x = "Country", y = "Mean life expectancy at birth (1900+)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




