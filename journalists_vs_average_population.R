library(tidyverse)

life <- read_csv("life-expectancy.csv")

mean_by_country <- life |>
  filter(Year >= 1900) |>
  group_by(Entity) |>
  summarise(
    mean_life_exp = mean(`Period life expectancy at birth`, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mean_life_exp))

ggplot(mean_by_country, aes(x = reorder(Entity, mean_life_exp), y = mean_life_exp)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Country",
    y = "Mean life expectancy at birth (1900+)",
    title = "Mean life expectancy by country since 1900"
  )

