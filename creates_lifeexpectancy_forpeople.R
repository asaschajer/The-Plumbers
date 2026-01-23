library(tidyverse)

countries_wanted <- c(
    "France","Canada","Israel","Norway","United Kingdom","United States",
  "Italy","Germany","Netherlands","Colombia","Australia","Spain","Russia","Brazil"
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




