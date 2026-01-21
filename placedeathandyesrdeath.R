library(tidyverse)
library(ggplot2)

# 1) Read CSV file
journalists_raw <- read_csv("journalists.csv", show_col_types = FALSE)|>
  select(`ontology/deathYear`, `ontology/deathPlace_label`)

# PLOT 1: Deaths per year

# 3) Make a summary table: count by year
deaths_by_year <- journalists_raw |>
  filter(`ontology/deathYear` == `ontology/deathYear`) |>
  group_by(`ontology/deathYear`) |>
  summarise(n = n()) |>
  filter(`ontology/deathYear`>1900 & nchar(`ontology/deathYear`) == 4)|>
  arrange(`ontology/deathYear`)|>
  mutate(`ontology/deathYear` = as.numeric(`ontology/deathYear`))

# 4) Line plot of the count by year
count_per_year_plot <- ggplot(data = deaths_by_year) +
  aes(x = `ontology/deathYear`, y = n, group = 1)+
  labs(x = "Death year", y = "Number of deaths ", title = "Journalists' deaths by year ", subtitle = "DQ: Are there any differences in the number of journalists' deaths \nrecorded between years?")+
  theme(plot.title = element_text(face = "bold", size = 14))+
  scale_x_continuous(limits = c(1900, 2020), breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020))+
  geom_line(color="#697fb3ff", size=1)

ggsave('count_per_year_plot.png', plot = count_per_year_plot)

# ----------------------------
# PLOT 2: Top 10 lugares
# ----------------------------

# 5) Hacer una tabla resumen: conteo por lugar
deaths_by_place <- journalists_raw |>
  filter(`ontology/deathPlace_label` == `ontology/deathPlace_label`) |>
  filter(`ontology/deathPlace_label` != "") |>
  group_by(`ontology/deathPlace_label`) |>
  summarise(n = n()) |>
  arrange(desc(n))

# 6) Quedarnos con el top 10 (usamos head() para mantenerlo simple)
top10_places <- head(deaths_by_place, 10)

# 7) Bar plot del top 10 lugares (puede quedar apretado si los nombres son largos)
ggplot(data = top10_places) +
  aes(x = `ontology/deathPlace_label`, y = n) +
  geom_col() +
  labs(
    x = "Place of death",
    y = "Number od deaths ",
    title = "Top 10 places of deaths "
  )







