library(tidyverse)

# Define violent death causes
violent_causes <- c(
  "Assassination", "Sniper", "Hanging", "Suicide", 'Execution by firing squad',
  "Concussion", 'Concussion | Suicide', "Decapitation", "Shot", "Killed", 'Ballistic trauma'
)

journalists_death_place_cause <- read_csv("journalists.csv") |>
  select(
    "ontology/birthPlace_label",
    "ontology/deathPlace_label",
    "ontology/deathCause_label",
    "ontology/deathYear"
  ) |>
  rename(
    birth_place = "ontology/birthPlace_label",
    death_place = "ontology/deathPlace_label",
    death_cause = "ontology/deathCause_label",
    death_year = "ontology/deathYear"
  ) |>
  mutate(
    death_year = as.numeric(death_year),
    bd_coincide = ifelse(birth_place == death_place,
                         "Same place",
                         "Different place"),
    cause_type = ifelse(death_cause %in% violent_causes, "Violent", "Non-violent")
  ) |>
  filter(!is.na(death_cause), death_year >= 1900)

plot_data <- journalists_death_place_cause |>
  group_by(bd_coincide, cause_type) |>
  summarise(count = n(), .groups = "drop")

# Stacked bar plot
death_place_cause_plot <- ggplot(plot_data) +
  aes(x = bd_coincide, y = count, fill = cause_type) +
  geom_col(fill = "#697fb3ff") +
  labs(
    title = "Journalists' place of death and type of death cause",
    subtitle = "DQ: How often do journalists die outside their country or place of birth,\nand what might this indicate about professional risk and mobility?",
    x = "Birth place and death place",
    y = "Number of journalists",
    fill = "Cause of death"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14))
#Also try:
 #aes(x = bd_coincide, fill = cause_type) +
  #geom_bar(position = "fill") +
  #facet_wrap(~ decade) +
  #scale_y_continuous(labels = scales::percent) 

ggsave("Death_place_vs_cause_type_stacked_bar.png")

# box plot: lifespan by place of death (mobility risk)

lifespan_place_plot <- read_csv("journalists.csv") |>
  select(
    "ontology/birthYear",
    "ontology/deathYear",
    "ontology/birthPlace_label",
    "ontology/deathPlace_label"
  ) |>
  na.omit() |>
  rename(
    birth_year = "ontology/birthYear",
    death_year = "ontology/deathYear",
    birth_place = "ontology/birthPlace_label",
    death_place = "ontology/deathPlace_label"
  ) |>
  mutate(
    birth_year = as.numeric(birth_year),
    death_year = as.numeric(death_year),
    life_length = death_year - birth_year,
    bd_coincide = ifelse(birth_place == death_place,
                         "Died at birthplace",
                         "Died elsewhere")
  ) |>
  filter(life_length > 0, death_year >= 1900) |>
  ggplot() +
  aes(x = bd_coincide, y = life_length, fill = bd_coincide) +
  geom_boxplot(alpha = 0.7, width = 0.6, fill = "#697fb3ff") +
  labs(
    x = "",
    y = "Journalists' lifespan (years)",
    title = "Journalists’ lifespan by place of death",
    subtitle = "DQ: How often do journalists die outside their country or place of birth,\nand what might this indicate about professional risk and mobility?",
 ) +
  theme_minimal() +
theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14)
    legend.position = "none")

ggsave("Journalists_lifespan_by_place_of_death.png", plot = lifespan_place_plot)

