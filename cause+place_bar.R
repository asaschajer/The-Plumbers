library(tidyverse)

# Define violent death causes
violent_causes <- c(
  "Assassination", "Sniper", "Hanging", "Suicide", 'Execution by firing squad',
  "Concussion", 'Concussion | Suicide', "Decapitation", "Shot", "Killed", 'Ballistic trauma'
)

journalists <- read_csv('journalists.csv') |>
  rename(
    birth_year = `ontology/birthYear`, 
    death_year = `ontology/deathYear`, 
    birth_place = `ontology/birthPlace_label`, 
    death_place = `ontology/deathPlace_label`,
    death_cause = "ontology/deathCause_label"
  ) |>
  select( 
    birth_year, 
    death_year, 
    birth_place, 
    death_place,
    death_cause
  )

all_journalists <- journalists |>
  na.omit()|>
  left_join(read_csv("all_country_mapping.csv"), by ='death_place')|>
  select( 
    birth_country, 
    death_country, 
    death_year, 
    birth_year,
    death_cause
  ) |>
  mutate(
    birth_year = as.numeric(birth_year), 
    death_year = as.numeric(death_year), 
  ) |>  
  mutate(
    bd_coincide = (birth_country == death_country),
    life_length = death_year - birth_year
  ) |>
  filter(death_year >= 1900)

journalists_death_place_cause <- all_journalists |>
  mutate(
    death_year = as.numeric(death_year),
    bd_coincide = ifelse(birth_country == death_country,
                         "Same place",
                         "Different place"),
    cause_type = ifelse(death_cause %in% violent_causes, "Violent", "Non-violent")
  ) |>
  filter(!is.na(death_cause), death_year >= 1900) |>
  filter(!is.na(bd_coincide))

plot_data <- journalists_death_place_cause |>
  group_by(bd_coincide, cause_type) |>
  summarise(count = n(), .groups = "drop")

# plot_data_total <- journalists_death_place_cause |>
#   group_by(bd_coincide) |>
#   # summarise(count = n(), .groups = "drop") |>
#   summarise(total = sum(count, na.rm=TRUE))

bd_place_data <- all_journalists |>
  mutate(
    bd_coincide = as.integer(birth_country == death_country),
  bd_differ = as.integer(birth_country != death_country)) 

bd_total <- bd_place_data |>
  pivot_longer(
    cols = c(bd_coincide, bd_differ),
    names_to = "birth_death",
    values_to = "coincide_or_differ"
  ) |>
  group_by(birth_death) |>
  summarise(total = sum(coincide_or_differ, na.rm=TRUE))

journalists_bd <- all_journalists |>
  mutate(
    birth_year = as.numeric(birth_year),
    death_year = as.numeric(death_year),
    life_length = death_year - birth_year,
    bd_coincide = ifelse(birth_country == death_country,
                         "Died at birthplace",
                         "Died elsewhere")
  ) |>
  filter(life_length > 0, death_year >= 1900) 

mean_bd <- journalists_bd |>
  group_by(bd_coincide) |>
  summarise(mean = mean(life_length))

ggplot(journalists_bd) +
  aes(x = bd_coincide, y = life_length, fill = bd_coincide) +
  geom_boxplot(show.legend = 0) +
  geom_hline(
    data = mean_bd,
    aes(yintercept = mean, color = bd_coincide),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_fill_manual(
    values = c(
      "Died at birthplace" = "#697fb3ff",
      "Died elsewhere" = "#c44e52"
    )
  ) +
  scale_color_manual(
    values = c(
      "Died at birthplace" = "#697fb3ff",
      "Died elsewhere" = "#c44e52"
    ),
    labels = c(
      "Died at birthplace" = "Mean lifespan (if died at birthplace)",
      "Died elsewhere" = "Mean lifespan (if died elsewhere)"
    ) 
  ) +
  labs(
    fill = "Average lifespan",
    color = "Average lifespan",
    x = "",
    y = "Journalists' lifespan (years)",
    title = "Journalists’ lifespan by place of death",
    subtitle = "DQ: How often do journalists die outside their country or place of birth,\nand what might this indicate about professional risk and mobility?"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14),
    legend.position = "right")

ggsave("Journalists_lifespan_by_place_of_death.png")

# ggplot(bd_total) +
#   aes(x = birth_death, y = total) +
#   geom_col(position = 'dodge') +
#   scale_fill_manual(
#           values = c(
#       "Non-violent" = "#697fb3ff",
#       "Violent" = "#c44e52"
#     )) +
#   labs(
#     title = "Journalists' place of death and type of death cause",
#     subtitle = "DQ: How often do journalists die outside their country or place of birth,\nand what might this indicate about professional risk and mobility?",
#     x = "Birth place and death place",
#     y = "Number of journalists",
#     fill = "Cause of death"
#   ) +
#   theme_minimal() +
#   theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14)) 
  


# # Stacked bar plot
# ggplot(plot_data) +
#   aes(x = bd_coincide, y = count / sum(count), fill = cause_type) +
#   geom_col(position = 'dodge') +
#   scale_fill_manual(
#           values = c(
#       "Non-violent" = "#697fb3ff",
#       "Violent" = "#c44e52"
#     )) +
#   labs(
#     title = "Journalists' place of death and type of death cause",
#     subtitle = "DQ: How often do journalists die outside their country or place of birth,\nand what might this indicate about professional risk and mobility?",
#     x = "Birth place and death place",
#     y = "Number of journalists",
#     fill = "Cause of death"
#   ) +
#   theme_minimal() +
#   theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14)) 
  

#Also try:
 #aes(x = bd_coincide, fill = cause_type) +
  #geom_bar(position = "fill") +
  #facet_wrap(~ decade) +
  #scale_y_continuous(labels = scales::percent) 

#ggsave("Death_place_vs_cause_type_stacked_bar.png")

# box plot: lifespan by place of death (mobility risk)

