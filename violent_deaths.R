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


journalists_violence <- all_journalists |>
  filter(
    !is.na(death_cause),
    life_length > 0,
    death_year >= 1900
  ) |>
  mutate(
    bd_coincide = ifelse(
      birth_country == death_country,
      "Died at birthplace",
      "Died elsewhere"
    ),
    cause_type = ifelse(
      death_cause %in% violent_causes,
      "Violent",
      "Non-violent"
    )
  )

violence_share <- journalists_violence |>
  group_by(bd_coincide, cause_type) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(bd_coincide) |>
  mutate(prop = n / sum(n))

ggplot(violence_share) +
  aes(x = bd_coincide, y = prop, fill = cause_type) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "Violent" = "#c44e52",
      "Non-violent" = "#697fb3ff"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "",
    y = "Proportion of deaths",
    fill = "Cause of death",
    title = "Violent vs non-violent deaths compared by place of death",
    subtitle = "DQ: What proportion of journalists’ deaths are from violent causes (assassination,\ndecapitation, suicide, etc.) based on the deaths’ locations?"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )

ggsave('violent1_bd_place_proportion_STACKED.png')

violent_only <- journalists_violence |>
  filter(cause_type == "Violent")

ggplot(violent_only) +
  aes(x = bd_coincide, y = life_length, fill = bd_coincide) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, alpha = 0.3, color = "grey30") +
  scale_fill_manual(
    values = c(
      "Died at birthplace" = "#f2c45f",
      "Died elsewhere" = "#697fb3ff"
    )
  ) +
  labs(
    x = "",
    y = "Lifespan (years)",
    title = "Lifespan of journalists who died violently",
    subtitle = "H: Among journalists who died violently, those who died abroad die younger due to the high risk of the profession",
    fill = "Place of death"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10),
    legend.position = 'none'
  )

ggsave('violent3_lifespan distribution_VIOLIN.png')

violent_time <- journalists_violence |>
  filter(cause_type == "Violent") |>
  group_by(death_year, bd_coincide) |>
  summarise(
    mean_life_span = mean(life_length, na.rm = TRUE),
    .groups = "drop"
  )


#better

violent_counts <- journalists_violence |>
  filter(cause_type == "Violent") |>
  count(bd_coincide)


violence_rate <- journalists_violence |>
  group_by(bd_coincide) |>
  summarise(
    violent = sum(cause_type == "Violent"),
    total = n(),
    proportion = violent / total
  )


overall_violent_share <- journalists_violence |>
  count(cause_type) |>
  mutate(
    proportion = n / sum(n)
  )

ggplot(overall_violent_share) +
  aes(x = "All journalists", y = proportion, fill = cause_type) +
  geom_col(width = 0.5) +
  scale_fill_manual(
    values = c(
      "Violent" = "#c44e52",
      "Non-violent" = "#697fb3ff"
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "",
    y = "Proportion of deaths",
    fill = "Cause of death",
    title = "Proportion of journalists’ deaths by cause type",
    subtitle = "DQ: What proportion of journalists’ deaths are from violent causes (assassination,\ndecapitation, suicide, etc.)?"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 10)
  )

ggsave('violent2_overall_proportion_STACKED.png')
