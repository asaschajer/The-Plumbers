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
    y = "Share of deaths",
    fill = "Cause of death",
    title = "Violent vs non-violent deaths by place of death",
    subtitle = "Journalists who died abroad are compared to those who died at their birthplace"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )

violent_only <- journalists_violence |>
  filter(cause_type == "Violent")

ggplot(violent_only) +
  aes(x = bd_coincide, y = life_length, fill = bd_coincide) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, alpha = 0.3, color = "grey30") +
  scale_fill_manual(
    values = c(
      "Died at birthplace" = "#697fb3ff",
      "Died elsewhere" = "#c44e52"
    )
  ) +
  labs(
    x = "",
    y = "Lifespan (years)",
    title = "Lifespan of journalists who died violently",
    subtitle = "Comparison between deaths at birthplace and deaths elsewhere",
    fill = "Place of death"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )

violent_time <- journalists_violence |>
  filter(cause_type == "Violent") |>
  group_by(death_year, bd_coincide) |>
  summarise(
    mean_life_span = mean(life_length, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(violent_time) +
  aes(x = death_year, y = mean_life_span, color = bd_coincide) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = c(1918, 1945, 1991, 2001),
    linetype = "dashed",
    color = "grey40"
  ) +
  scale_color_manual(
    values = c(
      "Died at birthplace" = "#697fb3ff",
      "Died elsewhere" = "#c44e52"
    )
  ) +
  labs(
    x = "Year of death",
    y = "Average lifespan (years)",
    color = "Place of death",
    title = "Average lifespan of journalists who died violently over time",
    subtitle = "Trends aligned with major historical periods"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

#better

violent_counts <- journalists_violence |>
  filter(cause_type == "Violent") |>
  count(bd_coincide)

ggplot(violent_counts) +
  aes(x = bd_coincide, y = n, fill = bd_coincide) +
  geom_col(width = 0.5) +
  scale_fill_manual(
    values = c(
      "Died at birthplace" = "#697fb3ff",
      "Died elsewhere" = "#c44e52"
    )
  ) +
  labs(
    x = "",
    y = "Number of violent deaths",
    title = "Violent deaths among journalists by place of death",
    subtitle = "Raw counts shown due to small sample size",
    fill = "Place of death"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

violence_rate <- journalists_violence |>
  group_by(bd_coincide) |>
  summarise(
    violent = sum(cause_type == "Violent"),
    total = n(),
    proportion = violent / total
  )

ggplot(violence_rate) +
  aes(x = bd_coincide, y = proportion, fill = bd_coincide) +
  geom_col(width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Died at birthplace" = "#697fb3ff",
      "Died elsewhere" = "#c44e52"
    )
  ) +
  labs(
    x = "",
    y = "Proportion of violent deaths",
    title = "Share of violent deaths by place of death",
    subtitle = "Proportions shown with group sample sizes",
    fill = "Place of death"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

ggplot(violent_counts) +
  aes(x = bd_coincide, y = n) +
  geom_point(size = 4, color = "#c44e52") +
  geom_segment(
    aes(xend = bd_coincide, y = 0, yend = n),
    linewidth = 1
  ) +
  labs(
    x = "",
    y = "Number of violent deaths",
    title = "Violent deaths among journalists by place of death"
  ) +
  theme_minimal()

ggplot(journalists_violence) +
  aes(x = bd_coincide, fill = cause_type) +
  geom_bar(position = "fill") +
  facet_wrap(~ cause_type) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "",
    y = "Share of deaths",
    title = "Distribution of death causes by place of death"
  ) +
  theme_minimal()

overall_violent_share <- journalists_violence |>
  summarise(
    violent = sum(cause_type == "Violent"),
    total = n(),
    proportion = violent / total
  ) |>
  print()

# ggplot(journalists_bd) +
#   aes(x = bd_coincide, y = life_length, fill = bd_coincide) +
#   geom_violin(show.legend = 0, trim = 0) +
#   geom_boxplot(width=0.1, color="grey", alpha=0.2, show.legend = 0) +
#   geom_hline(
#     data = mean_bd,
#     aes(yintercept = mean_lifespan, color = bd_coincide),
#     linetype = "dashed",
#     linewidth = 1
#   ) +
#   scale_fill_manual(
#     values = c(
#       "Died at birthplace" = "#698fb3ff",
#       "Died elsewhere" = "#697fb3ff"
#     )
#   ) +
#   scale_color_manual(
#     values = c(
#       "Died at birthplace" = "#699fb3ff",
#       "Died elsewhere" = "#697fb3ff"
#     ),
#     labels = c(
#       "Died at birthplace" = "Mean lifespan (if died at birthplace)",
#       "Died elsewhere" = "Mean lifespan (if died elsewhere)"
#     ) 
#   ) +
#   labs(
#     fill = "Average lifespan",
#     color = "Average lifespan",
#     x = "",
#     y = "Journalists' lifespan (years)",
#     title = "Journalists’ lifespan by place of death",
#     subtitle = "DQ: How often do journalists die outside their country or place of birth,\nand what might this indicate about professional risk and mobility?"
#   ) +
#   theme_minimal() +
#   theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14),
#     legend.position = "right")

# ggsave("Journalists_lifespan_by_place_of_death_VIOLIN.png")

# line plot by place align with history, do violent death also die young?
#  


# journalists_death_place_cause <- all_journalists |>
#   mutate(
#     death_year = as.numeric(death_year),
#     bd_coincide = ifelse(birth_country == death_country,
#                          "Same place",
#                          "Different place"),
#     cause_type = ifelse(death_cause %in% violent_causes, "Violent", "Non-violent")
#   ) |>
#   filter(!is.na(death_cause), death_year >= 1900) |>
#   filter(!is.na(bd_coincide))

# plot_data <- journalists_death_place_cause |>
#   group_by(bd_coincide, cause_type) |>
#   summarise(count = n(), .groups = "drop")

# # plot_data_total <- journalists_death_place_cause |>
# #   group_by(bd_coincide) |>
# #   # summarise(count = n(), .groups = "drop") |>
# #   summarise(total = sum(count, na.rm=TRUE))

# bd_place_data <- all_journalists |>
#   mutate(
#     bd_coincide = as.integer(birth_country == death_country),
#   bd_differ = as.integer(birth_country != death_country)) 

# bd_total <- bd_place_data |>
#   pivot_longer(
#     cols = c(bd_coincide, bd_differ),
#     names_to = "birth_death",
#     values_to = "coincide_or_differ"
#   ) |>
#   group_by(birth_death) |>
#   summarise(total = sum(coincide_or_differ, na.rm=TRUE))

# # ggplot(bd_total) +
# #   aes(x = birth_death, y = total) +
# #   geom_col(position = 'dodge') +
# #   scale_fill_manual(
# #           values = c(
# #       "Non-violent" = "#697fb3ff",
# #       "Violent" = "#c44e52"
# #     )) +
# #   labs(
# #     title = "Journalists' place of death and type of death cause",
# #     subtitle = "DQ: How often do journalists die outside their country or place of birth,\nand what might this indicate about professional risk and mobility?",
# #     x = "Birth place and death place",
# #     y = "Number of journalists",
# #     fill = "Cause of death"
# #   ) +
# #   theme_minimal() +
# #   theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14)) 
  


# # # Stacked bar plot
# # ggplot(plot_data) +
# #   aes(x = bd_coincide, y = count / sum(count), fill = cause_type) +
# #   geom_col(position = 'dodge') +
# #   scale_fill_manual(
# #           values = c(
# #       "Non-violent" = "#697fb3ff",
# #       "Violent" = "#c44e52"
# #     )) +
# #   labs(
# #     title = "Journalists' place of death and type of death cause",
# #     subtitle = "DQ: How often do journalists die outside their country or place of birth,\nand what might this indicate about professional risk and mobility?",
# #     x = "Birth place and death place",
# #     y = "Number of journalists",
# #     fill = "Cause of death"
# #   ) +
# #   theme_minimal() +
# #   theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14)) 
  

# #Also try:
#  #aes(x = bd_coincide, fill = cause_type) +
#   #geom_bar(position = "fill") +
#   #facet_wrap(~ decade) +
#   #scale_y_continuous(labels = scales::percent) 

# #ggsave("Death_place_vs_cause_type_stacked_bar.png")

# # box plot: lifespan by place of death (mobility risk)

