
# Calculate the journalist life length
library(tidyverse)
library(dplyr)

journalists <- read_csv('journalists.csv') |>
  mutate(
    birth_year = `ontology/birthYear`, 
    death_year = `ontology/deathYear`, 
    birth_place = `ontology/birthPlace_label`, 
    death_place = `ontology/deathPlace_label`
  ) |>
  select( 
    birth_year, 
    death_year, 
    birth_place, 
    death_place
  )

all_journalists <- journalists |>
  na.omit()|>
  left_join(read_csv("all_country_mapping.csv"), by ='death_place')|>
  select( 
    birth_country, 
    death_country, 
    death_year, 
    birth_year
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

avr_life_span <- read_csv('journalists.csv') |>
  select("title", "ontology/occupation_label", "ontology/birthYear", "ontology/deathYear")|>
  na.omit()|>
  rename(ontology_birthYear = `ontology/birthYear`, ontology_deathYear = `ontology/deathYear` )|>
  mutate(ontology_birthYear = as.numeric(ontology_birthYear), ontology_deathYear = as.numeric(ontology_deathYear))|>
  mutate(life_length = ontology_deathYear - ontology_birthYear)|>
  group_by(ontology_deathYear)|>
  filter(life_length > 0 & ontology_deathYear > 1900)|>
  summarise(avr_life_span = mean(life_length))

avr_life_span_plot <- avr_life_span |>
  ggplot() +
  aes(x = ontology_deathYear, y = avr_life_span) +
  geom_line(color = "#697fb3ff", linewidth = 1) +
  geom_vline(
    xintercept = c(1918, 1945, 1991, 2001),
    linetype = "dashed",
    color = "grey40"
  ) +
  annotate(
    "text",
    x = c(1914, 1940, 1965, 1996, 2010),
    y = max(avr_life_span, na.rm = TRUE),
    label = c("Pre-WWI", "Pre–WWII", "Cold War", "Post–Cold War", "Post-2001"),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    x = "Year of death",
    y = "Average journalist lifespan (years)",
    title = "Journalists’ average lifespan across historical eras",
    subtitle = "DQ: Are there any distinct differences in the life lengths of different journalists?\nAverage lifespan trends aligned with major geopolitical periods"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave('his_eras_avr_life_span.png', plot = avr_life_span_plot, width = 10, height = 5)

year_cat <- function(death_year) {
  if (death_year <= 1910){
    return("1900-1910")
  } else if (death_year <= 1920) {
    return("1910-1920")
  } else if (death_year <= 1930) {
    return("1920-1930")
  } else if (death_year <= 1940) {
    return("1930-1940")
  } else if (death_year <= 1950) {
    return("1940-1950")
  } else if (death_year <= 1960) {
    return("1950-1960")
  } else if (death_year <= 1970) {
    return("1960-1970")
  } else if (death_year <= 1980) {
    return("1970-1980")
  } else if (death_year <= 1990) {
    return("1980-1990")
  } else if (death_year <= 2000) {
    return("1990-2000")
  } else if (death_year <= 2010) {
    return("2000-2010")
  } else if (death_year <= 2020) {
    return("2010-2020")
  } else {
    return("no info")
  }
}

year_grouped <- all_journalists |>
  mutate(year = sapply(death_year, year_cat))

#Avr life span of journalist dying abroad annd dying in home country throughout the years
bar_plot <- year_grouped |>
  select('bd_coincide', 'life_length', 'year') |> 
  na.omit() |>
  group_by(bd_coincide, year) |>
  summarise(avr_life_span = mean(life_length), total = n())

mean_bd_year <- all_journalists |>
  mutate(
    bd_coincide = ifelse(
      birth_country == death_country,
      "Died at birthplace",
      "Died elsewhere"
    )
  ) |>
  filter(
    life_length > 0,
    death_year >= 1900
  ) |>
  group_by(death_year, bd_coincide) |>
  summarise(
    mean_life_span = mean(life_length, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(mean_bd_year) +
  aes(
    x = death_year,
    y = mean_life_span,
    color = bd_coincide
  ) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = c(1918, 1945, 1991, 2001),
    linetype = "dashed",
    color = "grey40"
  ) +
  annotate(
    "text",
    x = c(1914, 1940, 1965, 1996, 2010),
    y = max(mean_bd_year$mean_life_span, na.rm = TRUE),
    label = c(
      "Pre-WWI",
      "Pre–WWII",
      "Cold War",
      "Post–Cold War",
      "Post-2001"
    ),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_color_manual(
    values = c(
      "Died at birthplace" = "#699fb3ff",
      "Died elsewhere" = "#697fb3ff"
    )
  ) +
  labs(
    x = "Year of death",
    y = "Average journalist lifespan (years)",
    color = "Place of death",
    title = "Journalists’ average lifespan by place of death over time",
    subtitle = "Average lifespan trends aligned with major historical periods"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave('life_span_vs_location_death_LINE.png', width = 12, height = 8)

  ggplot(year_grouped)+
  aes(x= year, y = life_length, fill = bd_coincide)+
  geom_col(position = position_dodge(preserve = "single"))+
  labs(
    x = "Years",
    y = "Average life span (Yrs)", 
    title = "Journalists' average life span based on death place",
    subtitle = "DQ: Do journalists who died abroad have a shorter life expectency of journalists\nwho died in their home country?"
  )+
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )+
  labs(fill = "Location of death")+
  scale_fill_manual(
    values = c("#697fb3ff", "#699fb3ff"),
    labels = c("Died elsewhere", "Died at birthplace")
  )
  

ggsave('life_span_vs_location_death_BAR.png',  width = 12, height = 8)
