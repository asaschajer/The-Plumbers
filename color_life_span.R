library(tidyverse)


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
  summarise(avr_life_span = mean(life_length), total = n()) |>
  ggplot()+
  aes(x= year, y = avr_life_span, fill = bd_coincide)+
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
    values = c("#697fb3ff", "#f2c45f"),
    labels = c("Died elsewhere", "Died at birthplace")
  )

ggsave('life_span_vs_location_death_BAR.png', plot = bar_plot,  width = 12, height = 8)
