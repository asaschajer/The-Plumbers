
# Calculate the journalist life length
library(tidyverse)

journalists <- read_csv('journalists.csv') |>
  select("title", 
    "ontology/occupation_label", 
    "ontology/birthYear", 
    "ontology/deathYear", 
    'ontology/birthPlace_label',
    'ontology/deathPlace_label'
  ) |>
  rename(
    'occupation' = 'ontology/occupation_label',
    'birth_year' = 'ontology/birthYear', 
    'death_year' = 'ontology/deathYear', 
    'birth_place' = 'ontology/birthPlace_label', 
    'death_place' = 'ontology/deathPlace_label'
  ) |>
  mutate(
    birth_year = as.numeric(birth_year), 
    death_year = as.numeric(death_year), 
  ) |>  
  mutate(
    bd_coincide = (birth_place == death_place),
    life_length = death_year - birth_year
  ) |>
  filter(death_year >= 1900)|>


all_journalists <- journalists |>
  na.omit()|>
  left_join(read_csv("journalists_country.csv"), by ='death_place')
  # select(life_length, birth_country, death_country, death_year, birth_year, -death_place, -birth_place)

# journalists_country <- read_csv('journalists.csv') |>
#   select('ontology/deathPlace_label', "ontology/deathYear", 'ontology/birthPlace_label', "ontology/birthYear")|>
#   na.omit()

# write.csv(journalists_country, file = "Journalists_country.csv", row.names = FALSE)

# life_length_plot <- read_csv('journalists.csv') |>
#   select("title", "ontology/occupation_label", "ontology/birthYear", "ontology/deathYear")|>
#   na.omit()|>
#   rename('ontology_birthYear' = 'ontology/birthYear', 'ontology_deathYear' = 'ontology/deathYear' )|>
#   mutate(ontology_birthYear = as.numeric(ontology_birthYear), ontology_deathYear = as.numeric(ontology_deathYear))|>
#   mutate(life_length = ontology_deathYear - ontology_birthYear)|>
#   filter(life_length > 0)|>
#   ggplot()+
#   aes(x = life_length)+
#   labs(x = "Life lengths (Yrs)", y = 'Number of journalists', title = 'Differences in life lengths among journalists', subtitle = 'DQ: Are there any distinct differences in the life lengths of different journalists?' )+
#   theme(plot.title = element_text(face = "bold", size = 14))+
#   geom_bar(position = 'dodge', fill = "#697fb3ff")

# ggsave('life_length.png', plot = life_length_plot, width = 10, height = 5)

#avr life span of journalist per year
avr_life_span <- read_csv('journalists.csv') |>
  select("title", "ontology/occupation_label", "ontology/birthYear", "ontology/deathYear")|>
  na.omit()|>
  rename('ontology_birthYear' = 'ontology/birthYear', 'ontology_deathYear' = 'ontology/deathYear' )|>
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

# year_cat <- function(death_year) {
#   if (death_year <= 1910){
#     return("1900-1910")
#   } else if (death_year <= 1920) {
#     return("1910-1920")
#   } else if (death_year <= 1930) {
#     return("1920-1930")
#   } else if (death_year <= 1940) {
#     return("1930-1940")
#   } else if (death_year <= 1950) {
#     return("1940-1950")
#   } else if (death_year <= 1960) {
#     return("1950-1960")
#   } else if (death_year <= 1970) {
#     return("1960-1970")
#   } else if (death_year <= 1980) {
#     return("1970-1980")
#   } else if (death_year <= 1990) {
#     return("1980-1990")
#   } else if (death_year <= 2000) {
#     return("1990-2000")
#   } else if (death_year <= 2010) {
#     return("2000-2010")
#   } else if (death_year <= 2020) {
#     return("2010-2020")
#   } else {
#     return("no info")
#   }
# }

# year_grouped <- all_journalists |>
#   mutate(year = sapply(death_year, year_cat))

# #Avr life span of journalist dying abroad annd dying in home country throughout the years
# bar_plot <- year_grouped |>
#   select('bd_coincide', 'life_length', 'year') |> 
#   na.omit() |>
#   group_by(bd_coincide, year) |>
#   summarise(avr_life_span = mean(life_length), total = n()) |>
#   ggplot()+
#   aes(x= year, y = avr_life_span, fill = bd_coincide)+
#   labs(x = "Years", y = "Average life span (Yrs)", title = "Journalists' average life span based on death place")+
#   labs(fill = "Location of death")+
#   scale_fill_discrete(labels = c("Died abroad", "Died in home country"))+
#   geom_col(position = position_dodge(preserve = "single"))


# active_years <- read_csv('journalists.csv') |>
#   select("title", "ontology/deathYear", 'ontology/activeYears', 'ontology/activeYearsEndDate', 'ontology/activeYearsStartDate', 'ontology/activeYearsEndYear', 'ontology/activeYearsStartYear') |>
#   rename(deathYear = "ontology/deathYear", 'endYear' = 'ontology/activeYearsEndYear')

# death_on_duty_data <- active_years |>
#     mutate(
#     died_on_duty = as.integer(endYear == deathYear),
#   lived = as.integer(endYear != deathYear)) |>
#   pivot_longer(
#     cols = c(died_on_duty, lived),
#     names_to = "death_on_duty",
#     values_to = "on_duty_or_not"
#   ) |>
#   group_by(death_on_duty) |>
#   summarise(total = sum(on_duty_or_not, na.rm=TRUE))


# ggplot(death_on_duty_data) +
#   aes(x = death_on_duty, y = total / sum(total)) +
#   scale_y_continuous(labels = scales::label_percent()) +
#     labs (
#     title = 'Journalists deceased on duty',
#     subtitle = 'DQ: How often do journalists die while still being officially employed?',
#     y = 'Number of journalists', # whose birth and death locations differ/coincide
#     x = ''
#   ) +
#   scale_x_discrete(labels = c('Died on duty', 'Died after retirement')) +
#   theme_minimal() +
#   theme(plot.title = element_text(face = 'bold', size = 14), axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9), axis.title = element_text(size = 14))+
#   geom_col(position = 'dodge', fill = "#697fb3ff") 

# ggsave('died_on_duty.png')

