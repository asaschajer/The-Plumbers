# Calculate the journalist life length
library(tidyverse)


# df["ontology/deathDate" == 'NULL'] <- NA
df <- read_csv('journalists.csv') |>
  select("title", "ontology/birthDate", "ontology/birthYear","ontology/occupation_label", "ontology/deathDate", "ontology/deathYear",)|>
  na.omit()|>
  rename('ontology_birthYear' = 'ontology/birthYear', 'ontology_deathYear' = 'ontology/deathYear' )|>
  mutate(life_length = ontology_deathYear-ontology_birthYear)|>
  print()
