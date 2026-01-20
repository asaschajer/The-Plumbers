# Calculate the Journalists' life length

library(jsonlite)
library(tidyverse)

# Then we have to load JSON data
d <- fromJSON("journalists.json")

# Then convert  the JSON data to dataframe
# df["ontology/deathDate" == 'NULL'] <- NA
df <- as.data.frame(d) |>
  select("title", "ontology/birthDate", "ontology/birthYear","ontology/occupation_label", "ontology/deathDate", "ontology/deathYear",)|>
  na.omit()|>
  rename('ontology_birthYear' = 'ontology/birthYear', 'ontology_deathYear' = 'ontology/deathYear' )|>
  mutate(life_length = ontology_deathYear-ontology_birthYear)|>
  print()
