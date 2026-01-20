
library(jsonlite)
library(tidyverse)

# Then we have to load JSON data
d <- fromJSON("journalists.json")

# Then convert  the JSON data to dataframe
df <- as.data.frame(d) |>
  # select('ontology/deathPlace_label')

# At last print the output dataf
print(df)
