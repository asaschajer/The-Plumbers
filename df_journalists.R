library(jsonlite)
library(tidyverse)

# Then we have to load JSON data
d <- fromJSON("journalists.json")

# Then convert  the JSON data to dataframe
df <- as.data.frame(d)|>
  select('ontology/deathCause_label') |>
  na.omit()


# At last print the output dataf
#print(df)
glimpse(d)

