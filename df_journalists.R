library(jsonlite)
library(tidyverse)

# Then we have to load JSON data
d <- fromJSON("journalists.json")

# Then convert  the JSON data to dataframe
<<<<<<< HEAD
df <- as.data.frame(d)|>
  select('ontology/deathCause_label') |>
  na.omit()

=======
df <- as.data.frame(d) |>
  # select('ontology/deathPlace_label')
>>>>>>> acdadacfa4e141cd5ced9a09cd723cf0b3daa791

# At last print the output dataf
#print(df)
glimpse(d)

