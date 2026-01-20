library(tidyverse)
library(jsonlite)

# Then we have to load JSON data
d <- fromJSON("journalists.json")

# Then convert  the JSON data to dataframe
df <- as.data.frame(d) |>
  select('ontology/deathCause_label') |>
  

# At last print the output dataf
print(df)

#Exploring data set
glimpse(df)
#distinct(data_frame)
#head(data_frame)


