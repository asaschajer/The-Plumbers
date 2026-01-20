# 
top_year_place <- journalists_small |>
  filter(`ontology/deathYear` == `ontology/deathYear`) |>
  filter(`ontology/deathPlace_label` == `ontology/deathPlace_label`) |>
  filter(`ontology/deathPlace_label` != "") |>
  group_by(`ontology/deathYear`, `ontology/deathPlace_label`) |>
  summarise(n = n()) |>
  arrange(desc(n))
#maybe it is useful to see years and places pair 
top_year_place