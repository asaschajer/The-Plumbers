library(tidyverse)
library(dplyr)

d <- read_csv("journalists.csv")
# Then convert  the JSON data to dataframe
df <- d|>
  select(death_cause ='ontology/deathCause_label', death_place ='ontology/deathPlace_label')|>
  na.omit()|>
  count(death_cause, sort = TRUE)|>
  rename(death_counts = n)|>
  #filter(death_counts > 1)
  #filter(death_counts != 'Systemic lupus erythematosus' |death_counts != 'Stroke'|death_counts !='Scrub typhus'|death_counts !='Respiratory disease'|death_counts !='Pneumonia'|death_counts !='Mesothelioma'|death_counts !='Intracranial aneurysm'|death_counts !='Endocarditis'|death_counts !='Apoplexy'|death_counts !='Aorta | Aneurysm'|death_counts !='Amyotrophic lateral sclerosis')
  filter(death_cause == 'Brain tumor'|death_cause =='Cancer'|death_cause =='Assassination'|death_cause =='Decapitation'|death_cause =='Brain tumor'|death_cause =='Colorectal cancer'|death_cause =='Myocardial infarction'|death_cause =='Renal failure'|death_cause =='Ballistic trauma'|death_cause =='Colorectal cancer'|death_cause =='Concussion | Suicide'|death_cause =='Esophageal cancer'|death_cause =='Execution by firing squad'|death_cause =='Hanging'|death_cause =='Sniper'|death_cause =='Suicide')

print(df, n=40)

#Exploring data set
#glimpse(d)
#distinct(data_frame)
#head(data_frame)

ggplot(data = df) +
  aes(x = death_cause, y=death_counts) +
    labs(x="Death Cause", y='Number of deaths')+
  geom_bar(stat="summary")
ggsave("proposal_plot.pdf",width=10,height=10)


new_df <- df|>
  pivot_wider(names_from=death_cause, values_from=death_counts)|>
  group_by('Assassination', 'Decapitation','')
  
print(new_df)