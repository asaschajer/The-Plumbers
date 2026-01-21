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
ggsave("Causes_of_death_ungrouped.pdf",width=10,height=10)

new_df <- df|>
  pivot_wider(names_from=death_cause, values_from=death_counts)|>
  mutate(`Hazardous deaths`= Assassination + Decapitation+`Ballistic trauma`+ `Concussion | Suicide` +`Execution by firing squad`+ Hanging + Sniper +Suicide)|>
  pivot_longer(c(`Hazardous deaths`,Cancer,`Myocardial infarction`, `Renal failure`,`Brain tumor`, `Colorectal cancer`, `Esophageal cancer`))|>
  rename(Death_cause=name, Death_counts=value)|>
  select(Death_cause,Death_counts)|>
  arrange(desc(Death_counts))
  #grouped_hazard_deaths =group_by('Assassination', 'Decapitation','Ballistic trauma','Concussion|Suicide','Hanging','Sniper','Suicide')

print(new_df)

ggplot(data = new_df) +
  # arrange(desc(new_df))+
  aes(x =Death_counts,y= reorder(Death_cause,Death_counts)) +
    labs(x="Death count", y="Death cause", title = "Count of Hazardous and non hazardous deaths", subtitle= "DQ:How prevalent are violent death causes (assassination, sniper, hang,
suicide, concussion, decapitation) among journalists?")+
  theme(plot.title = element_text(face="bold", size= 14))+
  geom_col()+
  scale_x_continuous(breaks = seq(0, 12, by = 1))
ggsave("Causes_of_death_grouped.png")

