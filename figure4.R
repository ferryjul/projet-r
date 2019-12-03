library(ggplot2)
library(plyr) # dplyr
library(tidyverse)
library(grid)


db = read.csv(file = "./data/subventions-accordees-et-refusees.csv", sep= ';')

#on transforme notre dataset pour récupérer chaque domaine d'activité individuellement (un seul par ligne, on duplique les lignes qui en ont plusieurs)
df <- db %>%
  transform(Secteurs.d.activités.définies.par.l.association = strsplit((as.character(Secteurs.d.activités.définies.par.l.association)),";")) %>% unnest(Secteurs.d.activités.définies.par.l.association)

#réorganisation du dataset en fonction des secteurs + on calcule la montant moyen des subventions acceptées
db_sector = ddply(df, .(Secteurs.d.activités.définies.par.l.association), summarize, Tot_Subv=sum(Montant.voté), proportion_sector = 1)


total_subv = sum(db_sector$Tot_Subv)

db_sector$proportion_sector = db_sector$Tot_Subv / total_subv

db_sector_filter <- db_sector %>% filter(proportion_sector >= 0.025)



db_sector_pie <- rbind(db_sector_filter,data.frame("Secteurs.d.activités.définies.par.l.association"= "Autres","Tot_Subv"=0,"proportion_sector"=1-sum(db_sector_filter$proportion_sector)))

db_sector_pie$data_labeli <- scales::percent(db_sector_pie$proportion_sector)
  
ggplot(db_sector_pie)+
  geom_bar(aes(x="", y=proportion_sector, fill=Secteurs.d.activités.définies.par.l.association), width = 1, stat = "identity")+ coord_polar("y") +
  theme_void() + geom_text(aes(x= 1, y=proportion_sector, label = data_labeli), position=position_stack(vjust=0),size=4)
                                             
db_sector_pie %>% ggplot(aes(x=1, y=proportion_sector, fill=Secteurs.d.activités.définies.par.l.association)) +
  geom_col() +
  geom_text(aes(x=1.3,label=data_labeli), position=position_stack(vjust = 0.5), size=3) +
  coord_polar(theta = "y") + scale_fill_brewer(palette="Paired",name="Secteur d'activité")+ 
  theme(axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),  axis.title.x = element_blank(),axis.title.y = element_blank())+ labs(title="Proportion des différents secteurs d'activités dans le montant total des subventions versés")
