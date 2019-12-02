library(ggplot2)
library(plyr) # dplyr
library(tidyverse)


# ou pour savoir lequel appeler:
# dplyr::filter (par exemple)
db = read.csv(file = "./data/subventions-accordees-et-refusees.csv", sep= ';')

#on transforme notre dataset pour récupérer chaque domaine d'activité individuellement (un seul par ligne, on duplique les lignes qui en ont plusieurs)
df <- db %>%
  transform(Secteurs.d.activités.définies.par.l.association = strsplit((as.character(Secteurs.d.activités.définies.par.l.association)),";")) %>% unnest(Secteurs.d.activités.définies.par.l.association)

#réorganisation du dataset en fonction des secteurs + on calcule la montant moyen des subventions acceptées
db_sector = ddply(df, .(Secteurs.d.activités.définies.par.l.association), summarize, nbAccept=sum(Montant.voté!="0"), Tot_Subv=sum(Montant.voté), moy_Tot_accept = (Tot_Subv/nbAccept)/1000)

#premier graphe - montant moyen subvention / secteur
ggplot(data = db_sector, aes(x=Secteurs.d.activités.définies.par.l.association, y = moy_Tot_accept)) + 
  coord_flip() + 
  geom_bar(stat = "identity", position="dodge", fill = "#660066") + 
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        legend.background = element_rect(fill="lightgray"), 
        legend.key = element_rect(fill="black", color = NA), 
        plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Secteurs d'activités") + ylab("Montant moyen d'une subvention (en milliers d'euros)") + labs(title = "Montant moyen d'une subvention en fonction du secteur d'activité (2013 - 2019)")

#deuxième graphe - nb accepté subvention / secteur
ggplot(data = db_sector, aes(x=Secteurs.d.activités.définies.par.l.association, y = nbAccept)) + 
  coord_flip() + scale_y_continuous()+
  geom_bar(stat = "identity", position="dodge", fill = "#CC3300") + 
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        legend.background = element_rect(fill="lightgray"), 
        legend.key = element_rect(fill="black", color = NA), 
        plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Secteurs d'activités") + ylab("Nombre de demandes de subventions acceptées") + labs(title = "Nombre de demandes de subvention acceptées en fonction du secteur d'activité (2013 - 2019)")

  
