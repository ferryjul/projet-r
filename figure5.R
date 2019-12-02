library(ggplot2)
library(plyr) # dplyr

db =  read.csv(file = "./data/subventions-versees.csv", sep= ';')
db[is.na(db)] <- 0
db$Nature.juridique = iconv(db$Nature.juridique, to="ASCII//TRANSLIT") # Remove accents to avoid duplicates
db$Nature.juridique[db$Nature.juridique == "Entreprises"] <- "Entreprise" # To avoid duplicates

# ======= PLOTS AVEC LES NOMBRES DE SUBVENTIONS ==============
dbCountSummary = ddply(db, .(Publication, Nature.juridique), summarize, montantTot=sum(Montant..de.la.subvention!="0"))

ggplot(data = dbCountSummary, aes(x=factor(Publication), y = montantTot, group = Nature.juridique, color=Nature.juridique)) + geom_point() + geom_line() +
  scale_color_discrete(name="")  +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.background = element_rect(fill="lightgray"), 
        legend.key = element_rect(fill="black", color = NA), 
        plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Année du Compte Administratif") + ylab("Nombre de subventions") + labs(title = "Evolution du nombre de subventions à Paris", subtitle = "Toutes natures juridiques")#+ theme(legend.position = "none") # ou dodge pour côte à côte 

dbCountSummaryBis <- dbCountSummary
dbCountSummaryBis <- dbCountSummaryBis[which(dbCountSummaryBis$Nature.juridique != "Personnes physiques"),]
dbCountSummaryBis <- dbCountSummaryBis[which(dbCountSummaryBis$Nature.juridique != "Associations"),]

ggplot(data = dbCountSummaryBis, aes(x=factor(Publication), y = montantTot, group = Nature.juridique, color=Nature.juridique)) + geom_point() + geom_line() +
  scale_color_discrete(name="Nature juridique")  + theme(legend.position = "bottom", legend.direction = "horizontal",
                                                         legend.background = element_rect(fill="lightgray"), 
                                                         legend.key = element_rect(fill="black", color = NA), 
                                                         plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Année du Compte Administratif") + ylab("Nombre de subventions") + labs(title = "Evolution du nombre de subventions à Paris", subtitle = "Hors personnes physiques et associations")#+ theme(legend.position = "none") # ou dodge pour côte à côte 
# ======= PLOTS AVEC LES MONTANTS DES SUBVENTIONS EN € ==============
dbCountSummaryMontant = ddply(db, .(Publication, Nature.juridique), summarize, montantTot=sum(Montant..de.la.subvention))
dbCountSummaryMontant$montantTot = dbCountSummaryMontant$montantTot

ggplot(data = dbCountSummaryMontant, aes(x=factor(Publication), y = montantTot, group = Nature.juridique, color=Nature.juridique)) + geom_point() + geom_line() +
  scale_color_discrete(name="")  +
  theme(legend.position = "bottom", legend.direction = "horizontal",legend.background = element_rect(fill="lightgray"), legend.key = element_rect(fill="black", color = NA),
        plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Année du Compte Administratif") + ylab("Montant total des subventions (euros)")# + labs(title = "Evolution des subventions à Paris", subtitle = "Toutes natures juridiques")#+ theme(legend.position = "none") # ou dodge pour côte à côte 
dbCountSummaryMontant <- dbCountSummaryMontant[which(dbCountSummaryMontant$Nature.juridique != "Associations"),]
dbCountSummaryMontant <- dbCountSummaryMontant[which(dbCountSummaryMontant$Nature.juridique != "Etablissements publics"),]
dbCountSummaryMontant <- dbCountSummaryMontant[which(dbCountSummaryMontant$Nature.juridique != "Entreprise"),]
ggplot(data = dbCountSummaryMontant, aes(x=factor(Publication), y = montantTot, group = Nature.juridique, color=Nature.juridique)) + geom_point() + geom_line() +
  scale_color_discrete(name="Nature juridique")  + theme(legend.position = "bottom", legend.direction = "horizontal",legend.background = element_rect(fill="lightgray"), legend.key = element_rect(fill="black", color = NA),
        plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Année du Compte Administratif") + ylab("Montant total des subventions (k€)") + labs(title = "Evolution des subventions à Paris", subtitle = "Hors associations, entreprises et établissements publics")#+ theme(legend.position = "none") # ou dodge pour côte à côte 
