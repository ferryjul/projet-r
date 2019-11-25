library(ggplot2)
library(plyr) # dplyr
require(reshape2)
# ou pour savoir lequel appeler:
# dplyr::filter (par exemple)
db = read.csv(file = "./data/subventions-accordees-et-refusees.csv", sep= ';')

dbCountSummary = ddply(db, .(Année.budgétaire), summarize, nbAccept=sum(Montant.voté!="0"), nbRefuse=sum(Montant.voté=="0"))
dbSummedTot = ddply(db, .(Année.budgétaire), summarize, sum_nb=sum(Montant.voté))
dbCountMelt = melt(dbCountSummary, id.vars="Année.budgétaire")

ggplot(data = dbCountMelt, aes(x=factor(Année.budgétaire), y = value, fill = variable)) + 
  geom_bar(stat = "identity", position="stack") + # ou dodge pour côte à côte 
  scale_fill_manual(name="Nombre de demandes de subvention :", labels=c("Acceptées", "Refusées"), values=c("#74d712", "#f21e1e")) +
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        legend.background = element_rect(fill="lightgray"), 
        legend.key = element_rect(fill="black", color = NA), 
        plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab("Année budgétaire") + ylab("Nombre de subventions") + labs(title = "Subventions à Paris")#, subtitle = "Evolution depuis 2013")
