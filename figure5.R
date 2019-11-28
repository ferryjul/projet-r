library(ggplot2)
library(plyr) # dplyr
require(reshape2)
# ou pour savoir lequel appeler:
# dplyr::filter (par exemple)
db =  read.csv(file = "./data/subventions-versees.csv", sep= ';')
db[is.na(db)] <- 0

dbCountSummary = ddply(db, .(Publication, Nature.juridique), summarize, montantTot=sum(Montant..de.la.subvention!="0"))

