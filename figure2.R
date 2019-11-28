library(ggplot2)
library(plyr) # dplyr
require(reshape2)
# ou pour savoir lequel appeler:
# dplyr::filter (par exemple)
db = read.csv(file = "./data/subventions-accordees-et-refusees.csv", sep= ';')

dbCountSector = ddply(db, .(Secteurs.d.activités.définies.par.l.association), summarize, sum_nb=sum(Montant.voté))


strsplit("Aides aux associations;Architecture & urbanisme;Cu",";")


act_sector = unique(unlist(strsplit((as.character(db$Secteurs.d.activités.définies.par.l.association)),";")))

bSummedTot = ddply(db, .(act_sector), summarize, nbAccept=sum(Montant.voté!="0"))

