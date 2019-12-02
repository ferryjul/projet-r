library(ggplot2)
library(plyr) # dplyr

library(tidyverse)
# ou pour savoir lequel appeler:
# dplyr::filter (par exemple)
db = read.csv(file = "./data/subventions-accordees-et-refusees.csv", sep= ';')

df=db


df$Secteurs.d.activités.définies.par.l.association = strsplit((as.character(db$Secteurs.d.activités.définies.par.l.association)),";")

dbCountSector = ddply(db, .(Secteurs.d.activités.définies.par.l.association), summarize, sum_nb=sum(Montant.voté))


strsplit("Aides aux associations;Architecture & urbanisme;Cu",";")


act_sector = unique(unlist(strsplit((as.character(db$Secteurs.d.activités.définies.par.l.association)),";")))

bSummedTot = ddply(db, .(act_sector), summarize, nbAccept=sum(Montant.voté))
# 
# newPrenomsDB = ddply(prenomsDB, .(Prénom, Année), function(x){
#   if((x$Prénom[[1]] %in% dbFiveBest$Prénom)){
#     if(x$Sexe[[1]] == dbFiveBest[which(dbFiveBest$Prénom == x$Prénom[[1]]),]$Sexe[[1]]){
#       x$BestYear <- dbFiveBest[which(dbFiveBest$Prénom == x$Prénom[[1]]),]$Année[[1]]
#       x
#     }
#   }
# })

#           
which ((act_sector %in% df$Secteurs.d.activités.définies.par.l.association== TRUE))

xaxa <- which((act_sector %in% c("Culture & Arts", "Education & formation", "Social")) == TRUE)


ah = df %>%separate(Secteurs.d.activités.définies.par.l.association,c("s1","s2","s3"))



