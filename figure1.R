library(ggplot2)
library(plyr) # dplyr
# ou pour savoir lequel appeler:
# dplyr::filter (par exemple)
db = read.csv(file = "./data/subventions-accordees-et-refusees.csv", sep= ';')


dbClean <- db[which(db$Montant.voté != "0"),]
#dbSummed = ddply(dbClean, .(Année.budgétaire), summarize, sum_nb=sum(Nombre))

ggplot(data = db, aes(x=factor(Année.budgétaire), y = sum_nb, colour = status) + geom_bar(stat = "identity", position="dodge");
       