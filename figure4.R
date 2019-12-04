library(plyr) # dplyr
library(fmsb)
require(reshape2)

db = read.csv(file = "./data/subventions-accordees-et-refusees.csv", sep= ';')


# Compute sums and ratio
dbCountSummary = ddply(db, .(Direction), summarize, nbAccept=sum(Montant.voté!="0"), nbRefuse=sum(Montant.voté=="0"))
dbCountSummary$ratio = dbCountSummary$nbAccept / (dbCountSummary$nbRefuse + dbCountSummary$nbAccept)

# Sort the dataframes
dbCountSummary$totalSubvTraitees = dbCountSummary$nbAccept + dbCountSummary$nbRefuse
dbCountAcceptOrdered = dbCountSummary[order(dbCountSummary$nbAccept),]
dbCountRefuseOrdered = dbCountSummary[order(dbCountSummary$nbRefuse),]
dbCountRatioOrdered = dbCountSummary[order(dbCountSummary$ratio),]

# Get nb best elements and max in each category
nb = 10
nbestAccept = tail(dbCountAcceptOrdered, n=nb)
nbestRefuse = tail(dbCountRefuseOrdered, n=nb)
nbestRatio = tail(dbCountRatioOrdered, n=nb)
maxAccept = tail(nbestAccept, n=1)$nbAccept
maxRefuse = tail(nbestRefuse, n=1)$nbRefuse

# Prepare each dataframe to be used by radarchart()
## Accept
printAcceptData = data.frame(nbestAccept$nbAccept) # Get column
printAcceptData <- t(printAcceptData) # Transpose it
colnames(printAcceptData) <- as.vector(nbestAccept$Direction) # Adds column names
printAcceptData <- data.frame(printAcceptData)
rownames(printAcceptData) <- c() # Remove row names
# Adds max and min values in dataframe, as expected by radarchart()
printAcceptData <- rbind(rep(maxAccept,nb), rep(0,nb), printAcceptData)

## Refuse
printRefuseData = data.frame(nbestRefuse$nbRefuse)
printRefuseData <- t(printRefuseData)
colnames(printRefuseData) <- as.vector(nbestRefuse$Direction)
printRefuseData <- data.frame(printRefuseData)
rownames(printRefuseData) <- c()
printRefuseData <- rbind(rep(maxRefuse,nb), rep(0,nb), printRefuseData)

## Ratio
printRatioData = data.frame(nbestRatio$ratio)
printRatioData <- t(printRatioData)
colnames(printRatioData) <- as.vector(nbestRatio$Direction)
printRatioData <- data.frame(printRatioData)
rownames(printRatioData) <- c()
printRatioData <- rbind(rep(1,nb), rep(0,nb), printRatioData)

# Print charts
radarchart(printAcceptData, axistype=1,
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,maxAccept,maxAccept/4), cglwd=0.8,
           vlcex=0.8, title="Directions ayant accordé le plus de subventions"
)
legend(x=-1.2, y=-1.10, legend = "#Subventions", bty = "n", pch=20 , col=rgb(0.2,0.5,0.5,0.5) , text.col = "black", cex=1, pt.cex=3)

radarchart(printRefuseData, axistype=1,
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,maxRefuse,maxRefuse/4), cglwd=0.8,
           vlcex=0.8,  title=("Directions ayant refusé le plus de subventions")
)
legend(x=-1.2, y=-1.10, legend = "#Subventions", bty = "n", pch=20 , col=rgb(0.2,0.5,0.5,0.5) , text.col = "black", cex=1, pt.cex=3)
  
# Non utilisé à l'heure actuelle dans le rapport
radarchart(printRatioData, axistype=1,
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8,  title=("Directions ayant accordé (en %) le plus de subventions entre 2013 et 2018")
)
legend(x=-1.7, y=-1.10, legend = "% subventions accordées", bty = "n", pch=20 , col=rgb(0.2,0.5,0.5,0.5) , text.col = "black", cex=1, pt.cex=3)


