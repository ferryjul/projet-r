library(ggplot2)
library(plyr) # dplyr
library(tidyverse)
library(grid)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ou pour savoir lequel appeler:
# dplyr::filter (par exemple)
db = read.csv(file = "./data/subventions-accordees-et-refusees.csv", sep= ';')

#on transforme notre dataset pour récupérer chaque domaine d'activité individuellement (un seul par ligne, on duplique les lignes qui en ont plusieurs)
df <- db %>%
  transform(Secteurs.d.activités.définies.par.l.association = strsplit((as.character(Secteurs.d.activités.définies.par.l.association)),";")) %>% unnest(Secteurs.d.activités.définies.par.l.association)

#réorganisation du dataset en fonction des secteurs + on calcule la montant moyen des subventions acceptées
db_sector = ddply(df, .(Secteurs.d.activités.définies.par.l.association), summarize, nbAccept=sum(Montant.voté!="0"), Tot_Subv=sum(Montant.voté), moy_Tot_accept = (Tot_Subv/nbAccept)/1000)

#premier graphe - montant moyen subvention / secteur
g1 <- ggplot(data = db_sector, aes(x=Secteurs.d.activités.définies.par.l.association, y = moy_Tot_accept)) + 
  coord_flip() + scale_y_reverse()+
  geom_bar(stat = "identity", position="dodge", fill = "#660066") + 
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.background = element_rect(fill="lightgray"), 
        plot.margin = unit(c(1,1,0,40), "mm"),
        legend.key = element_rect(fill="black", color = NA), 
        plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + xlab(NULL)+
 ylab("Montant moyen subvention (en K euros)") #+ labs(title = "Montant moyen d'une subvention en fonction du secteur d'activité", subtitle = "De 2013 à 2019")

#deuxième graphe - nb accepté subvention / secteur
g2 <- ggplot(data = db_sector, aes(x=Secteurs.d.activités.définies.par.l.association, y = nbAccept)) + 
  coord_flip() +
  geom_bar(stat = "identity", position="dodge", fill = "#CC3300") + 
  theme(legend.position = "bottom", legend.direction = "horizontal", 
        axis.ticks.y = element_blank(),
        legend.background = element_rect(fill="lightgray"), 
        legend.key = element_rect(fill="black", color = NA), 
        #plot.margin = unit(c(1,0,1,0), "mm"), 
        plot.title= element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) + ylab("Nombre de subventions acceptées") #+ labs(title = "Nombre de demandes de subvention acceptées en fonction du secteur d'activité", subtitle = "De 2013 à 2019")

multiplot(g1,g2, cols=2)
