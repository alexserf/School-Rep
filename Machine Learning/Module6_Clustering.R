setwd("C:/Users/alexs/Documents/TELUQ/INF 1421 Apprentissage Machine/Module6/")

installXLSXsupport()
install.packages("readxl")

library("readxl")

fromage <-read_xlsx("DataSetFromage.xlsx", sheet=1)

#afficher fichier
print(fromage)

#statistiques descriptives
summary(fromage)

#graphique - croisement deux a deux
dev.new()
pairs(fromage[2:10])

#Matrice des distances entre individus
d_fromage <- dist(fromage, method = "euclidean")

#Regroupement hierarchique
Hirearchie_cluster <- hclust(d_fromage,"average")

#afficher dendogramme
dev.new()
plot(Hirearchie_cluster, labels=fromage$Fromages)

#dendogramme avec 4 groupes
rect.hclust(Hirearchie_cluster, k=4)

#decoupage en 4 groupes
Hirearchie_cluster_Groupe <- cutree(Hirearchie_cluster, k=4)

#liste des groupes
sort(Hirearchie_cluster_Groupe)
