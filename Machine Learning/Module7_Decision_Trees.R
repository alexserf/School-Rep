# Entropie
#
#       Humidite (14)
#       /           \
# Elevee (3+,4-)  Normale(6+,1-)
#
# Entropie(Eleve)=E(3+,4-) = (3/7)log(3/7)+(4/7)log(4/7) = 0.985
# E(Normale) = E(6+,1-) = (6/7)log(6/7)+(1/7)log(1/7) = 0.592
# E(Humidite) = (7/14)*E(Eleve)+7/14*E(Normale) = 0.789
#
# E(Jouer) = 0.94
#
# Gain
#
# Gain = Jouer - Humidite = 0.94 - 0.789 = 0.151
# 
# Gain = Jouer - Ciel = 0.94 - 0.693 = 0.247
#
# Rapport de gain
#
#               Ciel
#         /         |         \
#   Soleil        Couvert     Pluie
#
# R= Gain / SplitInfo
#
# SplitInfo = 5/14E(soleil)+4/14E(couvert)+5/14E(pluie)=1.577
#
# R=0.246/1.577=0.156
#
# Calcul de l'entropie de la variable Couleur

install.packages("DescTools")
library("DescTools")
Data <- matrix(c( 1 , 1 , 1 , 1 ,2 , 1 , 1 , 1 ,3 , 0 , 1 , 1 ,4 , 0 , 0 ,1 ,5 , 0 , 0 , 0 , 6 , 0 , 0 , 0 , 6 , 1 , 1 , 0 , 6 , 1 , 0 , 0) ,nrow=8, ncol=4, byrow=T)

#Entropie de la variable Couleur
EntrCouleur = Entropy(Data[,3],Data[,4],base=2)-1
View(EntrCouleur)

#Entropie de la variable classe
EntrClasse = Entropy(Data[,4],base=2)-1
View(EntrClasse)

#Calcul de Gain de la variable Couleur
GainCouleur = EntrClasse-EntrCouleur
View(GainCouleur)

#Gaine de la variable Taille
GainTaille=EntrClasse-EntrTaille
View(GainTaille)

#calcul
