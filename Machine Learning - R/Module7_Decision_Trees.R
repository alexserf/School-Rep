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


# EXERCICE 3

# Calcul de l'entropie de la variable Couleur

install.packages("DescTools")
library("DescTools")
Data <- matrix(c( 1 , 1 , 1 , 1 ,2 , 1 , 1 , 1 ,3 , 0 , 1 , 1 ,4 , 0 , 0 ,1 ,5 , 0 , 0 , 0 , 6 , 0 , 0 , 0 , 6 , 1 , 1 , 0 , 6 , 1 , 0 , 0) ,nrow=8, ncol=4, byrow=T)

#Entropie de la variable Couleur
EntrCouleur = Entropy(Data[,3],Data[,4],base=2)-1

#Entropie de la variable classe
EntrClasse = Entropy(Data[,4],base=2)-1

#Entropie de la variable Taille
EntrTaille = Entropy(Data[,2],base=2)-1

#Calcul de Gain de la variable Couleur
GainCouleur = EntrClasse-EntrCouleur

#Gaine de la variable Taille
GainTaille=EntrClasse-EntrTaille
#Puisque le gain est zero, la taille ne constitue pas un bon predicteur de la race

#Fonction pour l'entropie
EntropyFonction <- function(p,n)
{
  return((-p/(n+p))*log2(p/(n+p))-n/(n+p))*log2(n/(n+p))
}

# EXERCICE 4

install.packages("rpart", dep=TRUE)
library(rpart)
install.packages("FSelector")
library(FSelector)
data("iris")
data("kyphosis")
library(rpart)
library(rpart.plot)

v <- iris$Species



#Arbre de decision de donnees "iris"
dev.new()
arbreIris <- rpart(Species ~ ., data=iris, method="class",minsplit=20, xval=81)
plot(arbreIris, uniform=TRUE, margin=0.1, main="Arbre de decision 'iris'")
text(arbreIris, fancy=TRUE, use.n=TRUE, pretty=0, all=TRUE)

#Arbre de decision de la base de donnees "Kyphosis"
dev.new()
arbreKyphosis <- rpart(kyphosis, data=kyphosis, method="class",minsplit=20, xval=81)
plot(arbreKyphosis, uniform=TRUE, margin=0.1, main="Arbre de decision 'Kyphosis'")
text(arbreKyphosis, fancy=TRUE, use.n=TRUE, pretty=0, all=TRUE)

#Calcul Gain ratio et Gain pour les attributs
information.gain(Species~.,data=iris,unit="log2")
gain.ratio(Species~.,data=iris,unit="log2")
#Petal.Length & Width are most important

information.gain(kyphosis,data=kyphosis,unit="log2")
gain.ratio(kyphosis,data=kyphosis,unit="log2")
#Start is most important

#Arbre de decision de la base de donnees iris apres elimination des attributs non-importants
dev.new()
arbreIris2 <- rpart(Species ~ Petal.Length+Petal.Width, data=iris, method="class",minsplit=20, xval=81)
plot(arbreIris2, uniform=TRUE, margin=0.1, main="Arbre de decision 'iris'")
text(arbreIris2, fancy=TRUE, use.n=TRUE, pretty=0, all=TRUE)

#Arbre de decision de la base de donnees Kyphosis apres elimination des attributs non-importants
dev.new()
arbreKyphosis2 <- rpart(Kyphosis~Start, data=kyphosis, method="class",minsplit=20, xval=81)
plot(arbreKyphosis2, uniform=TRUE, margin=0.1, main="Arbre de decision 'Kyphosis'")
text(arbreKyphosis2, fancy=TRUE, use.n=TRUE, pretty=0, all=TRUE)


# EXERCICE 5
install.packages("rpart.plot") #recursive partitioning for classification
library(rpart.plot)
CreditData <- read.csv("C:/Users/alexs/Documents/TELUQ/INF 1421 Apprentissage Machine/Module7/CreditData.txt")

#Training & Test Data
Train<-CreditData[251 :1000,]
Test<-CreditData[1:250,]

#CART decision Tree
dev.new()
CartTree <- rpart(Creditability ~., data=Train)
prp(CartTree,extra=1)
title("Arbre de decision CART- Training Data")

#C5.0 Algorithm
install.packages("C50")
library(C50)
Train2 <- Train[1 : 750,]
dev.new()
TreeCiris <- C5.0(x=iris[,-5],y=iris$Species)
plot(TreeCiris)
summary(TreeCiris)

dev.new()
TreeCkyphosis <- C5.0(x=kyphosis[,-5],y=kyphosis$Kyphosis)
plot(TreeCkyphosis)
summary(TreeCkyphosis)

dev.new()
TreeCCreditability <- C5.0(x=Train2[,-1],y=Train2$Creditability)
plot(TreeCCreditability)
summary(TreeCCreditability)

