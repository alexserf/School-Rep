dev.new()
plot(function(x) dnorm(x,0,1), -5, 5, main="Representation", col="red")
curve(dnorm(x,1,1), add=TRUE, col="blue")
legend("topleft", legend=(c("p(x1)","p(x2)"),col=c("red","blue")))

setwd("C:/Users/alexs/Documents/TELUQ/INF 1421 Apprentissage Machine/Module2/")
library(readxl)
Iris <- read_xlsx("iris.xlsx")      
summary(Iris) #exprime les resultats de chaque colonne       
table(Iris$Species) #explique le nombre d'observation par resultat dans une colonne

#Divisez les données en une base d'entrainement et une base de
#test, avec 80% de données pour l'entrainement. Fixez set.seed à 7
#(set.seed(7)).
install.packages("caTools")
library(caTools)
set.seed((7))
sample_iris <-sample.split(iris$Species,SplitRatio = 0.8)
entrainement_iris <- subset(iris, sample_iris ==T) #True that 80% belong to training set
test_iris <- subset(iris, sample_iris == F)

#Développez un classificateur bayésien en utilisant la base d'entraînement
#des caractéristiques des espèces des fleurs.
#necessaire pour taux de classification et Bayes
install.packages("e1071")
library(e1071)
#creer un model de Bayes avec donnees d'entrainement
model_iris <-naiveBayes(Species~.,data =entrainement_iris)
summary(model_iris)

#choisir apriori
model_iris$apriori
#utiliser le model (qui utilise Bayes) en combinaison avec les variables du set Test
predict_iris <- predict(model_iris, test_iris)
summary(predict_iris)

#Evaluer le taux de classification
conf <- table(pred=predict_iris,true=test_iris$Species)
summary(conf)
tauxDeClassification <-sum(diag(conf))/sum(conf) #taux de 93.33%

#deuxieme maniere de faire.
library(caret)
confmatrix <- confusionMatrix(conf)
confmatrix
