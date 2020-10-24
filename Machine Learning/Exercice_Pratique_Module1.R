setwd("C:/Users/alexs/Documents/TELUQ/INF 1421 Apprentissage Machine/Module1/")
install.packages(pkgs = "gdata", repos = "http://probability.ca/cran")
library(gdata)
installXLSXsupport()

DonneesOzone <- read.xls(xls = "C:/Users/alexs/Documents/TELUQ/INF 1421 Apprentissage Machine/Module1/DonneesOzone.xls", sheet = 1)
View(DonneesOzone)

nrow(DonneesOzone) #nombre total de valeurs

TendanceCentrale <- summary(DonneesOzone[][2:11]) #All rows, columns 2 to 11 because 1 is useless.
View(TendanceCentrale)

#Boxplot temperature
dev.new()
boxplot(DonneesOzone[][3:5], 'boxstyle','filled','notch','on', ylab="Temperature observee en degree")
title("Temperature observee a 9h, 12h et 15h")

#boxplot
dev.new()
boxplot(DonneesOzone[][6:8], 'boxstyle','filled','notch','on', ylab="Nebulosite")
title ("Nebulosite observee a 9h, 12h et 15h")

DonneesOzone[][3:8]$out

#HELP
outlierNe9 <- boxplot.stats(DonnneesOzone$Ne9)$out
outlierNe9 <- boxplot.stats(DonnneesOzone[][6], do.conf=TRUE, do.out = TRUE)
print(outlierNe9)

install.packages("plotrix")
library(plotrix)

#Analyse graphique des variables vent et pluie
dev.new()
lbls <- names(table(DonneesOzone[][13]))
lbls
slices <- matrix(table(DonneesOzone[][13]))
slices
table(DonneesOzone[][13])
pct <- round(slices/sum(slices)*100)
lbls <-paste(lbls,pct,"%", sep=" ")
lbls
pie3D(table(DonneesOzone[][13]),labels = lbls, explode = 0.1,
            main ="Diagramme circulaire pour la variable vent")

dev.new()
lbls <- paste(names(table(DonneesOzone[][14])),"\n")
lbls
slices <- matrix(table(DonneesOzone[][14]))
slices
table(DonneesOzone[][14])
pct <- round(slices/sum(slices)*100)
lbls <-paste(lbls,pct,"%", sep=" ")
lbls
pie3D(table(DonneesOzone[][14]),labels = lbls, explode = 0.1,
      main ="Diagramme circulaire pour la variable vent")

install.packages("readxl")
library("readxl")
JeuDonnees <- read_xlsx("JeuDonnees.xlsx",sheet=1)
View(JeuDonnees)

#Confusion Matrix
install.packages("caret")
library(caret)
confMatr <- table(JeuDonnees$`Classe réelle`,JeuDonnees$`Classe prédite`)
print(confMatr)
install.packages("e1071")
f.confMatr <- confusionMatrix(confMatr)
f.confMatr
#Overall Accuracy is classification globale
#Pos Pred Value is bonne classifcation