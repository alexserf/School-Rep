Temps <- c(1.2,3.4,2.1,5.5) #vecteur de temps
Masse <- c(2.5,4.2,5.6,3.4)
Sexe <- c("male","femelle","male", "femelle")
jeu <- data.frame(Temps,Masse,Sexe)
jeu

autos <- data.frame(Vitesse = c(25,40,70,100),
                    Type = c("auto","camion","auto","camion"))
autos
str(jeu)
summary (jeu)

boxplot(jeu)
library()
RSiteSearch("read Excel files")

install.packages(pkgs = "gdata", repos = "http://probability.ca/cran")
library(gdata)
installXLSXsupport()

vers <- read.xls(xls = "C:/Users/alexs/Documents/TELUQ/INF 1421 Apprentissage Machine/Module1/JeuDonnees.xlsx", sheet = 1)

DonneesOzone <- read.xls(xls = "C:/Users/alexs/Documents/TELUQ/INF 1421 Apprentissage Machine/Module1/DonneesOzone.xls", sheet = 1)
