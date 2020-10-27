#2. Soit un échantillon provenant d'une loi de densité
#normale N de paramètre u= 0, et o2 = 2
vnorm <- function(theta,obs)
{
  mu<-theta[1]
  sigma <-theta[2]
  -sum(log(dnorm(x=obs,mu,sigma))) #log Likelihood
}

#1. En utilisant la fonction dnorm, générez 100 échantillons de la
#densité N(0; 2).

obs <-rnorm(100,0,2)

#2. Estimez les paramètres u et u2 en utilisant l'estimateur de maximum
#de vraissemlance dans R.

Estimateur_1 <- nlm(f=vnorm,p=c(0,0.9), obs=obs)
Estimateur_1$estimate

mean(obs)
sd(obs)

#3. Comparez les résultats obtenus avec les résultats théoriques (obtenu dans l'exercice 1.1).
u <-seq(-5,5, by=0.1) #by smoothes out line
hist(obs, probability = T) #histogramme des valeurs generes
lines(u,dnorm(u,Estimateur_1$estimate[1],Estimateur_1$estimate[2])) #marque la courbe presente

#3. Soit un échantillon provenant d'une loi de densité de
#poisson de paramètre lambda = 1.

poisson.lik <- function(mu,y)
{
  n<-nrow(y)
  logl<-sum(y)*log(mu)-n*mu
  return(-logl)
}
#1. En utilisant la fonction dnorm, générez 100 échantillons de la
#densité de poisson.

obs_2 <- rpois(100,1)
y<-obs_2
install.packages(pkgs = "stats4", repos = "http://probability.ca/cran")
#2. Enstimez le paramètre lambda en utilisant l'estimateur de maximum de
#vraissemlance dans R.
library(stats4)

nLL <- function(lambda) -sum(stats::dpois(y, lambda, log = TRUE))
fit0 <- mle(nLL, start = list(lambda = 1), nobs = NROW(y))
fit0$

Estimateur_2 <- nlm(f=poisson.lik,p=c(0,1),obs_2)
Estimateur_2$estimate

mean(obs_2)
sd(obs_2)

#3. Comparez les résultats obtenus avec les résultats théoriques
#(obtenus dans l'exercice 1.2).
