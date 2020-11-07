E<- c(4,5,5,6,12,14,15,15,16,17)

densite <- function (Points,x,h){
  densite <- 0
  Tempo <- 0
  for (i in 1:length(Points))
  {
    if ((abs(x-Points[i])/h)>0.5)
    {
      Tempo <- Tempo + 0
    }
    else{
      Tempo <- Tempo + 1
    }
  }
  densite <-((1/(length(Points)*h))*Tempo)
}

den <- densite(E,15,4)
print(den)

