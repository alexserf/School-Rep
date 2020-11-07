perceptron <- function (x,y,eta, w0, w1, w2, w3)
{
  for(i in 1:3)
  {
    a<-0
    t1<-w0
    t2<-x[i,1]*w1
    t3<-x[i,2]*w2
    t4<-x[i,3]*w3
    a <-w0+x[i,1]*w1+x[i,2]*w2+x[i,3]*w3
    
    if(a < 0)
    {
      a <- 0
    }
    else
    {
      a <- 1
    }
    
    if(a==y[i,1])
    {
      valeurspoids<- c(w0,w1,w2,w3)
      return(valeurspoids)
      
    }
    else 
    {
      w0<- w0+eta*1*(y[i,1]-a)
      w1<- w1+eta*x[i,1]*(y[i,1]-a)
      w2<- w2+eta*x[i,2]*(y[i,1]-a)
      w3<- w3+eta*x[i,3]*(y[i,1]-a)
    }
  }
}

inputs <- matrix(c(3,1,1,2,1,2,1,1,3),ncol=3,nrow=3) 
outputs<- matrix(c(0,1,1), ncol=1,nrow=3)

n<-0.1
b<-0.5
poids1<-0
poids2<-0
poids3<-0

perceptron(inputs,outputs,n,b,poids1,poids2,poids3)


########################################
quizperceptron <- function (x,y,eta, w0, w1, w2)
{
  for(i in 1:3)
  {
    a<-0
    a <-w0+x[i,1]*w1+x[i,2]*w2
    
    if(a < 0)
    {
      a <- 0
    }
    else
    {
      a <- 1
    }
    
    if(a==y[i,1])
    {
      valeurspoids<- c(w0,w1,w2)
      return(valeurspoids)
      
    }
    else 
    {
      w0<- w0+eta*1*(y[i,1]-a)
      w1<- w1+eta*x[i,1]*(y[i,1]-a)
      w2<- w2+eta*x[i,2]*(y[i,1]-a)
    }
  }
}


inputs <- matrix(c(20,2,16,5,6,2,8,8,10,2,3,3), ncol=2, nrow=6)
outputs<- matrix(c(1,1,1,0,0,0), ncol=1, nrow=3)
n<-0.1
b<- -20
poids1<- 1
poids2<- 2

quizperceptron(inputs,outputs,n,b,poids1,poids2)


