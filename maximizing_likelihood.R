neg_max_likelihood<-function(x){
  y<-data
  mu<-x[1]
  sigma<-x[2]
  n<-100
  lik<--((-(n/2)*log(sigma^2))-((n/2)*log(2*pi))-(sum((y-mu)^2)/(2*sigma^2)))
  return(lik)
}

gradient<-function(x){
  y<-data
  mu<-x[1]
  sigma<-x[2]
  n<-100
  return(c(-((sum(y-mu))/(sigma^2)), -(((sum((y-mu)^2))/(sigma^3))-(n/sigma))))
}


optim(par=c(0,1),fn=neg_max_likelihood,method = "BFGS")
optim(par=c(0,1),fn=neg_max_likelihood,method = "CG")
optim(par=c(0,1),fn=neg_max_likelihood,method = "BFGS",gr=gradient)
optim(par=c(0,1),fn=neg_max_likelihood,method = "CG",gr=gradient)
