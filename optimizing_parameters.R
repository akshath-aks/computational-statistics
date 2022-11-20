my_func<-function(x_val){
  ac<-x_val+(x_val^2)
  return(ac)
}


error_func<-function(par,...){
  a0<-par[1]
  a1<-par[2]
  a2<-par[3]
  x0<-...[1]
  x1<-...[2]
  x2<-...[3]
  
  #(x+(x^2)) is my actual aproximated function
  min_error<-((my_func(x0)-(a0+(a1*x0)+(a2*(x0^2))))^2)+((my_func(x1)-(a0+(a1*x1)+(a2*(x1^2))))^2)+
    ((my_func(x2)-(a0+(a1*x2)+(a2*(x2^2))))^2)
  
  return(min_error)
}


optimization<-function(x){
  op<-optim(par=c(1,1,1),fn=error_func,x=x)
  a0<-op$par[1]
  a1<-op$par[2]
  a2<-op$par[3]
  
  # act<-c(my_func(x[1]),my_func(x[2]),my_func(x[3]))
  # parab<-c((a0+(a1*x[1])+(a2*(x[1]^2))),(a0+(a1*x[2])+(a2*(x[2]^2))),(a0+(a1*x[3])+(a2*(x[3]^2))))
  return(c(a0,a1,a2))
  #,act=act,parab=parab
}
optimization(c(0.1,0.5,0.8))


vec<-seq(0.01,1,by=0.01)
vec
tes<-c()
approx<-c()
for (i in 1:length(vec)){
  test<-my_func(vec[i])
  tes<-c(tes,test)
  a0<-optimization(c(vec[i]-0.01,vec[i]-0.005,vec[i]))[1]
  a1<-optimization(c(vec[i]-0.01,vec[i]-0.005,vec[i]))[2]
  a2<-optimization(c(vec[i]-0.01,vec[i]-0.005,vec[i]))[3]
  appr<-(a0+(a1*vec[i])+(a2*(vec[i]^2)))
  approx<-c(approx,appr)
}
plot(vec,approx,type='l')
lines(vec,tes,type='l')

my_func<-function(x_val){
  ac<--x_val*(1-x_val)
  return(ac)
}

my_func<-function(x_val){
  ac<--x_val*(sin(10*pi*x_val))
  return(ac)
}



