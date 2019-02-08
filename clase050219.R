library(deSolve)
r=c(-1,-.5,.5,1,3,8)
A=100
par(mfrow=c(2,3))
yini=c(p=5)  
for (i in 1:6) {
movimiento=function(t,y,parms){
  with(as.list(y),{
    dp=r[i]*p*(1-(p/A))
    list(c(dp))})}
tiempo=seq(0,10,0.001)

solucion=ode(y=yini,times=tiempo,func=movimiento, parms=NULL)
plot(tiempo,solucion[,'p'], type='l')
}


#----------------------------------------------------------

n=seq(0,20)
x=rep(0,21)
r=c(-1,-.5,.5,1,3,8)
#r=1.2
A=100
x[1]=5
for (i in 1:6) {
  for (j in 2:length(n)){
    x[j]=r[i]*x[j-1]*(1-(x[j-1]/A))
  }
  plot(n,x,type='l')
}

