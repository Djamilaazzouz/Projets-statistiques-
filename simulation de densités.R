n=100
p=0.5
V=rnorm(n,-1,1)
W=rnorm(n,1,0.5)
U=rbinom(n,size=1,prob=p)
Z=U*W+(1-U)*W
print(Z)
hist(Z, main="Estimateur par histogramme",col="yellow",freq=F)
densite=density(Z)
lines(densite,col="red",lwd=1)
vers=c(100,1000,10000)
par(mfrow=c(1,3))
for(i in vers){
x=rnorm(i)
d_x=density(x)
plot(density(x,bw=0.01,kernel="gaussian"),main="Variation de l'échantillon",\end{verbatim} \begin{verbatim}col="green",lwd="1")
lines(d_x,col="red",lwd="2")
}
n=1000
y=rexp(n)
par(mfrow=c(1,3))
plot(density(y,bw=0.01,kernel="rectangular"),main="Variation de la fenêtre" 
     ,col="blue",lwd="1")
lines(density(y),col="red",lwd="2")
plot(density(y,bw=0.3,kernel="rectangular"),main="Variation de la fenêtre",
     col="blue",lwd="1")
lines(density(y),col="red",lwd="2")
plot(density(y,bw=3,kernel="rectangular"),main="Variation de la fenêtre",
     col="blue",lwd="1")
lines(density(y),col="red",lwd="2")
z=rchisq(1000,df=3,ncp=0)
par(mfrow=c(1,3))
plot(density(z,bw=0.01,kernel="triangular"),main="Estimateur à noyau de
la loi de chi2",col="blue",lwd="1")
lines(density(z),col="red",lwd="3")
plot(density(z,bw=0.01,kernel="rectangular"),main="Estimateur à noyau"
     ,col="blue",lwd="1")
lines(density(z),col="red",lwd="3")
plot(density(z,bw=0.01,kernel="gaussian"),main="Estimateur à noyau"
     ,col="blue",lwd="1")
lines(density(z),col="red",lwd="3")
#Ecrire une fonction qui prend en paramètre u et renvoie le noyau gaussien.
K_G=function(u){
  f=1/sqrt(2*pi)*(exp(-u^2/2))
  return(f)
}
# de l'échantillon X muni d'un noyau gaussien et de la fenêtre h
fchap=function(X,x,h) {
  n=length(X)
  fh=0
  for (i in 1:n){
    u= (X[i]-x)/h
    fh=fh+K_G(u)
  }
  return(fh/(n*h))
}
# simuler X_1,.....X_n en supposons que les X_i sont indépendantes
M=100
x=seq(0.01,1,0.01)
h=c(0.01, 0.02, 0.03)
X=rnorm(M,0,1)
for (i in 1:M){
  x=i/M
  return(fchap)
}
#calcul fchap
for(i in 1:h){
  z=fchap(X,x,i)
  return(z)
}
#calcul f
for(i in 1:M){
  x=i/M
  f=(1/sqrt(2*pi))*exp(-x^2/2)
  return(f)
}
#calculer Sg
print(z)
l=z-f
S_M=mean(l^2)
print(S_M)
#affecter S_M à risque[j]
Nrep=100
risque=vector()
for(j in 1:Nrep){
  risque[j]= S_M
}

par(mfrow=c(1,3))
plot(density(risque,bw=0.01,kernel="gaussian"),
     main="Variation de la fenetre",col="blue",lwd="1")
lines(density(risque),col="red",lwd="2")
plot(density(risque,bw=0.02,kernel="gaussian"),
     main="Variation de la fenetre",col="blue",lwd="1")
lines(density(risque),col="red",lwd="2")
plot(density(risque,bw=0.03,kernel="gaussian"),
     main="Variation de la fenetre",col="blue",lwd="1")
lines(density(risque),col="red",lwd="2")
install.packages("FNN")
library(FNN)
knn.