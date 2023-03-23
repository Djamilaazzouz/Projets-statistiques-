#Projet 2 en statistique grande dimension
#Partie
#Question 1: écrire une fonction qui prend en paramètre n
n=100
#Fonction qui nous permet de calculer la norme
X=matrix(data=runif(n*d,0,1),nrow=n,ncol=d)
norme=function(X){
  norme_2=rep(0,n)
  for(i in 1:n){
    norme_2[i]= sqrt(sum(X[i,]^2))
  }
  return(norme_2)
}
#On construit la fonction qui prend en param n
d=1
fon=function(n){
  epsi=rnorm(n,0,1)
  Y=5*exp(norme(X))+epsi
  return(Y)
}
#Question 2:
#projection n n-échantillon de même loi que (X,Y)
plot(X,fon(n),main="Graphe de n-éch et la fonction de régression")
#Calculons f*
#On sait que f*=E(Y|X)
#f*=E(E(Y|X))=5*E(exp(norme_2|X))=5*exp(X)
f_et=function(x){
  f=5*exp(x)
  return(f)
}
curve((f_et(x)),lwd="2",col="red", add=TRUE)
#Partie 2
#Question 1: Calculons le risque L2
# On sait que R(f*)=E((Y-f*(x)^2)
# Nous avons donc : R(f*)=E((5*exp(x)+epsi-5*exp(x)^2)
# Ainsi R(f*)= E(epsi^2)= Var(epsi)=1
#Question 2:
# Nous allons varier k :
#On sait que f_chap=1/n*sum(Y_(i))
#Construction d'un nouveau echantillon 
n=100
#Fonction qui nous permet de calculer la norme
X=matrix(data=runif(n,0,1),nrow=n,ncol=1)
Xtest=X
norme=function(Xtest){
  norme_2=rep(0,n)
  for(i in 1:n){
    norme_2[i]= sqrt(sum(Xtest[i,]^2))
  }
  return(norme_2)
}
tes=function(n){
  epsi=rnorm(n,0,1)
  Ytest=5*exp(norme(Xtest))+epsi
  return(Ytest)
}
library(FNN)
par(mfrow=c(2,2))
f_chap=knn.reg(X,Xtest,tes(n),k=1)
f_chap$pred
f_et=function(x){
  f=5*exp(x)
  return(f)
}
plot(sort(X),sort(f_chap$pred),type='l',col="green",main="k=1",lwd=3)
curve((f_et(x)),lwd="2",col="red", add=TRUE)
#Variation de k
f_chap=knn.reg(X,Xtest,tes(n),k=4)
f_chap$pred
f_et=function(x){
  f=5*exp(x)
  return(f)
}
plot(sort(X),sort(f_chap$pred),type='l',col="green",main="k=4",lwd=3)
curve((f_et(x)),lwd="2",col="red", add=TRUE)
############################################"
f_chap=knn.reg(X,Xtest,tes(n),k=20)
f_chap$pred
f_et=function(x){
  f=5*exp(x)
  return(f)
}
plot(sort(X),sort(f_chap$pred),type='l',col="green",main="k=20",lwd=3)
curve((f_et(x)),lwd="2",col="red", add=TRUE)
#########################################
f_chap=knn.reg(X,Xtest,tes(n),k=80)
f_chap$pred
f_et=function(x){
  f=5*exp(x)
  return(f)
}
plot(sort(X),sort(f_chap$pred),type='l',col="green",main="k=80",lwd=3)
curve((f_et(x)),lwd="2",col="red", add=TRUE)

#Variation de l'echantillon
library(FNN)
par(mfrow=c(2,2))
n=100
X=matrix(runif(n,0,1),nrow=n,ncol=1)
Xtest=X
f_chap=knn.reg(X,Xtest,tes(n),k=3)
f_chap$pred
f_et=function(x){
  f=5*exp(x)
  return(f)
}
plot(sort(X),sort(f_chap$pred),type='l',col="blue",main="n=100",lwd=3)
curve((f_et(x)),lwd="2",col="red", add=TRUE)

#Variation de n
n=500
X=matrix(runif(n,0,1),nrow=n,ncol=1)
Xtest=X
f_chap=knn.reg(X,Xtest,tes(n),k=3)
f_chap$pred
f_et=function(x){
  f=5*exp(x)
  return(f)
}
plot(sort(X),sort(f_chap$pred),type='l',col="blue",main="n=500",lwd=3)
curve((f_et(x)),lwd="2",col="red", add=TRUE)
############################################"
n=1000
X=matrix(runif(n,0,1),nrow=n,ncol=1)
Xtest=X
f_chap=knn.reg(X,Xtest,tes(n),k=3)
f_chap$pred
f_et=function(x){
  f=5*exp(x)
  return(f)
}
plot(sort(X),sort(f_chap$pred),type='l',col="blue",main="n=1000",lwd=3)
curve((f_et(x)),lwd="2",col="red", add=TRUE)
#########################################
n=1500
X=matrix(runif(n,0,1),nrow=n,ncol=1)
Xtest=X
f_chap=knn.reg(X,Xtest,tes(n),k=3)
f_chap$pred
f_et=function(x){
  f=5*exp(x)
  return(f)
}
plot(sort(X),sort(f_chap$pred),type='l',col="blue",main="n=1500",lwd=3)
curve((f_et(x)),lwd="2",col="red", add=TRUE)
#On remarque que plus aue l'echantillon est grand plus que notre estimateur
# est plus proche de f_etoile

#Question 3
#  1/ Simulation des D_n
#Fonction qui nous permet de calculer la norme
n=5
X=matrix(data=runif(n,0,1),nrow= n,ncol=1)
Xtest=X
norme=function(Xtest){
  norme_2=rep(0,n)
  for(i in 1:n){
    norme_2[i]= sqrt(sum(Xtest[i,]^2))
  }
  return(norme_2)
}
tes=function(n,Xtest){
  epsi=rnorm(n,0,1)
  Ytest=5*exp(norme(Xtest))+epsi
  return(Ytest)
}
#Simulation de (X_n+1,Y_n+1)......(X_n+M,Y_n+M)
#Fonction qui nous permet de calculer la norme
n=5
M=2
X_1=matrix(data=runif((n+M),0,1),nrow=(n+M),ncol=1)
Xtest1=X_1
norme(X_1)
Y_1=tes(n+M,Xtest1)
Y_1
#Calculer f_chap à partir de D_n
library(FNN)
f_chap1=knn.reg(X,Xtest,tes(n),k=3)
f_chap1$pred
#Calculer f_chap du deuxième échantillon
f_chap2=knn.reg(X_1,Xtest1,Y_1,k=3)
#fchap2=f_chap1(X_1,Y_1)
risque_q=vector()
erreur=function(n,Y_1){
  for(i in 1:M){
    risque_q[i]=risque_q[i]+(Y_1[i]-f_chap2[i])^2
  }
  return(risque_q/M)
}
