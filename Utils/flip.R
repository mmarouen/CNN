#flipping a kernel 180°

flip<-function(X){
  k=ncol(X)
  n=nrow(X)
  X1=X
  X1[floor(n/2):1,]=X[(ceiling(n/2)+1):n,]
  X1[n:(ceiling(n/2)+1),]=X[1:floor(n/2),]
  Xf=X1
  Xf[,floor(k/2):1]=X1[,(ceiling(k/2)+1):k]
  Xf[,k:(ceiling(k/2)+1)]=X1[,1:floor(k/2)]
  return(Xf)
}
