#performs forward propagation for convolution layer

FP_Conv<-function(Im,W,p1,p2,zeroPad){
  
  im2col=Im2Col(Im,W,p1,p2,zeroPad)
  X=im2col$X
  z1=im2col$z1
  z2=im2col$z2
  Wl=apply(W$Weight,4,c)
  bl=W$bias
  W=W$Weight
  Z=apply(X,3,function(x) x%*%Wl+bl)
  Z=array(Z,dim=c(z1*z2,dim(W)[4],dim(Im)[4]))
  Y=apply(Z,3,function(x){
    x[x<0]=0
    return(x)
  })
  Y=array(Y,dim=c(z1,z2,dim(W)[4],dim(Im)[4]))
  return(list(new_Im=Y,Z=Z))
}
