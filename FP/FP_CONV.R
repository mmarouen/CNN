#performs forward propagation for convolution layer

FP_Conv<-function(Im, #input images
                  W, #weights
                  p1, #horizontal stride
                  p2, #vertical stride
                  zeroPad #binary
                 ){
  
  im2col=Im2Col(Im,W$Weight,p1,p2,zeroPad)
  X=im2col$X
  z1=im2col$z1
  z2=im2col$z2
  Wl=apply(W$Weight,4,c)
  bl=W$bias
  D=dim(W$Weight)[4]
  N=dim(Im)[4]
  Z=apply(X,3,function(x) t(t(x%*%Wl)+bl))
  Z=array(Z,dim=c(z1*z2,D,N))
  Y=apply(Z,3,function(x){
    x[x<0]=0
    return(x)
  })
  Y=array(Y,dim=c(z1,z2,D,N))
  Y=aperm(Y,c(2,1,3,4))
  return(list(new_Im=Y))
}

