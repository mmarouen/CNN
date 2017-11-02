#performs forward propagation for pooling layer7
#Input:
#  Im: input images
#  mask:mask size default= 2x2
#  p1,p2: slide=2 horizontal, 2 vertical
#  zeropad: TRUE/FALSE

FP_Pool<-function(Im,mask,p1,p2,zeroPad){
  
  k1=mask[1]
  k2=mask[2]
  d=dim(Im)[3]
  N=dim(Im)[4]
  Wmask=array(dim=c(k1,k2,d,d))
  im2col=Im2Col(Im,Wmask,p1,p2,FALSE)
  X=im2col$X
  z1=im2col$z1
  z2=im2col$z2
  filt=rep(1:d,each=k1*k2)
  posId=seq(0,dim(X)[2]-1,by=k1*k2)
  Maxes=apply(X,c(1,3),function(x) tapply(x,filt,which.max))
  Maxes=aperm(Maxes,c(2,1,3))
  Maxes=aperm(array(Maxes,dim=c(z1,z2,d,N)),c(2,1,3,4))
  #Maxes=array(Maxes,dim=c(z1*z2,k1*k2*d,N))
  # Maxes=array(sapply(1:d,function(x) Maxes[,x:(x+d),]),dim=c(z1*z2,k1*k2,N,d))
  # Maxes=aperm(Maxes,c(1,2,4,3))
  # Maxes=apply(Maxes,c(3,4),function(x){
  #   do.call("rbind",lapply(seq(1,z1*z2,by = z2),function(l) matrix(t(x[l:(l+z1-1),]),nrow=k2)))
  # })
  # Maxes=array(Maxes,dim=c((z1-1)*p1+k1,(z2-1)*p2+k2,d,N))
  Y1=apply(X,3,function(Im){
    t(sapply(1:nrow(Im),function(x) tapply(Im[x,],filt,max)))
  })
  Y0=array(Y1,dim=c(z1,z2,dim(X)[2]/(k1*k2),dim(X)[3]))
  Y0=aperm(Y0,c(2,1,3,4))
  return(list(M=Maxes,Y=Y0))
}
