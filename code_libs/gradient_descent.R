#Performs forward propagation
FP<-function(Im, #input images (4D tensor
             W, #input weights (4-D tensor
             archi, #architecture matrix
             zeroPad #binary
            ){
  
  L=length(W)
  Z=list() #linear convolutions
  Y=list() #activations
  M=list() #indices max values
  Z[[1]]=0 #convolution Image, weights
  Y[[1]]=Im #transfer function applied to Z
  M[[1]]=0
  fcprev=FALSE
  for(l in 2:L){
    l_name=colnames(archi)[l-1]
    # print(paste("layer:",l))
    if(l_name=="C"){
      p1=archi[3,l-1]
      p2=archi[4,l-1]
      fp=FP_Conv(Y[[l-1]],W[[l]],p1,p2,zeroPad)
      Y[[l]]=fp$new_Im
      M[[l]]=0
    }
    if(l_name=="P"){
      p1=archi[3,l-1]
      p2=archi[4,l-1]
      mask=c(archi[1,l-1],archi[2,l-1])
      fp=FP_Pool(Y[[l-1]],mask,p1,p2)
      Y[[l]]=fp$Y
      M[[l]]=fp$M
    }
    if(l_name=="F"){
      outfunction=ifelse(l==L,"Softmax","ReLU")
      fcprev=ifelse(colnames(archi)[l-2] == "F",TRUE,FALSE)
      fp=FP_FC(Y[[l-1]],W[[l]],outF=outfunction,fcprev)
      Y[[l]]=fp$Y
      M[[l]]=0
    }
  }
  return(list(Y=Y,M=M))
}

{
  r=1 #epochs
  t=0 #iterations
  obj=c()

  repeat{

    fp=FP(Im,W,archTab,TRUE)
    M=fp$M
    Y=fp$Y
    bp=BP(Y,M,W,respMat,archTab,learning_rate)
    ypred=Y[[length(Y)]]
    W=bp$W
 
    obj1=Objective(ypred,respMat)
    obj=c(obj,obj1)
    print(obj1)
    # lines( 1:r,obj)
    # Sys.sleep(0)
    print(r)
    r=r+1
    if(r==100) break
  }
  return(list(yhat=ypred,obj=obj))
}

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

#performs forward propagation for FC layer using ReLU activation
#Input:
FP_FC<-function(Im, #  Im: input images
                W, #  W: layer's weight
                outF="ReLU",  #activation function
                fcprev=TRUE #  fcprev: wether previous layer is also FC layer
               ){
  b=W$bias
  W=W$Weight
  X=Im
  if(!fcprev) X=t(apply(Im,4,c))
  Z=t(t(X%*%W)+b)
  if(outF=="ReLU"){
    Y=Z
    Y[Y<0]=0
  }
  if(outF=="Softmax"){
    Y=softmax(Z)
  }
  return(list(Y=Y))
}

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
  Y1=apply(X,3,function(Im){
    t(sapply(1:nrow(Im),function(x) tapply(Im[x,],filt,max)))
  })
  Y0=array(Y1,dim=c(z1,z2,dim(X)[2]/(k1*k2),dim(X)[3]))
  Y0=aperm(Y0,c(2,1,3,4))
  return(list(M=Maxes,Y=Y0))
}

