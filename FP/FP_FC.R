#performs forward propagation for FC layer using ReLU activation
#Input:
#  Im: input images
#  W: layer's weight
#  fcprev: wether previous layer is also FC layer

FP_FC<-function(Im,W,outF="ReLU",fcprev=TRUE){
  b=W$bias
  W=W$Weight
  X=Im
  if(!fcprev) X=t(apply(Im,4,c))
  Z=X%*%W+b
  if(outF=="ReLU"){
    Y=Z
    Y[Y<0]=0
  }
  if(outF=="Softmax"){
    Y=softmax(Z)
  }
  return(list(Y=Y,Z=Z))
}
