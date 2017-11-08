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

