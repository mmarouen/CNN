#Performs forward propagation
#Input: 
#  Im:input images
#  W: network weights
#  archi:architecture table
#  zeroPad: TRUE/FALSE


FP<-function(Im,W,archi,zeroPad){
  
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
      Z[[l]]=fp$Z
      Y[[l]]=fp$new_Im
      M[[l]]=0
    }
    if(l_name=="P"){
      p1=archi[3,l-1]
      p2=archi[4,l-1]
      mask=c(archi[1,l-1],archi[2,l-1])
      fp=FP_Pool(Y[[l-1]],mask,p1,p2,zeroPad)
      Y[[l]]=fp$Y
      M[[l]]=fp$M
      Z[[l]]=0
    }
    if(l_name=="F"){
      fcprev=TRUE
      if(colnames(archi)[l-2] != "F") fcprev=FALSE
      M[[l]]=0
      if(l<L){
        fp=FP_FC(Y[[l-1]],W[[l]],outF="ReLU",fcprev)
        Y[[l]]=fp$Y
        Z[[l]]=fp$Z
      }
      if(l==L){
        fp=FP_FC(Y[[l-1]],W[[l]],outF="Softmax",fcprev)
        Y[[l]]=fp$Y
        Z[[l]]=fp$Z
      }
    }
  }
  return(list(Y=Y,Z=Z,M=M))
}
