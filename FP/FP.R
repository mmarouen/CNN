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
