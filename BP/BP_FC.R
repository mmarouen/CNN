#performs BP on a FC layer
BP_FC<-function(Y1, #output of layer l+1
                Y2, #output of layer l-1
                deltas, #derivatives w.r.t. Z
                W, #weights
                noWeight=FALSE #only to compute deltas or not
               ){
  
  grads=c()
  grads_b=c()
  b=W$bias
  W=W$Weight
  deltas1=deltas%*%t(W)
  partial_Z=Y1
  partial_Z[Y1>0]=1
  if(noWeight) deltas1=array(deltas1,dim(Y1))
  deltas1=partial_Z*deltas1
  if(!noWeight){
    if(length(dim(Y2))>2) Y2=t(apply(Y2,4,c))
    grads=t(Y2)%*%deltas1
    grads_b=colSums(deltas1)
  }
  return(list(grads=grads,grads_b=grads_b,deltas=deltas1))
}
