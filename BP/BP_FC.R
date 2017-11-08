#Performs BP on FC layer

BP_FC<-function(Y1,#current activations
                Y2,#activations for layers l-1
                deltas, #deltas from layer l+1
                W, #weights from layer l+1
                noWeight=FALSE #binary switch indicating if need compute gradients wrt weights
               ){
  
  grads=c()
  grads_b=c()
  b=W$bias
  W=W$Weight
  deltas1=deltas%*%t(W)
  partial_Z=Y1
  partial_Z[Y1>0]=1
  if(noWeight) deltas1=array(t(deltas1),dim(Y1))
  deltas1=partial_Z*deltas1
  if(!noWeight){
    if(length(dim(Y2))>2) Y2=t(apply(Y2,4,c))
    grads=t(Y2)%*%deltas1
    grads_b=colSums(deltas1)
  }
  
  return(list(grads=grads,grads_b=grads_b,deltas=deltas1))
}
