#BP on the conv layer
#Computes deltas & gradients with regards to weights & biases
#Implementation not yet properly vectorized, it's ongoing

BP_Conv<-function(Y1,#activation of current layer (compute deltas)
                  Y2, #activation of previous layer (compute gradients)
                  deltas, #delta for following layer 
                  W, #weight from following layer
                  arch, #architecture details for current and next layers
                  poolprev, #binary to indicate if previous layer is pooling layer
                  noWeight=FALSE #binary to indicate if need compute gradients or not
                 ){
  
  arch1=arch[,1]
  arch2=arch[,2]
  k1=arch1[1]
  k2=arch1[2]
  p1=arch2[3]
  p2=arch2[4]
  h=dim(Y1)[1]
  w=dim(Y1)[2]
  C=dim(Y2)[3]
  grads=c()
  grads_b=c()
  #compute delta
  if(poolprev)deltas1=deltas
  if(!poolprev){
    b=W$bias
    Wf=flipKernel(W$Weight)
    im2col=Im2Col(deltas,Wf,p1,p2,TRUE)
    X=im2col$X
    z1=im2col$z1
    z2=im2col$z2
    Wf=apply(Wf,3,c)
    D1=apply(X,3,function(x) x%*%Wf)
    D1=array(D1,dim=dim(Y1))
    D1=aperm(D1,c(2,1,3,4))
    partial_Z=Y1
    partial_Z[Y1>0]=1
    deltas1=partial_Z*D1
  }
  if(!noWeight){#compute gradient
    #deltas1f=flipKernel(deltas1)
    d=dim(deltas1)[3]
    grads=array(dim  =c(k1,k2,C,d))
    Y11=apply(Y2,4,function(x)apply(x,3,function(t){
      tt=cbind(0,t,0)
      tt2=rbind(0,tt,0)
      return(tt2)
    }))
    Y11=array(Y11,dim=c(dim(Y2)[1]+2,dim(Y2)[2]+2,dim(Y2)[3],dim(Y2)[4]))
    n1=dim(Y1)[1]
    n2=dim(Y1)[2]
    for(i in 1:C){
      for(j in 1:d){
        for(kk1 in 1:3){
          for(kk2 in 1:3){
            grads[kk1,kk2,i,j]=sum(deltas1[,,j,]*Y11[kk1:(kk1+n1-1),kk2:(kk2+n2-1),i,])
          }
        }
      }
    }
    grads_b=apply(deltas1,3,sum)
  }
  return(list(grads=grads,grads_b=grads_b,deltas=deltas1))
}
