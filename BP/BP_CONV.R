#BP on the conv layer
BP_Conv<-function(Y1,Y2,deltas,W,arch,poolprev,noWeight=FALSE){
  
  k1=arch[1]
  k2=arch[2]
  p1=arch[3]
  p2=arch[4]
  h=dim(Y1)[1]
  w=dim(Y1)[2]
  C=dim(Y2)[3]
  d=dim(deltas)[3]
  grads=c()
  grads_b=c()
  #compute delta
  if(poolprev)deltas1=deltas
  if(!poolprev){
    b=W$bias
    W=W$Weight
    Wf=flipKernel(W)
    im2col=Im2Col(deltas,Wf,p1,p2,TRUE)
    X=im2col$X
    z1=im2col$z1
    z2=im2col$z2
    Wf=apply(Wf,3,c)
    D1=apply(X,3,function(x) x%*%Wf+b)
    D1=array(D1,dim=c(z1*z2,dim(W)[3],dim(Y1)[4]))
    D1=aperm(array(D1,dim=c(z1,z2,dim(W)[3],dim(Y1)[4])),c(2,1,3,4))
    partial_Z=Y1
    partial_Z[Y1>0]=1
    deltas1=partial_Z*D1
  }
  if(!noWeight){#compute gradient
    grads=apply(deltas1,3,function(x) apply(Y2,3,function(t) convolveMat(t,x,k1,k2)))
    grads=array(grads,dim=c(k1,k2,C,d))
    grads_b=apply(deltas1,3,sum)
  }
  return(list(grads=grads,grads_b=grads_b,deltas=deltas1))
}
