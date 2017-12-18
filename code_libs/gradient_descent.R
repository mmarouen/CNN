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

#Performs BP on a set of images
BP<-function(Y, #activations
             M, #switches matrix list
             W, #shared weights
             resp, #response matrix
             arch, #architecture table
             lr #learning rate
            ){
  deltas=list()
  gradRaw=list()
  gradRaw_b=list()
  W_new=c()
  b_new=c()
  weights=list()
  deltas[[1]]=0
  gradRaw[[1]]=0
  gradRaw_b[[1]]=0
  W_new[[1]]=0
  b_new[[1]]=0
  L=length(W)
  N=nrow(resp)
  Pool_prev=FALSE
  for(l in L:2){
    # print(paste("layer:",l))
    l_name=colnames(arch)[l-1]
    if(l==L){
      deltas[[l]]=Y[[l]]-resp
      gradRaw[[l]]=t(Y[[l-1]])%*%deltas[[l]]
      gradRaw_b[[l]]=colSums(deltas[[l]])
    }
    if(l<L){
      # print(l_name)
      if(l_name=="F"){
        bp=BP_FC(Y[[l]],Y[[l-1]],deltas[[l+1]],W[[l+1]])
        deltas[[l]]=bp$deltas
        gradRaw[[l]]=bp$grads
        gradRaw_b[[l]]=bp$grads_b
      }
      if(l_name=="P" & l<(L-1)){
        p=arch[1,l-1]
        if(colnames(arch)[l]=="F")bp1=BP_FC(Y[[l]],Y[[l-1]],deltas[[l+1]],W[[l+1]],TRUE)
        if(colnames(arch)[l]=="C")bp1=BP_Conv(Y[[l]],Y[[l-1]],deltas[[l+1]],W[[l+1]],arch[,(l-1):l],FALSE,TRUE)
        deltas1=bp1$deltas
        bp=BP_Pool(deltas1,M[[l]],p)
        deltas[[l]]=bp$deltas
        gradRaw[[l]]=0
        gradRaw_b[[l]]=0
      }
      if(l_name=="C" & l<(L-1)){
        Pool_prev=ifelse(colnames(arch)[l]=="P",TRUE,FALSE)
        bp=BP_Conv(Y[[l]],Y[[l-1]],deltas[[l+1]],W[[l+1]],arch[,(l-1):l],Pool_prev,FALSE)
        deltas[[l]]=bp$deltas
        gradRaw[[l]]=bp$grads
        gradRaw_b[[l]]=bp$grads_b
      }
    }
    # gradupdate=gradUpdate(gradRaw[[l]],gradRaw_b[[l]],method="BGD")
    # gradsW=gradupdate$gradsW
    # gradsb=gradupdate$gradsb
    if(l_name=="P")weights[[l]]=0
    if(l_name!="P"){
      W_new=W[[l]]$Weight-lr*(1/N)*gradRaw[[l]]
      b_new=W[[l]]$bias-lr*(1/N)*gradRaw_b[[l]]
      weights[[l]]=list(Weight=W_new,bias=b_new)
    }
  }
  return(list(W=weights,deltas=deltas,grads=gradRaw,grads_b=gradRaw_b))
}

#BP on the conv layer
#Computes deltas & gradients with regards to weights & biases

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
    grads1=convolveMat(Y2,deltas1,k1,k2)
    grads_b=apply(deltas1,3,sum)
  }
  return(list(grads=grads,grads_b=grads_b,deltas=deltas1))
}

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

#BP on pooling layer
#Basically it upscales the gradient to the CONV layers
BP_Pool<-function(D, #deltas coming from FC or CONV layers
                  M, #switches indicating position of maximum
                  p #stride
                 ){
  h=dim(D)[1]
  w=dim(D)[2]
  d=dim(D)[3]
  N=dim(D)[4]
  D1=array(0,dim=c(h*p,w*p,d,N))
  tab=as.matrix(expand.grid(seq(1,d),seq(1,N)))
  for(x in 1:nrow(tab)){
    d0=tab[x,1]
    n=tab[x,2]
    for(i in 1:h){
      for(j in 1:w){
        i1=1+p*(i-1)
        j1=1+p*(j-1)
        Dtmp=D1[i1:(i1+p-1),j1:(j1+p-1),d0,n]
        id=M[i,j,d0,n]
        val=D[i,j,d0,n]
        if(id==1)Dtmp[1,1]=val
        if(id==2)Dtmp[2,1]=val
        if(id==3)Dtmp[1,2]=val
        if(id==4)Dtmp[2,2]=val
        D1[i1:(i1+p-1),j1:(j1+p-1),d0,n]=Dtmp
      }
    }
  }
  return(list(deltas=D1))
}
