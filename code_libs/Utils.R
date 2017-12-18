#convert a column matrix to an image.
#this function is the opposite of Im2Col

Col2Im<-function(X,W,z1,z2,p1,p2){ #inverse of Im2Col
  
  k1=dim(W)[1]
  k2=dim(W)[2]
  d=dim(W)[4]
  N=dim(X)[3]
  Im=apply(X,3,function(x) as.array(x,dim=c(z1,z2,d)))
  Im=array(Im,dim=c(z1,z2,d,N))
  return(Im)
}

#convert an Image to 3D tensor using a weight
#Input:
#  Im: image of dimension 4
#  W:  weights vector
#  p1,p2: slide H & V
#  zeroPadding: TRUE/FALSE

Im2Col<-function(Im,W,p1,p2,zeroPadding=TRUE){
  #Im: images array of size h,w,d,N
  #w:weights + bias for the layer
  #p1,p2: stride, slide
  h=dim(Im)[1]#height
  w=dim(Im)[2]#width (not be confused with W the weights)
  d=dim(Im)[3]#depth of image (number of channels)
  N=dim(Im)[4]#number of images
  if(is.list(W)) W=W$Weight
  k1=dim(W)[1]#height of filter
  k2=dim(W)[2]#width of filter
  z1=floor((h-k1)/p1)+1
  z2=floor((w-k2)/p2)+1
  if(zeroPadding){#zero padding
    padded=padZeros(Im,z1,z2)
    Im=padded$Im_padded
    p1r=padded$p1r
    p1l=padded$p1l
    p2r=padded$p2r
    p2l=padded$p2l
    z1=floor((h-k1+p1l+p1r)/p1)+1
    z2=floor((w-k2+p2r+p2l)/p2)+1
  }
  
  X=array(dim=c(z1*z2,k1*k2*d,N)) #input data
  for(i in 1:z1){
    for(j in 1:z2){
      i1=1+(i-1)*p1
      j1=1+(j-1)*p2
      Im1=Im[i1:(i1+k1-1),j1:(j1+k2-1),,]
      X[j+z2*(i-1),,]=apply(Im1,4,c)
    }
  }
  return(list(X=X,z1=z1,z2=z2))
}
#matrix cross correlation during back propagation

convolveMat<-function(Y, #input activation
                      D, #input delta
                      k1, #stride to left
                      k2 #stride down
                     ){
{
  P1=k1-1
  P2=k2-1
  p1l=ceiling(P1/2)
  p1r=floor(P1/2)
  p2l=floor(P2/2)
  p2r=ceiling(P2/2)
  
  h=dim(Y)[1]
  w=dim(Y)[2]
  C=dim(Y)[3]
  N=dim(Y)[4]
  
  idx = dim(Y) + c(p1l+p1r,p2r+p2l,0,0)
  mat1r=matrix(0,ncol=p1r,nrow=h)
  mat1l=matrix(0,ncol=p1l,nrow=h)
  mat2r=matrix(0,ncol=(w+p1l+p1r),nrow=p2r)
  mat2l=matrix(0,ncol=(w+p1l+p1r),nrow=p2l)
  h1=h+P1
  w1=w+P2
  
  Y_new=array(apply(Y,c(3,4),function(Im){
    mm=cbind(mat1l,Im,mat1r)
    mm=rbind(mat2r,mm,mat2l)
    return(mm)
  }),dim=c(h1,w1,C,N))
  
  W=array(dim=c(k1,k2,C,dim(D)[3]))
  for(i in 1:k1){
    for(j in 1:k2){
      i1=i+h-1
      j1=j+w-1
      W[i,j,,]=apply(D,3,function(x) apply(Y_new[i:i1,j:j1,,],3,function(t) sum(t*x)))
    }
  }
  return(W)
}

#flipping a kernel 180°
flip<-function(X){
  k=ncol(X)
  n=nrow(X)
  X1=X
  X1[floor(n/2):1,]=X[(ceiling(n/2)+1):n,]
  X1[n:(ceiling(n/2)+1),]=X[1:floor(n/2),]
  Xf=X1
  Xf[,floor(k/2):1]=X1[,(ceiling(k/2)+1):k]
  Xf[,k:(ceiling(k/2)+1)]=X1[,1:floor(k/2)]
  return(Xf)
}

#flip a kernel array
flipKernel<-function(W){
  flipped=apply(W,4,function(x) apply(x,3,flip))
  flipped=array(flipped,dim=dim(W))
}

#zero padding function
#Input:
#  Im: image
#  z1,z2:target size
padZeros<-function(Im,z1,z2){
  
  h=dim(Im)[1]
  w=dim(Im)[2]
  d=dim(Im)[3]
  N=dim(Im)[4]
  p1r=floor((h-z1)/2)
  p1l=ceiling((h-z1)/2)
  p2r=ceiling((w-z2)/2)
  p2l=floor((w-z2)/2)
  
  idx = dim(Im)[-4] + c(p1l+p1r,p2r+p2l,0)
  mat1r=matrix(0,ncol=p1r,nrow=h)
  mat1l=matrix(0,ncol=p1l,nrow=h)
  mat2r=matrix(0,ncol=(w+p1l+p1r),nrow=p2r)
  mat2l=matrix(0,ncol=(w+p1l+p1r),nrow=p2l)
  Im_new=apply(Im,4,function(Im0){
    apply(Im0,3, function(X){
        mm=cbind(mat1l,X,mat1r)
        mm=rbind(mat2r,mm,mat2l)
    })
  })
  Im_new=array(Im_new,dim=c(idx,N))
  return(list(p1r=p1r,p1l=p1l,p2r=p2r,p2l=p2l,Im_padded=Im_new))
}

#apply softmax function to a matrix
softmax<-function(X){
  K=ncol(X)
  soft=matrix(nrow=nrow(X),ncol=ncol(X))
  if(K==1) soft=1/(1+exp(-X))
  if(K>2){
    eps=1e-15
    Eps=1-eps
    M=max(X)
    soft=apply(X,2,function(x) exp(-M-log(rowSums(exp(X-M-x)))))
    soft=pmax(soft,eps)
    soft=pmin(soft,Eps)
  }
  return(soft)
}

#convert a response matrix into a labelized output
transformOutput<-function(ZZ,classes){
  yhat=c()
  yhatMat=matrix(ncol=ncol(ZZ[[length(ZZ)]]),nrow=nrow(ZZ[[length(ZZ)]]))
  yhatMat=ZZ[[length(ZZ)]]
  K=ncol(as.matrix(yhatMat))
  if(K==1){
    yhat=rep(0,length=nrow(yhatMat))
    yhat[yhatMat>=0.5]=1
    yhat=as.factor(yhat)
  }
  if(K>2){
    yhat=apply(yhatMat,1,function(x) classes[which.max(x)])
    yhat=as.factor(yhat)
  }
  return(list(yhat=yhat,yhatMat=yhatMat))
}

#convert a labelized vector into a response matrix
transformResponse<-function(resp){
  classes=NULL
  K=length(unique(resp))
  if(K==2){respMat=as.matrix(resp)}
  if(K>2){
    respMat=matrix(0,nrow=length(resp),ncol=K)
    classes=sort(unique(resp))
    for (i in 1:length(resp)){respMat[i,which(resp[i]==classes)]=1}
  }
  return(list(respMat=respMat,CL=classes,response=resp))
}
