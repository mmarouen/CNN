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
