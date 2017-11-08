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
