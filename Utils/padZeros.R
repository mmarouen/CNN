#zero padding function
#Input:
#  Im: image
#  z1,z2:target size

padZeros<-function(Im,z1,z2){
  
  h=dim(Im)[1]
  w=dim(Im)[2]
  d=dim(Im)[3]
  N=dim(Im)[4]
  p1=sample(c(floor((h-z1)/2),ceiling((h-z1)/2)),2,replace = F)
  p1r=p1[1]
  p1l=p1[2]
  p2=sample(c(floor((w-z2)/2),ceiling((w-z2)/2)),2,replace = F)
  p2r=p2[1]
  p2l=p2[2]
  
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
