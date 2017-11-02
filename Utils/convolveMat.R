#matrix cross correlation
#input:
#  Y,D: input matrices (not inter-changeable)
#  k1,k2: slide H & V

convolveMat<-function(Y,D,k1,k2){
  P1=k1-1
  P2=k2-1
  sample1=sample(c(floor(P1/2),ceiling(P1/2)),2)
  p1l=sample1[1]
  p1r=sample1[2]
  sample2=sample(c(floor(P2/2),ceiling(P2/2)),2)
  p2l=sample2[1]
  p2r=sample2[2]
  
  h=dim(Y)[1]
  w=dim(Y)[2]
  N=dim(Y)[3]
  
  idx = dim(Y) + c(p1l+p1r,p2r+p2l,0)
  mat1r=matrix(0,ncol=p1r,nrow=h)
  mat1l=matrix(0,ncol=p1l,nrow=h)
  mat2r=matrix(0,ncol=(w+p1l+p1r),nrow=p2r)
  mat2l=matrix(0,ncol=(w+p1l+p1r),nrow=p2l)
  h1=h+P1
  w1=w+P2
  
  Y_new=array(apply(Y,3,function(Im){
    mm=cbind(mat1l,Im,mat1r)
    mm=rbind(mat2r,mm,mat2l)
    return(mm)
  }),dim=c(h1,w1,N))

  #Y1=array(aperm(Y,c(2,1,3,4)),dim=c(h*w,dim(Y)[3],dim(Y)[4]))
  W=matrix(nrow=k1,ncol=k2)
  for(i in 1:k1){
    for(j in 1:k2){
      i1=i+h-1
      j1=j+w-1
      W[i,j]=sum(D*Y_new[i:i1,j:j1,])
    }
  }
  #W=apply(W,c(1,2),sum)
  return(W)
}
