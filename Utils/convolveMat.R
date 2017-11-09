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
