#flip a kernel array

flipKernel<-function(W){
  flipped=apply(W,4,function(x) apply(x,3,flip))
  flipped=array(flipped,dim=dim(W))
}
