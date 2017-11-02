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
