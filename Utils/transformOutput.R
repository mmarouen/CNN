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
