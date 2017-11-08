#Performs gradient check to verify sanity of implementation
gradientCheck<-function(Y,W,M,grads,grads_b,respMat,archTab,tol=1e-4){
  errorVec=rep(NA,2)
  res=sample(c("F","C"),1)
  res="C"
  N=nrow(respMat)
  layers=which(colnames(archTab)==res)
  c0=sample(layers,1)+1
  dims=dim(W[[c0]]$Weight)
  g=c()
  if(res=="C"){
    idxes=which(W[[c0]]$Weight!=0,arr.ind = TRUE)
    n=sample(nrow(idxes),1)
    posit=idxes[n,]
    c1=posit[1]
    c2=posit[2]
    c3=posit[3]
    c4=posit[4]
    c5=sample(which(W[[c0]]$bias!=0),1)
  }
  if(res=="F"){
    c1=sample(dims[1],1)
    c2=sample(dims[2],1)
    c3=sample(dims[2],1)
  }
  
  ####1. Weight gradient
  #right side
  WP=W
  if(res=="C")WP[[c0]]$Weight[c1,c2,c3,c4]=W[[c0]]$Weight[c1,c2,c3,c4]+tol
  if(res=="F")WP[[c0]]$Weight[c1,c2]=W[[c0]]$Weight[c1,c2]+tol
  fp1=FP(Y[[1]],WP,archTab,TRUE)
  YP=fp1$Y[[length(fp1$Y)]]
  estPlus=Objective(YP,respMat)
  #left side
  WM=W
  if(res=="C")WM[[c0]]$Weight[c1,c2,c3,c4]=W[[c0]]$Weight[c1,c2,c3,c4]-tol
  if(res=="F")WM[[c0]]$Weight[c1,c2]=W[[c0]]$Weight[c1,c2]-tol
  fp2=FP(Y[[1]],WM,archTab,TRUE)
  YM=fp2$Y[[length(fp2$Y)]]
  estMinus=Objective(YM,respMat)
  num=(estPlus-estMinus)/(2*tol)
  if(res=="C") g=grads[[c0]][c1,c2,c3,c4]
  if(res=="F") g=grads[[c0]][c1,c2]
  anal=(1/N)*g
  if(anal!=0) errorVec[1]=(num-anal)/max(abs(num),abs(anal))
  if(anal==0) errorVec[1]=num
  ####2. bias term
  WP=W
  pos=c()
  if(res=="C") pos=c5
  if(res=="F") pos=c3
  WP[[c0]]$bias[pos]=W[[c0]]$bias[pos]+tol
  fp1=FP(Y[[1]],WP,archTab,TRUE)
  YP=fp1$Y[[length(fp1$Y)]]
  estPlus=Objective(YP,respMat)
  WM=W
  WM[[c0]]$bias[pos]=W[[c0]]$bias[pos]-tol
  fp2=FP(Y[[1]],WM,archTab,TRUE)
  YM=fp2$Y[[length(fp2$Y)]]
  estMinus=Objective(YM,respMat)
  num=(estPlus-estMinus)/(2*tol)
  anal=(1/N)*grads_b[[c0]][pos]
  errorVec[2]=(num-anal)/max(abs(num),abs(anal))
  print(paste("layer",c0))
  return(list(errorvec=errorVec,res=res))
}
