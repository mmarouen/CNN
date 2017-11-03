{
  r=1 #epochs
  t=0 #iterations
  obj=c()

  repeat{

    fp=FP(Im,W,archTab,TRUE)
    M=fp$M
    Y=fp$Y
    bp=BP(Y,M,W,respMat,archTab,learning_rate)
    ypred=Y[[length(Y)]]
    W=bp$W
 
    obj1=Objective(ypred,respMat)
    obj=c(obj,obj1)
    print(obj1)
    # lines( 1:r,obj)
    # Sys.sleep(0)
    print(r)
    r=r+1
    if(r==100) break
  }
  return(list(yhat=ypred,obj=obj))
}
