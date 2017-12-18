CNN<-function(Im,y,ImTest=NULL,yTest=NULL,
              archi=NULL,
              learning_rate=0.01, epochs=200,
              optAlg="GD",
              beta1=0.9,#momentum parameter
              beta2=0.99,#RMS prop parameter
              mini_batch=NULL,#process in minibatches mini batch size are in 2^p so user input only exponent term
              weightDecay=FALSE,lambda=NULL, #weight decay yes/No and decay amount 'lambda'
              #dropout=FALSE,dropout yes/no, probs=0.7
              probs=1, #probability
              gradientCheck=FALSE,traceObj=FALSE,traceWeights=FALSE #debuggers
              ){
  
  resp=transformResponse(y)
  Y=resp$respMat
  CL=resp$CL
  init=initNetwork(length(unique(y)),dim(Im),archi=NULL)
  W=init$Weight
  archtab=init$archi
  gd=gradientDescent(Im,Y,W,archtab,learning_rate)
  yhatMat=gd$yhat
  yhat=transformOutput(yhatMat,CL)$yhat
  return(list(yhat=yhat,obj=gd$obj))
}
