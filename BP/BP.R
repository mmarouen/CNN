#Performs BP on a set of images
#input: 
#  Y: output vectors
#  M: maxpool indices
#  W: weights
#  resp: response vector
#  arch: architecture table
#  lr: learning rate

BP<-function(Y,M,W,resp,arch,lr){
  deltas=list()
  gradRaw=list()
  gradRaw_b=list()
  W_new=c()
  b_new=c()
  weights=list()
  deltas[[1]]=0
  gradRaw[[1]]=0
  gradRaw_b[[1]]=0
  W_new[[1]]=0
  b_new[[1]]=0
  L=length(W)
  N=nrow(resp)
  Pool_prev=FALSE
  for(l in L:2){
    # print(paste("layer:",l))
    l_name=colnames(arch)[l-1]
    if(l==L){
      deltas[[l]]=Y[[l]]-resp
      gradRaw[[l]]=t(Y[[l-1]])%*%deltas[[l]]
      gradRaw_b[[l]]=colSums(deltas[[l]])
    }
    if(l<L){
      # print(l_name)
      if(l_name=="F"){
        bp=BP_FC(Y[[l]],Y[[l-1]],deltas[[l+1]],W[[l+1]])
        deltas[[l]]=bp$deltas
        gradRaw[[l]]=bp$grads
        gradRaw_b[[l]]=bp$grads_b
      }
      if(l_name=="P" & l<(L-1)){
        p=arch[1,l-1]
        if(colnames(arch)[l]=="F"){
          bp1=BP_FC(Y[[l]],Y[[l-1]],deltas[[l+1]],W[[l+1]],TRUE)
        }
        if(colnames(arch)[l]=="C"){
          poolprev=ifelse(colnames(arch)[l+1]=="P",TRUE,FALSE)
          bp1=BP_Conv(Y[[l]],Y[[l-1]],deltas[[l+1]],W[[l+1]],arch[,l],poolprev,TRUE)
        }
        deltas1=bp1$deltas
        bp=BP_Pool(deltas1,M[[l]],p)
        deltas[[l]]=bp$deltas
        gradRaw[[l]]=0
        gradRaw_b[[l]]=0
      }
      if(l_name=="C" & l<(L-1)){
        Pool_prev=ifelse(colnames(arch)[l]=="P",TRUE,FALSE)
        bp=BP_Conv(Y[[l]],Y[[l-1]],deltas[[l+1]],W[[l+1]],arch[,l-1],Pool_prev,FALSE)
        deltas[[l]]=bp$deltas
        gradRaw[[l]]=bp$grads
        gradRaw_b[[l]]=bp$grads_b
      }
    }
    # gradupdate=gradUpdate(gradRaw[[l]],gradRaw_b[[l]],method="BGD")
    # gradsW=gradupdate$gradsW
    # gradsb=gradupdate$gradsb
    if(l_name=="P")weights[[l]]=0
    if(l_name!="P"){
      W_new=W[[l]]$Weight-lr*(1/N)*gradRaw[[l]]
      b_new=W[[l]]$bias-lr*(1/N)*gradRaw_b[[l]]
      weights[[l]]=list(Weight=W_new,bias=b_new)
    }
    
  }
  return(list(W=weights,deltas=deltas,grads=gradRaw,grads_b=gradRaw_b))
}
