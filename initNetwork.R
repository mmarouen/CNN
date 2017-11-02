#initialize network weights
#Inputs:
#K: number of neurons in final layer
#dims: dimension of input images
#archi (optional): architecture table
initNetwork<-function(K,dims,archi=NULL){
  
  h=dims[1]
  w=dims[2]
  d0=dims[3]
  weights=list()
  if(is.null(archi)){ #default architecture
    archi=matrix(ncol=12,nrow = 5)
    colnames(archi)=c(rep(c("C","C","P"),3),"F","F","F")
    rownames(archi)=c("Fsize_r","Fsize_l","Fstride_r","Fstride_l","Nnumber")
    archi[1,]=c(rep(c(3,3,2),3),NA,NA,NA)
    archi[2,]=c(rep(c(3,3,2),3),NA,NA,NA)
    archi[3,]=c(rep(c(1,1,2),3),NA,NA,NA)
    archi[4,]=c(rep(c(1,1,2),3),NA,NA,NA)
    archi[5,]=c(8,8,NA,16,16,NA,32,32,NA,64,64,K)
  }
  L=ncol(archi)+ 1#1 for input layer, final layer already included
  ImSizes=list()
  ImSizes[[1]]=c(h,w)
  for(l in 2:L){
    if(colnames(archi)[l-1]=="P"){
      h=floor((h-archi[1,l-1])/archi[3,l-1])+1
      w=floor((w-archi[2,l-1])/archi[4,l-1])+1
    }
    ImSizes[[l]]=c(h,w)
  } 
  weights=lapply(2:L,function(x){
    x=x-1
    if(is.na(archi[5,x])) return(0)
    if(!is.na(archi[5,x])){
      bias=rnorm(archi[5,x])
      d=archi[5,x]
      if(x==1) dprec=d0
      if(x!=1){
        if(colnames(archi)[x-1]!="P") dprec=archi[5,x-1]
        if(x!=1 & colnames(archi)[x-1]=="P") dprec=archi[5,x-2]
      }
      if(colnames(archi)[x]=="C"){
        k1=archi[1,x]
        k2=archi[2,x]
        weight=array(rnorm(k1*k2*d*dprec)*sqrt(2/(dprec*k1*k2)),dim = c(k1,k2,dprec,d))
      }
      if(colnames(archi)[x]=="F"){
        if(colnames(archi)[x-1]=="P"){
          dprec=archi[5,x-2]
          k1prec=ImSizes[[x]][1]
          k2prec=ImSizes[[x]][2]
        }
        if(colnames(archi)[x-1]=="F"){
          k1prec=1
          k2prec=1
        }
        weight=matrix(rnorm(dprec*k1prec*k2prec*d)*sqrt(2/(dprec*k2prec*k1prec)),nrow=dprec*k2prec*k1prec,ncol=d)
      }
      return(list(bias=bias,Weight=weight))
    }
  })
  weights=c(0,weights)
  return(list(Weight=weights,archi=archi,ImSizes=ImSizes))
}
