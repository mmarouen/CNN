#BP on pooling layer
BP_Pool<-function(D, #deltas of next layer
                  M, #indices of max
                  p #padding
                 ){
  h=dim(D)[1]
  w=dim(D)[2]
  d=dim(D)[3]
  N=dim(D)[4]
  D1=array(0,dim=c(h*p,w*p,d,N))
  tab=as.matrix(expand.grid(seq(1,d),seq(1,N)))
  for(x in 1:nrow(tab)){
    d0=tab[x,1]
    n=tab[x,2]
    for(i in 1:h){
      for(j in 1:w){
        i1=1+p*(i-1)
        j1=1+p*(j-1)
        Dtmp=D1[i1:(i1+p-1),j1:(j1+p-1),d0,n]
        id=M[i,j,d0,n]
        val=D[i,j,d0,n]
        if(id==1)Dtmp[1,1]=val
        if(id==2)Dtmp[2,1]=val
        if(id==3)Dtmp[1,2]=val
        if(id==4)Dtmp[2,2]=val
        D1[i1:(i1+p-1),j1:(j1+p-1),d0,n]=Dtmp
      }
    }
  }
  return(list(deltas=D1))
}
