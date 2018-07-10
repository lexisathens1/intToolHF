convert.matrix=function(grid1,var1,uni.x,uni.y){
  nx=length(uni.x)
  grid2=matrix(NA,nx,nx)
  for (i in 1:nx){
    cond=grid1$x==uni.x[i]
    tmp=grid1[cond,]
    tmp1=tmp[order(tmp$y),]
    grid2[,i]=tmp1[,var1]
  }
  round(grid2,3)
}
get.incid.preval=function(coef1,grid1,x.hf.new,y.hf.new){
  nx=length(x.hf.new)
  dist.mat=matrix(NA,nrow(grid1),nx)
  for (i in 1:nx){
    x1=(grid1$x-x.hf.new[i])^2
    y1=(grid1$y-y.hf.new[i])^2
    dist.mat[,i]=sqrt(x1+y1)
  }
  grid1$dist1=apply(cbind(grid1$dist.orig,dist.mat),1,min)
  
  tmp=coef1[1]+coef1[,'Dist_HF']*grid1$dist1+coef1[,'wealth']*grid1$wealth+
    coef1[,'Dist_HF.wealth']*grid1$dist1*grid1$wealth
  grid1$preval=exp(tmp)/(1+exp(tmp))
  grid1$incid=grid1$preval*grid1$pop
  grid1
}