get.incid.preval=function(coef2,grid1,x.hf.new,y.hf.new,stats){
  #get distance to nearest health facility
  nx=length(x.hf.new)
  dist.mat=matrix(NA,nrow(grid1),nx)
  for (i in 1:nx){
    x1=(grid1$x-x.hf.new[i])^2
    y1=(grid1$y-y.hf.new[i])^2
    dist.mat[,i]=sqrt(x1+y1)
  }
  grid1$dist1=apply(cbind(grid1$dist_hf,dist.mat),1,min)
  grid1$dist_hf.sd=(grid1$dist1-stats['media','dist_hf'])/stats['sd1','dist_hf']
  
  #calculate malaria prevalence and population at risk
  tmp=coef2['(Intercept)']+
    coef2['dist_hf']*grid1$dist_hf.sd+
    coef2['dist_UC']*grid1$dist_uc.sd+
    coef2['dist_hf:dist_UC']*grid1$dist_hf.sd*grid1$dist_uc.sd
  
  grid1$preval=exp(tmp)/(1+exp(tmp))
  grid1$incid=grid1$preval*grid1$pop
  grid1
}