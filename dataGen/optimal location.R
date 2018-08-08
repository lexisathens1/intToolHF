rm(list=ls(all=TRUE))
library(ggplot2)

#get data
source('dataGen/optimal location function.R')
grid1=read.csv('dataGen/fake data gridded.csv',as.is=T)
pop=read.csv('dataGen/fake data popcenter.csv',as.is=T)
hf=read.csv('dataGen/fake data hf coord.csv',as.is=T)

coef1=read.csv('dataGen/glmer coeff.csv',as.is=T)
coef2=coef1[,'Estimate']
names(coef2)=coef1$X
stats=read.csv('dataGen/glmer covariate stats.csv',as.is=T)
rownames(stats)=stats$X

#calculate prevalence and incidence based on existing HF
grid1$dist_hf.sd=(grid1$dist_hf-stats['media','dist_hf'])/stats['sd1','dist_hf']
grid1$dist_uc.sd=(grid1$dist_uc-stats['media','dist_UC'])/stats['sd1','dist_UC']
tmp=coef2['(Intercept)']+
    coef2['dist_hf']*grid1$dist_hf.sd+
    coef2['dist_UC']*grid1$dist_uc.sd+
    coef2['dist_hf:dist_UC']*grid1$dist_hf.sd*grid1$dist_uc.sd
grid1$preval=exp(tmp)/(1+exp(tmp))
grid1$incid=grid1$preval*grid1$pop
tot.antes=sum(grid1$incid)
rango.incid=c(0,max(grid1$incid))
write.csv(grid1,'dataGen/fake data gridded precalc.csv',row.names=F)

#show predicted prevalence with existing HF
create.plot(grid1,'preval',num=1,hf)

#show predicted incidence with existing HF
create.plot(grid1,'incid',num=1,hf)

#get optimal location
size.land=15
init=list()
init[[1]]=c(0.5,0.5)*size.land
init[[2]]=c(0.1,0.8,
            0.1,0.8)*size.land
init[[3]]=c(0,0.5,0.9,
            0,0.5,0.9)*size.land

fim=numeric()
for (i in 1:3){
  #optimize function
  nx=length(init[[i]])/2
  res=optim(init[[i]],object.function,lower=rep(0,nx),upper=rep(size.land,nx),method='L-BFGS-B')
  
  #get results
  x.hf.new=res$par[1:nx]
  y.hf.new=res$par[(nx+1):length(res$par)]
  hf1=data.frame(x=c(hf$x,x.hf.new),y=c(hf$y,y.hf.new))
  
  print(c(i,res$value/tot.antes,res$convergence,res$message))
  
  #re-calculate distance to HF
  dist.mat=matrix(NA,nrow(grid1),nx)
  for (i in 1:nx){
    x1=(grid1$x-x.hf.new[i])^2
    y1=(grid1$y-y.hf.new[i])^2
    dist.mat[,i]=sqrt(x1+y1)
  }
  grid1$dist1=apply(cbind(grid1$dist_hf,dist.mat),1,min)
  grid1$dist_hf.sd=(grid1$dist1-stats['media','dist_hf'])/stats['sd1','dist_hf']
  
  #re-calculate preval and incidence with new HF
  tmp=coef2['(Intercept)']+
    coef2['dist_hf']*grid1$dist_hf.sd+
    coef2['dist_UC']*grid1$dist_uc.sd+
    coef2['dist_hf:dist_UC']*grid1$dist_hf.sd*grid1$dist_uc.sd
  grid1$preval=exp(tmp)/(1+exp(tmp))
  grid1$incid=grid1$preval*grid1$pop
  
  #show predicted prevalence with new HF
  create.plot(grid1,'preval',num=i+1,hf1)
  
  #show predicted incidence with existing HF
  create.plot(grid1,'incid',num=i+1,hf1)  
  
  #store optimized results
  tmp=cbind(i,x.hf.new,y.hf.new)
  fim=rbind(fim,tmp)
}
colnames(fim)=c('nhf','x','y')
write.csv(fim,'dataGen/optimized hf coord.csv',row.names=F)
