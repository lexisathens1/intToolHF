rm(list=ls(all=TRUE))
library(ggplot2)
set.seed(1)

#define size of landscape
size.land=15

#create gridded variables for wealth and pop
combo=expand.grid(x=seq(from=0,to=1,length.out=101),y=seq(from=0,to=1,length.out=101))*size.land
pop.center=data.frame(x=c(0.75,0.3,0.6),y=c(0.6,0.8,0.2))*size.land
pop1=dnorm(combo$x,mean=pop.center$x[1],sd=size.land*0.2)*
     dnorm(combo$y,mean=pop.center$y[1],sd=size.land*0.15)*500
pop2=dnorm(combo$x,mean=pop.center$x[2],sd=size.land*0.2)*
     dnorm(combo$y,mean=pop.center$y[2],sd=size.land*0.1)*750
pop3=dnorm(combo$x,mean=pop.center$x[3],sd=size.land*0.2)*
     dnorm(combo$y,mean=pop.center$y[3],sd=size.land*0.15)*500
combo$pop=pop1+pop2+pop3

#look at the pop patterns we have created
res=ggplot() + 
  geom_tile(data = combo, alpha = 0.8,aes(x = x, y = y,fill = pop)) +
  scale_fill_gradient2(low = "cyan", mid = "red",high='purple',limits=c(0,max(combo$pop)),midpoint=max(combo$pop)/2)
res

#calculate distance to pop centers
dist1=numeric()
for (i in 1:nrow(pop.center)){
  x2=(combo$x-pop.center$x[i])^2
  y2=(combo$y-pop.center$y[i])^2
  dist1=cbind(dist1,sqrt(x2+y2))
}
combo$dist_uc=apply(dist1,1,min)

#export hf spatial coordinates
hf=matrix(c(0.25,0.75),1,2)*size.land
colnames(hf)=c('x','y')
write.csv(hf,'dataGen/fake data hf coord.csv',row.names=F)

#calculate distance to HF
x2=(combo$x-hf[,'x'])^2
y2=(combo$y-hf[,'y'])^2
combo$dist_hf=sqrt(x2+y2)

write.csv(combo,'dataGen/fake data gridded.csv',row.names=F)
write.csv(pop.center,'dataGen/fake data popcenter.csv',row.names=F)

