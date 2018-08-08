object.function=function(param){
  #re-calculate distance to HF
  nx=length(param)/2
  x.hf.new=param[1:nx]; y.hf.new=param[(nx+1):length(param)]
  dist.mat=matrix(NA,nrow(grid1),nx)
  for (i in 1:nx){
    x1=(grid1$x-x.hf.new[i])^2
    y1=(grid1$y-y.hf.new[i])^2
    dist.mat[,i]=sqrt(x1+y1)
  }
  grid1$dist1=apply(cbind(grid1$dist_hf,dist.mat),1,min)
  grid1$dist_hf.sd=(grid1$dist1-stats['media','dist_hf'])/stats['sd1','dist_hf']
  
  #re-calculate prevalence and population at risk
  tmp=coef2['(Intercept)']+
    coef2['dist_hf']*grid1$dist_hf.sd+
    coef2['dist_UC']*grid1$dist_uc.sd+
    coef2['dist_hf:dist_UC']*grid1$dist_hf.sd*grid1$dist_uc.sd
  grid1$preval=exp(tmp)/(1+exp(tmp))
  grid1$incid=grid1$preval*grid1$pop
  
  #output population at risk
  sum(grid1$incid)
}
create.plot=function(grid1,var1,num,hf){
  max1=max(grid1[,var1])
  grid1$zzz=grid1[,var1]
  if (var1=='preval') {rango=c(0,1); nome='Prevalence'}
  if (var1=='incid')  {rango=rango.incid; nome='Incidence'}
  res=ggplot() + 
    geom_tile(data = grid1, alpha = 0.8,aes(x = x, y = y,fill = zzz)) +
    scale_fill_gradient2(low = "cyan", mid = "red",high='purple',limits=rango,midpoint=rango[2]/2,
                         name = '') + 
    geom_point(data = hf, aes(x = x,y=y,size=3),shape=17,show.legend=F) +
    geom_point(data = pop, aes(x = x,y=y,size=3),shape=10,show.legend=F) +
    xlab('Longitude') + ylab("Latitude") + ggtitle(paste(num,'HF')) +
    theme(plot.title = element_text(hjust = 0.5,size=20))
    # annotate(geom="text", x=0.5, y=1, label=nome,size=20,color="black")
    # guide(colour='colorbar')
  ggsave(file=paste('graph ',num,'_',var1,'.jpeg',sep=''), res,width=7,height=7)
  res
}
