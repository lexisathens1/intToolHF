create.plot=function(grid1,var1){
  max1=max(grid1[,var1])
  grid1$zzz=grid1[,var1]
  rango=c(0,max(grid1$zzz));
  res=ggplot() + 
    geom_tile(data = grid1, alpha = 0.8,aes(x = x, y = y,fill = zzz)) +
    scale_fill_gradient2(low = "cyan", mid = "red",high='purple',limits=rango,midpoint=rango[2]/2,name = '') + 
    geom_point(data = hf, aes(x = x,y=y,size=3),shape=17,show.legend=F) +    
    geom_point(data = pop, aes(x = x,y=y,size=3),shape=10,show.legend=F) +
    xlab('Longitude') + ylab("Latitude")
  # annotate(geom="text", x=0.5, y=1, label=nome,size=20,color="black")
  # guide(colour='colorbar')
  ggsave(file=paste('spatial factors graph ',var1,'.jpeg',sep=''), res,width=7,height=7)
  res
}
