rm(list=ls(all=TRUE))
library(ggplot2)

#get data
source('public health tool/spatial factors function.R')
grid1=read.csv('public health tool/fake data gridded.csv',as.is=T)
hf=read.csv('public health tool/fake data hf coord.csv',as.is=T)
pop=read.csv('public health tool/fake data popcenter.csv',as.is=T)

#show spatial distribution of pop
create.plot(grid1,'pop')

#show spatial distribution of distance to hf
create.plot(grid1,'dist_hf')

#show spatial distribution of distance to pop center
create.plot(grid1,'dist_uc')
