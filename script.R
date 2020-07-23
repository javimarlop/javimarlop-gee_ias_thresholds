#1: 'natural trees',
#2: 'open trees',
#3: 'dense scrub',
#4: 'open scrub',
#5: 'rainfed trees',
#6: 'rainfed crops',
#7: 'irrigated trees',
#8: 'irrigated crops',
#9: 'greenhouses',
#10: 'unproductive area'

library(reshape)
library(ggplot2)

df96sum<-read.table('sample_data_summer_1996.csv',header=T,sep=',')
df96win<-read.table('sample_data_winter_1996.csv',header=T,sep=',')
df01sum<-read.table('sample_data_summer_2001.csv',header=T,sep=',')
df01win<-read.table('sample_data_winter_2001.csv',header=T,sep=',')
df09sum<-read.table('sample_data_summer_2009.csv',header=T,sep=',')
df09win<-read.table('sample_data_winter_2009.csv',header=T,sep=',')

df96sm<-melt(df96sum, id=c('year','season','lc'))
df01sm<-melt(df01sum, id=c('year','season','lc'))

s96 <- ggplot(df96sm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")

s01 <- ggplot(df01sm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")


