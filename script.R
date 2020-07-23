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


rmv_outl<-function(table){ # this must be done by each lc class!
	df<-table
	#print(head(df))
 for(i in 3:32){
	#print(i)
	if(i!=28){
		x<-sd(df[,i])
		xneg<--1*x
		#print(x)
		df[abs(df[,i])>x,i]<-NA
	}
 }
 return(df)
}

s96df<-rmv_outl(df96sum)


df96sm<-melt(s96df, id=c('year','season','lc'),na.rm=T)
df01sm<-melt(s01df, id=c('year','season','lc'),na.rm=T)

s96plot <- ggplot(df96sm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")

s01plot <- ggplot(df01sm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")


