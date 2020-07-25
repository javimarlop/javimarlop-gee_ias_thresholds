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
		for(j in 1:10){
			x<-quantile(df[df$lc==j,i],c(0.05,0.95))
			xmax<-x[2]
			xmin<-x[1]
			#xneg<--1*x
			#print(x)
			df[df$lc==j & df[,i]>xmax,i]<-NA
			df[!is.na(df[,i]) & df$lc==j & df[,i]<xmin,i]<-NA
	}
	}
 }
 return(df)
}

s96df<-rmv_outl(df96sum)
s01df<-rmv_outl(df01sum)
s09df<-rmv_outl(df09sum)
w96df<-rmv_outl(df96win)
w01df<-rmv_outl(df01win)
w09df<-rmv_outl(df09win)

df96sm<-melt(s96df, id=c('year','season','lc'),na.rm=T)
df01sm<-melt(s01df, id=c('year','season','lc'),na.rm=T)
df09sm<-melt(s09df, id=c('year','season','lc'),na.rm=T)
df96wm<-melt(w96df, id=c('year','season','lc'),na.rm=T)
df01wm<-melt(w01df, id=c('year','season','lc'),na.rm=T)
df09wm<-melt(w09df, id=c('year','season','lc'),na.rm=T)

s96plot <- ggplot(df96sm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")
ggsave('s96plot.png',s96plot,scale=2,width=16,height=8,uni='cm')
s01plot <- ggplot(df01sm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")
ggsave('s01plot.png',s01plot,scale=2,width=16,height=8,uni='cm')
s09plot <- ggplot(df09sm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")
ggsave('s09plot.png',s09plot,scale=2,width=16,height=8,uni='cm')

w96plot <- ggplot(df96wm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")
ggsave('w96plot.png',w96plot,scale=2,width=16,height=8,uni='cm')
w01plot <- ggplot(df01wm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")
ggsave('w01plot.png',w01plot,scale=2,width=16,height=8,uni='cm')
w09plot <- ggplot(df09wm, aes(as.factor(lc), value)) + geom_boxplot(aes(colour = season)) + facet_wrap(vars(variable), scales = "free")
ggsave('w09plot.png',w09plot,scale=2,width=16,height=8,uni='cm')

# summer thresholds

dfs<-rbind(s96df,s01df,s09df)
attach(dfs)

iTrees<-
##GCVI_max	>=1.5 &
##GCVI_mean	>=1.25 &
GCVI_min	>=0.98 & # very good predictor
##MTI	>=1 &
#NDBI_max	<=0 & # good predictor
#NDBI_mean	<=0 &
##NDBI_min	<=-0.1 &
##NDVI_max	>=0.4 &
##NDVI_mean	>=0.3 &
##NDVI_min	>=0.25 &
#NDWI_min	>=0 & # good predictor
#NDWI_max	>= -0.1 &
#NDWBI_max <= -0.1 &
##WGI_max	>=0.25 &
##WGI_mean	>=0.1 &
#WGI_min	>=0 & # good predictor
#slope	<5 &
##blue	>1000 &
#green	>1000 
##red	>1500 
nir	>2650 # very good predictor
##swir1	>2500 

table(dfs$lc[iTrees]) 
#3    7    8 
#9 6721   69 


# LDA summer

library(MASS)
library(ROCR)

smp_size <- floor(0.75 * nrow(dfs))
train_ind <- sample(nrow(dfs), size = smp_size)

train.df <- dfs[train_ind, ]
test.df <- dfs[-train_ind, ]

#f <- paste(names(train.df)[28], "~", paste(names(train.df)[-c(1,2,28)], collapse=" + "))
f <- paste("as.factor(train.df$lc) ~ ", paste(names(train.df)[-c(1,2,28)], collapse=" + "))
summer.lda <- lda(as.formula(paste(f)), data = train.df)

summer.lda.predict <- predict(summer.lda, newdata = test.df)

summer.lda.predict$class

table(summer.lda.predict$class,test.df$lc)


# Get the posteriors as a dataframe.
summer.lda.predict.posteriors <- as.data.frame(summer.lda.predict$posterior)
# Evaluate the model
pred <- prediction(summer.lda.predict.posteriors[,2], test.df$lc)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
# Plot
plot(roc.perf)
#abline(a=0, b= 1)
#text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

