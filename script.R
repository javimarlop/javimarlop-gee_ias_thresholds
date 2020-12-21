# GNU General Public License v3.0
# Author: Javier Martínez-López

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
df09sum<-read.table('sample_data_summer_2009.csv',header=T,sep=',')
df01sum<-read.table('sample_data_summer_2001.csv',header=T,sep=',')

df01win<-read.table('sample_data_winter_2001.csv',header=T,sep=',')
df09win<-read.table('sample_data_winter_2009.csv',header=T,sep=',')
df96win<-read.table('sample_data_winter_1996.csv',header=T,sep=',')


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

## only for plots
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
## only for plots


# Trees summer thresholds 

dfs<-rbind(s96df,s01df,s09df)

attach(dfs)

isTrees<-
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


table(dfs$lc[isTrees]) 
#3    7    8 
#9 6721   69 


# Tree winter thresholds (maybe not necessary?)

dfw<-rbind(w96df,w01df,w09df)
attach(dfw)

iwTrees<-
##GCVI_max	>2 &
##GCVI_mean	>1.5 &
##GCVI_min	> 1 &
##GCVI_std > 0.5 &
MTI     	> 3 & # ok
NDBI_max	< -0.1 & # imp
##NDBI_mean	< -0.1 &
##NDBI_min	< -0.2 &
##NDVI_max	> 0.6 &
#NDVI_mean	...
#NDVI_min	...
#NDVI_std   ...
#NDWBI_max  ...
#NDWBI_mean ...
##NDWBI_min  < -0.25 & # Ted
##NDWI_max	> 0.3 &
#NDWI_mean  ...
#NDWI_min   ...
#NDWI_std   ...
##WGI_max	> 1 &
##WGI_mean	> 0.3 &
##WGI_min	> 0 & # Ted
WGI_std    > 0.25  # ok # 0.2
#blue	    ...
#green		...
#nir		...
#red		...
##slope		< 4 # Ted  
##swir1		...

table(dfw$lc[iwTrees]) 


# Crops summer thresholds 

isCrops<-
#GCVI_max	> 1.5 &
##GCVI_mean	 &
##GCVI_min	 &
#GCVI_std  > 0.3 &
#MTI  > 3 & 
#NDBI_max	> 0.05 & 
NDBI_mean	 > 0 & #looks good to differenciate from iTress
#NDBI_min	 > -0.05 &
NDVI_max	 < 0.3 & #looks good to differenciate from iTress
#NDVI_mean	< 0.25 &
#NDVI_min	< 0.15 & 
#NDVI_std   > 0.075 & #looks good to differenciate from iTress
#NDWBI_max  ...
#NDWBI_mean < -0.35 & 
#NDWBI_min  ...
##NDWI_max	 &
#NDWI_mean  ...
#NDWI_min   ...
#NDWI_std   > 0.075 & 
##WGI_max	 &
##WGI_mean	 &
WGI_min	< -0.05 & #looks good to differenciate from iTress
#WGI_std     
#blue	    ...
#green		...
nir		> 2500  
#red		...
#slope		< 5 
##swir1		...

table(dfs$lc[isCrops]) 

#    2     3     4     5     6     7     8     9    10 
#  280  4526  5420 10242  9927  1006  5149  2205  5489 



# Crops winter thresholds 

iwCrops<-
##GCVI_max	 &
##GCVI_mean	 &
##GCVI_min	 &
##GCVI_std  &
#MTI  & 
#NDBI_max	 & 
##NDBI_mean	 &
##NDBI_min	 &
##NDVI_max	 &
#NDVI_mean	...
#NDVI_min	...
#NDVI_std   ...
#NDWBI_max  ...
#NDWBI_mean ...
#NDWBI_min  ...
##NDWI_max	 &
#NDWI_mean  ...
#NDWI_min   ...
#NDWI_std   ...
##WGI_max	 &
##WGI_mean	 &
#WGI_min	...
#WGI_std     
#blue	    ...
#green		...
#nir		...
#red		...
#slope		...  
##swir1		...

table(dfs$lc[iwCrops]) 


# https://sebastianraschka.com/Articles/2014_python_lda.html
# LDA summer

library(MASS)
library(ROCR)
library(caret)
library(mda)
library(klaR)

# All clasess
smp_size <- floor(0.75 * nrow(dfs))
train_ind <- sample(nrow(dfs), size = smp_size)
train.df <- dfs[train_ind, ]
test.df <- dfs[-train_ind, ]
f <- paste("as.factor(train.df$lc) ~ ", paste(names(train.df)[-c(1,2,28)], collapse=" + "))
summer.lda <- lda(as.formula(paste(f)), data = train.df)
summer.lda.predict <- predict(summer.lda, newdata = test.df)
#summer.lda.predict$class
confusionMatrix(table(summer.lda.predict$class,test.df$lc))

plot(summer.lda.predict$x[,1],summer.lda.predict$x[,2],type='n')
text(summer.lda.predict$x[,1],summer.lda.predict$x[,2],lab=test.df$lc,col=test.df$lc)
#text(summer.lda.predict$x[,1],summer.lda.predict$x[,2],lab=summer.lda.predict$class)#,col=summer.lda.predict$class)

plot(factor(test.df$lc),summer.lda.predict$x[,1])
plot(factor(test.df$lc),summer.lda.predict$x[,2]) # very good for class 7

dfy<-rbind(w96df,w01df,w09df,s96df,s01df,s09df)

dfs378<-dfs
dfs23<-dfs
ind78<-dfs$lc==7 | dfs$lc==8
ind7<-dfs$lc==7
ind8<-dfs$lc==8

dfs78<-dfs[ind78,]
dfs378$lc[!ind78]<-3

dfs23$lc[!ind78]<-3
dfs23$lc[ind78]<-2

dfs73<-dfs23[!ind8,]
dfs83<-dfs23[!ind7,]

# Irrigated trees and crops
smp_size78 <- floor(0.75 * nrow(dfs78))
train_ind78 <- sample(nrow(dfs78), size = smp_size78)
train78.df <- dfs78[train_ind78, ]
test78.df <- dfs78[-train_ind78, ]
f <- paste("as.factor(train78.df$lc) ~ ", paste(names(train78.df)[-c(1,2,28)], collapse=" + "))
summer.lda78 <- lda(as.formula(paste(f)), data = train78.df)
summer.lda78.predict <- predict(summer.lda78, newdata = test78.df)
#summer.lda78.predict$class
confusionMatrix(table(summer.lda78.predict$class,test78.df$lc))

plot(factor(test78.df$lc),summer.lda78.predict$x) # good to distinguish between class 7 and 8

#diffs78<-abs(summer.lda78$means[1,]/summer.lda78$means[2,])
#diffs78[order(diffs78,decreasing=T)]

# Irrigated trees and crops vs. the rest of the classes
smp_size378 <- floor(0.75 * nrow(dfs378))
train_ind378 <- sample(nrow(dfs378), size = smp_size378)
train378.df <- dfs378[train_ind378, ]
test378.df <- dfs378[-train_ind378, ]
f <- paste("as.factor(train378.df$lc) ~ ", paste(names(train378.df)[-c(1,2,28)], collapse=" + "))
summer.lda378 <- lda(as.formula(paste(f)), data = train378.df) # , na.action=na.exclude
summer.lda378.predict <- predict(summer.lda378, newdata = test378.df)
#summer.lda378.predict$class
confusionMatrix(table(summer.lda378.predict$class,test378.df$lc))

plot(summer.lda378.predict$x[,1],summer.lda378.predict$x[,2],type='n')
text(summer.lda378.predict$x[,1],summer.lda378.predict$x[,2],lab=test378.df$lc,col=test378.df$lc)

plot(factor(test378.df$lc),summer.lda378.predict$x[,1])
plot(factor(test378.df$lc),summer.lda378.predict$x[,2])

# Merged Irrigated trees and crops (2) vs. the rest of the classes (3)

set.seed(13) 

smp_size23 <- floor(0.75 * nrow(dfs23))
train_ind23 <- sample(nrow(dfs23), size = smp_size23)
train23.df <- dfs23[train_ind23, ]
test23.df <- dfs23[-train_ind23, ]
f <- paste("as.factor(train23.df$lc) ~ ", paste(names(train23.df)[-c(1,2,28)], collapse=" + "))
summer.lda23 <- lda(as.formula(paste(f)), data = train23.df) # , na.action=na.exclude
summer.lda23.predict <- predict(summer.lda23, newdata = test23.df)
#summer.lda23.predict$class
confusionMatrix(table(summer.lda23.predict$class,test23.df$lc))

plot(summer.lda23)
plot(factor(test23.df$lc),summer.lda23.predict$x)

summer.mda23 <- mda(as.formula(paste(f)), data = train23.df)
summer.mda23$confusion # correct of class 2: 7416 (Best!)

summer.fda23 <- fda(as.formula(paste(f)), data = train23.df)
summer.fda23$confusion # correct of class 2: 6877

summer.lda23.predict2 <- predict(summer.lda23, newdata = train23.df)
confusionMatrix(table(summer.lda23.predict2$class,train23.df$lc)) # correct of class 2: 6877

ummer.rda23 <- rda(as.formula(paste(f)), data = train23.df)
summer.rda23.predict <- predict(summer.rda23, newdata = test23.df)
confusionMatrix(table(summer.rda23.predict$class,test23.df$lc)) # correct of class 2: 2338

summer.qda23 <- qda(as.formula(paste(f)), data = train23.df) # ERROR

#indna<-apply(train23.df,1,function(x) any(is.na(x)))
#indna<-as.vector(indna)



# Merged Irrigated trees (2) vs. the rest of the classes (3)
smp_size73 <- floor(0.75 * nrow(dfs73))
train_ind73 <- sample(nrow(dfs73), size = smp_size73)
train73.df <- dfs73[train_ind73, ]
test73.df <- dfs73[-train_ind73, ]
f <- paste("as.factor(train73.df$lc) ~ ", paste(names(train73.df)[-c(1,2,28)], collapse=" + "))
summer.lda73 <- lda(as.formula(paste(f)), data = train73.df) # , na.action=na.exclude
summer.lda73.predict <- predict(summer.lda73, newdata = test73.df)
#summer.lda73.predict$class
confusionMatrix(table(summer.lda73.predict$class,test73.df$lc))

plot(factor(test73.df$lc),summer.lda73.predict$x) # thrs lower than -4

# Merged Irrigated crops (2) vs. the rest of the classes (3)
smp_size83 <- floor(0.75 * nrow(dfs83))
train_ind83 <- sample(nrow(dfs83), size = smp_size83)
train83.df <- dfs83[train_ind83, ]
test83.df <- dfs83[-train_ind83, ]
f <- paste("as.factor(train83.df$lc) ~ ", paste(names(train83.df)[-c(1,2,28)], collapse=" + "))
summer.lda83 <- lda(as.formula(paste(f)), data = train83.df) # , na.action=na.exclude
summer.lda83.predict <- predict(summer.lda83, newdata = test83.df)
#summer.lda83.predict$class
confusionMatrix(table(summer.lda83.predict$class,test83.df$lc))

plot(factor(test83.df$lc),summer.lda83.predict$x)


# convert to a funtion to test 

head(summer.lda23.predict$x) - head(lda1) # 5.076223

dfspred<- dfs # test23.df

for(i in 1:dim(coef(summer.lda23))[1]){

	coln<-which(names(dfspred)==rownames(coef(summer.lda23))[i])
	dfspred[,coln]<-dfspred[,coln]*coef(summer.lda23)[i]
	
}

lda1<-NULL

for(j in 1:dim(dfspred)[1]){

	lda1[j]<-sum(dfspred[j,-c(1,2,28)]) + 5.076223#+ 5.016518 #,na.rm=T) # the origin value only works for the 23 summer lda

}

#hist(lda1)
plot(factor(dfs$lc),lda1)
table(dfspred$lc[lda1 < -1.5])

#   2    3    4    5    6    7    8    9   10 
#   2   10   61   24  205 6126 3119  526  180 

#table(dfspred$lc[lda1 < 2.5]) # winter values!

#decr<-order(abs(summer.lda23$scaling),decreasing=T)
#summer.lda23$means[,decr]

#diffs23<-abs(summer.lda23$means[1,]/summer.lda23$means[2,])
#diffs23<-abs(summer.lda23$means[1,]-summer.lda23$means[2,])

diffs23<-NULL
for(i in 1:dim(summer.lda23$means)[2]){
	dv<-summer.lda23$means[,i]
	#if(min(dv)<0){dv+abs(min(dv))}
	x<-(dv[1]-dv[2])/mean(dv)
	#x<-cv(dv)
	diffs23[i]<-abs(x)
	}

diffs23[order(diffs23,decreasing=T)]
summer.lda23$means[,order(diffs23,decreasing=T)] # variables with higher difference first

attach(dfs)

ias<-
##GCVI_max	 &
##GCVI_mean	 &
##GCVI_min	 &
##GCVI_std  &
#MTI  & 
#NDBI_max	 & 
#NDBI_mean	< -0.02253282 &
#NDBI_min	< -0.10494539 &
##NDVI_max	 &
NDVI_mean	> 0.2673560 &
#NDVI_min	...
#NDVI_std   ...
#NDWBI_max  ...
NDWBI_mean < -0.3452431 &
#NDWBI_min  ...
#NDWI_max	> 0.10494539 &
#NDWI_mean  > 0.02253282 &
#NDWI_min   ...
NDWI_std   > 0.05186614
#WGI_max	> 0.228233120 &
#WGI_mean	> 0.07429631 
#WGI_min	> -0.01236259 &
#WGI_std  > 0.08744740   
#blue	    ...
#green		...
#nir		...
#red		...
#slope < 2.172271
##swir1		...

table(dfs$lc[ias]) 


#diffs23[order(diffs23,decreasing=T)]

#diffs23i<-diffs23
#for(i in 1:length(diffs23)){if(diffs23[i]<1){diffs23i[i]<-1/#diffs23[i]}}
#diffs23i[order(diffs23i,decreasing=T)]

# Get the posteriors as a dataframe. # NA ERROR!
#summer.lda.predict.posteriors <- as.data.frame(summer.lda.predict$posterior)
# Evaluate the model
#pred <- prediction(summer.lda.predict.posteriors[,2], test.df$lc) # Error: 'predictions' contains NA.
#roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#auc.train <- performance(pred, measure = "auc")
#auc.train <- auc.train@y.values
# Plot
#plot(roc.perf)
#abline(a=0, b= 1)
#text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


# MDS

ddfs<-dist(dfs[1:50000,-c(1:2,28)])


