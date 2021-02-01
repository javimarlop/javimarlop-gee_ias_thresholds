library(MASS)
library(caret)

df96sum <- read.table("sample_data_summer_2009.csv", header = T, sep = ",")
#df09sum <- read.table("sample_data_summer_2009.csv", header = T, sep = ",")
#df01sum <- read.table("sample_data_summer_2001.csv", header = T, sep = ",")

rmv_outl<-function(table){ 
	df<-table
 for(i in 3:32){
	if(i!=28){
		for(j in 1:10){
			x<-quantile(df[df$lc==j,i],c(0.05,0.95))
			xmax<-x[2]
			xmin<-x[1]
			df[df$lc==j & df[,i]>xmax,i]<-NA
			df[!is.na(df[,i]) & df$lc==j & df[,i]<xmin,i]<-NA
	}
	}
 }
 return(df)
}

dfs <- rmv_outl(df96sum)
#s01df <- rmv_outl(df01sum)
#s09df <- rmv_outl(df09sum)
#dfs <- rbind(s96df, s01df, s09df)
dfs23 <- dfs
ind78 <- dfs$lc == 7 #| dfs$lc == 8
dfs23$lc[!ind78] <- 3
dfs23$lc[ind78] <- 2

set.seed(13) 
smp_size23 <- floor(0.75 * nrow(dfs23))
train_ind23 <- sample(nrow(dfs23), size = smp_size23)
train23.df <- dfs23[train_ind23, ]
test23.df <- dfs23[-train_ind23, ]
f <- paste("as.factor(train23.df$lc) ~ ", paste(names(train23.df)[-c(1, 2, 28)], collapse = " + ")) # check columns!
summer.lda23 <- lda(as.formula(paste(f)), data = train23.df)
summer.lda23.predict <- predict(summer.lda23, newdata = test23.df)
print(confusionMatrix(table(summer.lda23.predict$class,test23.df$lc)))

# Identifying the intercept

dfspred<- dfs23
#dfspred<- train23.df
#dfspred<- test23.df 

pred<-predict(summer.lda23, newdata = dfspred)

for(i in 1:dim(coef(summer.lda23))[1]){

	coln<-which(names(dfspred)==rownames(coef(summer.lda23))[i])
	dfspred[,coln]<-dfspred[,coln]*coef(summer.lda23)[i]
	
}

lda1<-NULL

for(j in 1:dim(dfspred)[1]){

	lda1[j]<-sum(dfspred[j,-c(1,2,28)]) 

}

# Extracting the intercept
int0<-pred$x - lda1
ind<-!is.na(int0)
intercept<-unique(int0[ind])[1]

lda1ok<-lda1 + intercept

# if we export that to a csv/json, can we use it later on directly?
print('LDA summer coefficients')
print(coefficients(summer.lda23))
print(paste('The intercept is: ',intercept, sep=''))

dict0<-as.data.frame(cbind(rownames(coefficients(summer.lda23)),as.numeric(coefficients(summer.lda23))))

dict<-rbind(dict0,as.data.frame(rbind(c('intercept',intercept))))

#write.table(dict,'dict_sum.csv',sep=',',col.names=F,row.names=F,quote=F)

# User decides based on plot
# IAs are group 2
plot(summer.lda23)

# Allow to play with the value to test results
#print(table(dfspred$lc[lda1 < -2]))

### establish optimum threshold

id2<-dfspred$lc==2
id3<-dfspred$lc==3

mean2<-as.numeric(quantile(lda1ok[id2],na.rm=T)[3])
mean3<-as.numeric(quantile(lda1ok[id3],na.rm=T)[3])

wm<-which.max(c(mean2,mean3))

init<-mean(c(mean2,mean3))

if(wm==2){end<-max(lda1ok[id3],na.rm=T)}
if(wm==1){end<-min(lda1ok[id3],na.rm=T)}

candidates<-seq(init,end,0.01)

ratios<-NULL
for(n in 1:length(candidates)){
freq<-table(dfspred$lc[lda1ok>=candidates[n]])
ratio<-freq[2]/freq[1]
ratios[n]<-ratio
}

# https://lindeloev.github.io/mcp/articles/packages.html
library(cpm)
chpts<-processStream(na.omit(ratios),cpmType = "Exponential")

wmc<-chpts$changePoints[length(chpts$changePoints)]
#wmc<-which.max(ratios)
#wmc<-which(ratios==ratios[ratios>200][1])

dev.new()
plot(ratios,type='l')
abline(v=wmc,col=2)

opt<-candidates[wmc]
print(opt)
print(ratios[wmc])
print(table(dfspred$lc[lda1ok>=candidates[wmc]]))



