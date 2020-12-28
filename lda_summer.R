library(MASS)
library(caret)

df96sum <- read.table("sample_data_summer.csv", header = T, sep = ",")
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
dfspred<- test23.df 

for(i in 1:dim(coef(summer.lda23))[1]){

	coln<-which(names(dfspred)==rownames(coef(summer.lda23))[i])
	dfspred[,coln]<-dfspred[,coln]*coef(summer.lda23)[i]
	
}

lda1<-NULL

for(j in 1:dim(dfspred)[1]){

	lda1[j]<-sum(dfspred[j,-c(1,2,28)]) 

}

# Extracting the intercept
int0<-summer.lda23.predict$x - lda1
ind<-!is.na(int0)
intercept<-unique(int0[ind])[1]

# if we export that to a csv/json, can we use it later on directly?
print('LDA summer coefficients')
print(coefficients(summer.lda23))
print(paste('The intercept is: ',intercept, sep=''))

dict0<-as.data.frame(cbind(rownames(coefficients(summer.lda23)),as.numeric(coefficients(summer.lda23))))

dict<-rbind(dict0,as.data.frame(rbind(c('intercept',intercept))))

write.table(dict,'dict_sum.csv',sep=',',col.names=F,row.names=F,quote=F)

# User decides based on plot
# IAs are group 2
plot(summer.lda23)

# Allow to play with the value to test results
print(table(dfspred$lc[lda1 < -2]))

