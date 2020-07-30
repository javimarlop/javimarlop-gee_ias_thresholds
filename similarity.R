library(raster)

mh<-raster('clf_dist.tif')
mh2<-mh*mh

dist<-getValues(mh)
simil<-pchisq(dist,df=2)
#simil2<-1-simil
results<-setValues(mh2,simil)
plot(results)


