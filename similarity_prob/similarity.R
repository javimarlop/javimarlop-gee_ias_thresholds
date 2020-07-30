library(raster)

mh<-raster('clf_dist.tif')
#mh2<-mh*mh

dist<-getValues(mh)
simil<-1-pchisq(dist,df=6)
#simil2<-1-simil
results<-setValues(mh,simil)
plot(results)

# afterwards the working threshold is 0.75
