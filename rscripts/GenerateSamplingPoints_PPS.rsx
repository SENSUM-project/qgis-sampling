##SENSUM=group
##density_layer=raster
##sampling_points=output vector
##sample_size=number 100
require(raster)
require(rgdal)
require(spatstat)
library(focusmapr)

#convert to vector of indices where fm  != NA
fm_v<-as.vector(density_layer)
fm_i<-which(!is.na(fm_v))

#normalize. If necessary trim to the minimum value 1e-6 to avoid zero-inclusion probabilities
fm_v<-ifelse(fm_v<1e-6,1e-6,fm_v)
fm_v<-fm_v/sum(fm_v[fm_i],na.rm=TRUE)

# get a sample using the focus map as inclusion probability, without replacement
samp_ind<-sample(fm_i,size=sample_size,prob=fm_v[fm_i], replace=FALSE)

#get the coords of the sampled points
out<-SpatialPoints(xyFromCell(density_layer,samp_ind))

#re-set original projection
crs(out)<-crs(density_layer)

# convert to a SatialPointsDataFrame, including the focus map value as attribute
smp_val<-data.frame(fm_v[samp_ind])
names(smp_val)<-'inc_prob'
sampling_points=SpatialPointsDataFrame(out,smp_val)
