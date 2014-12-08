##SENSUM=group
##density_layer=raster
##sampling_points=output vector
##sample_size=number 100
##consider_NA=boolean

require(raster)
require(rgdal)
require(spatstat)
library(focusmapr)

#convert to vector of indices where fm  != NA
fm_v<-as.vector(density_layer)
# check if NA must be excluded
# if consider_NA is FALSE then the NAs are discarded from the area frame
if (consider_NA) fm_i<-which(!is.na(fm_v)|is.na(fm_v)) else fm_i<-which(!is.na(fm_v))

#normalize. trim to 1e-6 to avoid zero inclusion probability
#fm_v<-ifelse(fm_v<1e-6,1e-6,fm_v)
#fm_v<-fm_v/sum(fm_v[fm_i])

# get a sample, use Simple Random Sampling without replacement
samp_ind<-sample(fm_i,size=sample_size, replace=FALSE)

#get the coords of the sampled points
out<-SpatialPoints(xyFromCell(density_layer,samp_ind))

#re-set original projection
crs(out)<-crs(density_layer)

# convert to a SatialPointsDataFrame, with inclusion probability as attribute
smp_val<-data.frame(fm_v[samp_ind])
names(smp_val)<-'focus_map'
sampling_points=SpatialPointsDataFrame(out,smp_val)
