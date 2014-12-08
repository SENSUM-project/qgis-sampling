##SENSUM=group
##density_layer=raster
##sampling_points=output vector
##quantized_focus_map=output raster
##sample_size_per_stratum=number 10
##num_quantiles=number 5

require(raster)
require(rgdal)
require(spatstat)
library(focusmapr)

# if is a rasterbrick take first layer
if (class(density_layer)=='RasterBrick') density_layer<-subset(density_layer, 1)

# normalization
fm_v<-as.vector(density_layer)
fm_v<-ifelse(fm_v<1e-6,1e-6,fm_v) # trim in order not to have zero inclusion probability
nval<-sum(fm_v,na.rm=TRUE)
density_layer<-calc(density_layer,fun=function(x) x/nval)

# compute the quantiles
dq<-1/num_quantiles
fm_q<-quantile(as.vector(density_layer[!is.na(density_layer)]),probs=seq(from=0,by=dq,length.out=num_quantiles+1))

#compute factors (values range from 1 to nquant)
fm_fact<-cut(density_layer, breaks=fm_q)

ss<-sample_size_per_stratum
sample_stratum<-function(x)
{
ind_st<-which(as.vector(fm_fact)== x)
# get a sample
samp_i_st<-sample(ind_st,size=ss,replace=FALSE) # ,prob=fm_v[fm_i])
samp_i_st
}

res<-unlist(sapply(1:num_quantiles,FUN=sample_stratum))

#get the coords of the sampled points
out<-SpatialPoints(xyFromCell(fm_fact,res))

#re-set original projection
crs(out)<-crs(density_layer)

# generate sample with stratum and inclusion probability for each sample point
df<-data.frame(fm_fact[res],density_layer[res])
names(df)<-c('stratum','inc_prob')

sampling_points<-SpatialPointsDataFrame(out,df)
quantized_focus_map<-fm_fact

