source('_SRC/SORP_settings.R')
#library(sp)

#take samples in the survey area and create raster from spataildistance to features etc.
SURVEYAREA<-rgdal::readOGR(file.path(MERGEDDIR,'SHAPES','SORP_BOUNDARY_100K_BUFFER.gpkg'))
SURVEYAREA<-sp::spTransform(SURVEYAREA,ANT_POL_STEREO)
sample_res <-5000
target_res<-c(5000,5000)

coords <- sp::spsample(SURVEYAREA,cellsize=sample_res,type='regular',offset = c(0, 0))
coords <- rgeos::gCentroid(coords,byid=TRUE)
coords<-SpatialPointsDataFrame(coords,data.frame(id=1:length(coords)))
coords$x_cent<-coords@coords[,1]
coords$y_cent<-coords@coords[,2]

distanceSHAPES<-list.files(file.path(STATICSHPDIR),'DIST2_.*\\.gpkg',full.names=T)

# processing in smaller chunks is much faster: chunksize n>4000 gets slow
splits<-getChunks(1:length(coords),3000)
for (f in distanceSHAPES){
  SHP <-rgdal::readOGR(f)
  SHP<-sp::spTransform(SHP,ANT_POL_STEREO)
  name<-basename(f)
  name<-strsplit(name,'\\.')[[1]][1]
  print(name)
  dist<-vector(length=length(coords))
  pb<-txtProgressBar(0, length(splits),style=3)
  for (i in 1:length(splits)){
    setTxtProgressBar(pb,i)
    pos<-splits[[i]]
    distance<-apply(rgeos::gDistance(SHP, coords[pos,],byid=TRUE),1,min)
    dist[pos]<-as.numeric(distance)
  }
  close(pb)
  coords[[name]]<-as.numeric(dist)
}
coords<-coords@data
coordinates(coords)<-~x_cent+y_cent
gridded(coords)<-T
coords<-SpatialPixelsDataFrame(coords@coords,coords@data)
coords@proj4string<-ANT_POL_STEREO

reference_raster<-raster::raster(files_IBCSO[1])
reference_raster<-raster::aggregate(reference_raster,fact=10) #500x500 to 5000x5000

dist_files<-c()
for (var in names(coords)){
  if (var %in% c('x_cent','y_cent','id')){next}
  print('Processing')
  print(var)
  dist_raster<-raster::raster(coords[var])
  r<-raster::resample(dist_raster,reference_raster)
  dist_file<-file.path(RESSPDIR,paste0('SORP_COVARS_',var,'_EPSG9354'))
  raster::writeRaster(r,dist_file,format='GTiff', overwrite=T)
  dist_files<-c(dist_files,dist_file)
}

#create raster stack of static covars
covar_static_stack<-list()
for (f in files_IBCSO){
  value<-strsplit(basename(f),'\\.')[[1]][1]
  value<-strsplit(value,'_')[[1]][2]
  if (value == 'crop'){value<-'depth'} #basic depth file is called _crop, hence need to rename
  r<-raster::raster(f)
  r<-raster::aggregate(r,fac=10)
  agg_file<-file.path(RESSPDIR,paste0('SORP_COVARS_',value,'_EPSG9354'))
  raster::writeRaster(r,agg_file,format='GTiff', overwrite=T)
  covar_static_stack[[value]]<-raster::raster(paste0(agg_file,'.tif'))
}

for (f in dist_files){
  value<-strsplit(basename(f),'\\.')[[1]][1]
  value<-strsplit(value,'_EPSG9354')[[1]]
  value<-strsplit(value,'SORP_COVARS_')[[1]][[2]]
  covar_static_stack[[value]]<-raster::raster(paste0(f,'.tif'))
}
#names(covar_static_stack)<-gsub('crop', 'depth', names(covar_static_stack)) 
covar_static_stack<-raster::stack(covar_static_stack)

#create raster stack of env covars per quarter
covar_env_stack<-list()
for (q in names(quarts)){
  print('QUARTER')
  print(q)
  print('Resampling SST and CHLA raster to IBCSO2 resolution (500x500) and EPSG:9354')
  
  months<-quarts[[q]]
  months<-month.abb[months]
  
  chla_list<-list.files(path=file.path(ENV_CHLA_DIR,months),pattern='*.tif',full.names=T)
  sst_list<-list.files(path=file.path(ENV_SST_DIR,months),pattern='*.tif',full.names=T)
  
  r_chla<-raster::stack(chla_list)
  r_chla<-raster::calc(r_chla, fun=mean,na.rm=T)
  r_chla<-raster::projectRaster(r_chla,crs = ANT_POL_STEREO)
  r_chla<-raster::resample(r_chla,covar_static_stack[[1]])
  chla_file<-file.path(RESSPDIR,paste0('SORP_COVARS_CHLA_EPSG9354_QUART_',q))
  raster::writeRaster(r_chla,chla_file,format='GTiff', overwrite=T)
  
  r_sst<-raster::stack(sst_list)
  r_sst<-raster::calc(r_sst, fun=mean,na.rm=T)
  r_sst<-raster::projectRaster(r_sst,crs = ANT_POL_STEREO)
  r_sst<-raster::resample(r_sst,covar_static_stack[[1]])
  sst_file<-file.path(RESSPDIR,paste0('SORP_COVARS_SST_EPSG9354_QUART_',q))
  raster::writeRaster(r_sst,sst_file,format='GTiff', overwrite=T)
  covar_env_stack[[q]]<-raster::stack(list(sst=raster::raster(paste0(sst_file,'.tif')),chla=raster::raster(paste0(chla_file,'.tif'))))
}

save(covar_env_stack, covar_static_stack,file = COVAR_STACKS,compress='gzip')