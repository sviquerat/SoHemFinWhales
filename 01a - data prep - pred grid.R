source('_SRC/SORP_settings.R')
library(sp)

#all covars are in IBCSO projection

SURVEYAREA<-rgdal::readOGR(file.path(MERGEDDIR,'SHAPES','SORP_BOUNDARY_100K_BUFFER.gpkg'))
SURVEYAREA@proj4string<-ANT_POL_STEREO
SURVEYAREA<-sp::spTransform(SURVEYAREA,IBCSO)

#hex cells are equidistant
#form regular grid
#area of a hex:
#d=distance between centroids
#s=side length
#r=radius
#A = 3*sqrt(3)/2*s^2 = 2*sqrt(3)*r^2 = sqrt(3)/2*d^2

predGrid <-sp::spsample(SURVEYAREA,type="hexagonal",cellsize=hex_spacing,offset = c(0, 0))
predGrid <- sp::HexPoints2SpatialPolygons(predGrid)
predGrid <- sp::SpatialPolygonsDataFrame(predGrid, data.frame(CellID=names(predGrid)), match.ID = F)
predGrid@proj4string<-IBCSO

load(COVAR_STACKS) #covars are in IBCSO projection

# now load covars stack and use as covars for grid
splits<-getChunks(1:nrow(predGrid),1000) # split data into chunks of size 1000
for (var in names(covar_static_stack)){
  
  if (var %in% c('DIST2_SGSS','DIST2_WAP_ISLANDS', 'DIST2_MAINLAND')){next}
  
  print(var)
  v<-v_se<-vector(length=nrow(predGrid))
  pb<-txtProgressBar(0, length(splits),style=3)
  for (i in 1:length(splits)){
    setTxtProgressBar(pb,i)
    pos<-splits[[i]]
    pg<-predGrid[pos,]
    r<-covar_static_stack[[var]]
    v[pos]<-raster::extract(r,pg,method='simple',fun = mean,na.rm=T)
    v_se[pos]<-raster::extract(r,pg,method='simple',fun = standardError)
  }
  predGrid[[var]]<-v
  predGrid[[paste0(var,'_se')]]<-v_se
}
close(pb)

save(predGrid,file=PRED_GRID,compress='gzip')

#create quarterly pred_grid geopackages
for (q in names(quarts)){
  print('QUARTER')
  print(q)
  print('Creating quarter pred grids as geopackages')
  
  pg<-predGrid
  env_covars<-covar_env_stack[[q]]
  chla<-raster::extract(env_covars$chla,pg,method='simple',fun=mean,na.rm=T)
  sst<-raster::extract(env_covars$sst,pg,method='simple',fun=mean,na.rm=T)
  pg$chla<-as.numeric(chla)
  pg$sst<-as.numeric(sst)
  rgdal::writeOGR(pg,dsn=file.path(RESSPDIR,paste0('SORP_prediction_grid_QUART_',q,'.gpkg')),
                  layer=paste0('SORP_prediction_grid_',q),driver='GPKG', overwrite_layer = T)
  #save(pg,file=file.path(RESDATDIR,paste0('SORP_prediction_grid_QUART_',q,'.RData')),compress='gzip')
}
