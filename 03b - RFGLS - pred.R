source('_SRC/SORP_settings.R')
source(file.path(SCRIPTDIR,'RF_settings.R'))

load(PRED_GRID)
load(SAMPLEGRID)
load(RFGLS_MODELS)
load(COVAR_STACKS)

#surveyarea for masking
SURVEYAREA<-rgdal::readOGR(file.path(MERGEDDIR,'SHAPES','SORP_BOUNDARY_100K_BUFFER.gpkg'))
SURVEYAREA@proj4string<-ANT_POL_STEREO
SURVEYAREA<-sp::spTransform(SURVEYAREA,IBCSO)

data.rf <- sample_grid

RF_validation<-data.frame()
for (q in names(RF_models)){
  print(paste0('Quarter : ',q))  
  
  modelData<-RF_models[[q]]
  
  data<-modelData$data
  covars<-modelData$covars
  model<-modelData$model
  centers<-modelData$centers
  scales<-modelData$scales
  
  p_grid<-raster::stack(covar_static_stack,covar_env_stack[[q]])
  p_grid<-p_grid[[covars]]
  p_raster<-raster::mask(p_grid, SURVEYAREA)
  xyz<-raster::rasterToPoints(p_raster)
  
  for (col in covars){
    idx<-which(colnames(xyz) ==col)
    xyz[,idx]<-scale_manual(xyz[,idx],centers[col],scales[col])
  }  
  xyz<-xyz[complete.cases(xyz),]
  
  p_coords<-xyz[,1:2]
  p_cov<-xyz[,covars]
  
  estimate <- RFGLS_predict_spatial(model, coords.0=p_coords, Xtest=p_cov,h=RFGLS_ncores)
  y<-estimate$prediction
  y_<-inv_range01(y,max(data$I),min(data$I)) #need to convert back to original range 
  
  #validation
  I_obs_scaled<-model$y
  I_obs<-inv_range01(I_obs_scaled,max(data$I),min(data$I))
  
  v_coords<-model$coords
  v_data<-model$X
  validation<-RFGLS_predict_spatial(model, coords.0=v_coords, Xtest=v_data,h=RFGLS_ncores)
  I_pred_scaled<-validation$prediction
  I_pred<-inv_range01(I_pred_scaled,max(data$I),min(data$I))
  
  new_validation<-data.frame(quart=q, data, I_obs_scaled = I_obs_scaled, I_pred_scaled = I_pred_scaled, I_pred = I_pred)
  RF_validation<-rbind(RF_validation,new_validation)
  
  pR<-data.frame(x=xyz[,1],y=xyz[,2],I_Q50=y_)
  sp::coordinates(pR)<- ~ x + y
  r<-raster::raster(p_raster$depth)
  r<-raster::rasterize(pR,r,field='I_Q50')
  raster::crs(r) <- IBCSO
  raster::writeRaster(r,file.path(FINALSPDIR,paste0('SORP_FINAL_RF_I_QUART_',q)),format='GTiff',overwrite = T)
}

#make rasterstack persistent
RF_PREDICTION_RASTER<-list()
for (q in names(RF_models)){
  rasterstack<-list()
  Q50<-raster::raster(file.path(FINALSPDIR,paste0('SORP_FINAL_RF_I_QUART_',q,'.tif')))
  
  rasterstack$RFGLS_I_Q50<-Q50
  rasterstack<-raster::stack(rasterstack)
  RF_PREDICTION_RASTER[[q]]<-rasterstack  
}
RF_validation$pretty_quart<-'Q1'
RF_validation$pretty_quart[RF_validation$quart=='B']<-'Q2'
RF_validation$pretty_quart[RF_validation$quart=='D']<-'Q4'

RF_validation<-sqldf::sqldf('select quart, pretty_quart, cell_x,cell_y,I as I_obs, I_obs_scaled, I_pred, I_pred_scaled, aspect, depth, slope, tpi, DIST2_SHELF, chla, sst from RF_validation')

save(RF_validation,file = RFGLS_VALIDATION_DATA,compress='gzip')
save(RF_PREDICTION_RASTER,file = RFGLS_RASTER_PREDICTION,compress='gzip')