source('_SRC/SORP_settings.R')
source(file.path(SCRIPTDIR,'RF_settings.R'))

#load(PRED_GRID)
#load(SAMPLEGRID)
load(RFGLS_MODELS)
load(COVAR_STACKS)

#surveyarea for masking
SURVEYAREA<-rgdal::readOGR(file.path(MERGEDDIR,'SHAPES','SORP_BOUNDARY_100K_BUFFER.gpkg'))
SURVEYAREA@proj4string<-ANT_POL_STEREO
SURVEYAREA<-sp::spTransform(SURVEYAREA,IBCSO)

#data.rf <- sample_grid

RF_validation<-data.frame()
for (quarter in names(RF_models)){
  print(paste0('Quarter : ',quarter))  
  
  modelData<-RF_models[[quarter]]
  
  data<-modelData$data
  covars<-modelData$covars
  model<-modelData$model
  centers<-modelData$centers
  scales<-modelData$scales
  
  p_grid<-raster::stack(covar_static_stack,covar_env_stack[[quarter]])
  p_grid<-p_grid[[covars]]
  p_raster<-raster::mask(p_grid, SURVEYAREA)
  xyz_all<-cbind(raster::xyFromCell(p_raster, 1:raster::ncell(p_raster)), raster::values(p_raster)) #this includes all empty cells that were masked
  
  for (col in covars){
    idx<-which(colnames(xyz_all) ==col)
    xyz_all[,idx]<-scale_manual(xyz_all[,idx],centers[col],scales[col])
  }  
  valid_xyz<-complete.cases(xyz_all)
  y_all<-rep(NA,nrow(xyz_all)) #this vector will hold the predictions and is 1:1 with raster cells
  
  xyz<-xyz_all[valid_xyz,] #we don't need NA for prediction
  
  p_coords<-xyz[,1:2]
  p_cov<-xyz[,covars]
  
  estimate <- RFGLS_predict_spatial(model, coords.0=p_coords, Xtest=p_cov,h=RFGLS_ncores, verbose=T)
  estimate_all <- RFGLS_predict(model, Xtest=p_cov,h=RFGLS_ncores, verbose=T)
  
  # this is quasi CI estimation maybe?
  trees<-estimate_all$predicted_matrix #contains all 1000 tree predictions for the validation set
  validation_trees<-data.frame()
  for (r in valid_xyz){
    line<-trees[r,] 
    validation_trees<-rbind(validation_trees,quantile(line))
  }
  summary_names<-c('I_pred_scaled_min','I_pred_scaled_Q25','I_pred_scaled_Q50','I_pred_scaled_Q75','I_pred_scaled_max')
  names(validation_trees)<-summary_names
  
  validation_trees$I_pred_scaled<-estimate_all$predicted
  validation_trees$I_pred<-inv_range01(estimate_all$predicted,max(data$I),min(data$I))

  for (sum_name in summary_names){
    new_name<-gsub('_scaled_','',sum_name)
    validation_trees[[new_name]]<-inv_range01(validation_trees[[sum_name]],max(data$I),min(data$I))
  }
  
  empty<-raster::setValues(p_raster$aspect,NA) #take raster from stack as template
  
  y_all<-raster::getValues(empty)
  y_all[valid_xyz]<-validation_trees$I_pred_Q25
  r<-raster::setValues(empty,y_all) #and assign predicted values
  raster::writeRaster(r,file.path(FINALSPDIR,paste0('SORP_FINAL_RF_I_25_QUART_',quarter)),format='GTiff',overwrite = T)
  
  y_all<-raster::getValues(empty)
  y_all[valid_xyz]<-validation_trees$I_pred_Q75
  r<-raster::setValues(empty,y_all) #and assign predicted values
  raster::writeRaster(r,file.path(FINALSPDIR,paste0('SORP_FINAL_RF_I_75_QUART_',quarter)),format='GTiff',overwrite = T)
  
  y<-estimate$prediction
  y_<-inv_range01(y,max(data$I),min(data$I)) #need to convert back to original range 
  
  y_all[valid_xyz]<-y_ #insert the predicted values at the right position
  r<-raster::setValues(p_raster$aspect,NA) #take raster from stack as template
  r<-raster::setValues(r,y_all) #and assign predicted values
  raster::writeRaster(r,file.path(FINALSPDIR,paste0('SORP_FINAL_RF_I_QUART_',quarter)),format='GTiff',overwrite = T)
  
  # this is quasi validation and could be in 3a
  trees<-model$predicted_matrix #contains all 1000 tree predictions for the validation set
  validation_trees<-data.frame()
  for (r in 1:nrow(trees)){
    line<-trees[r,] 
    validation_trees<-rbind(validation_trees,quantile(line))
  }
  summary_names<-c('I_pred_scaled_min','I_pred_scaled_Q25','I_pred_scaled_Q50','I_pred_scaled_Q75','I_pred_scaled_max')
  names(validation_trees)<-summary_names
  validation_trees$I_obs_scaled<-model$y
  validation_trees$I_obs<-inv_range01(model$y,max(data$I),min(data$I))
  validation_trees$I_pred_scaled<-model$predicted
  validation_trees$I_pred<-inv_range01(model$predicted,max(data$I),min(data$I))
  
  for (sum_name in summary_names){
    new_name<-gsub('_scaled_','_',sum_name)
    validation_trees[[new_name]]<-inv_range01(validation_trees[[sum_name]],max(data$I),min(data$I))
  }
  
  new_validation<-data.frame(quart=quarter, data, validation_trees)#I_obs = I_obs, I_obs_scaled = I_obs_scaled, I_pred_scaled = I_pred_scaled, I_pred = I_pred)
  
  RF_validation<-rbind(RF_validation,new_validation)
}

RF_validation$pretty_quart<-'Q1'
RF_validation$pretty_quart[RF_validation$quart=='B']<-'Q2'
RF_validation$pretty_quart[RF_validation$quart=='D']<-'Q4'
RF_validation<-sqldf::sqldf('select quart, pretty_quart, cell_x,cell_y,I as I_obs, I_obs_scaled, I_pred, I_pred_scaled, aspect, depth, slope, tpi, DIST2_SHELF, chla, sst from RF_validation')

save(RF_validation,file = RFGLS_VALIDATION_DATA,compress='gzip')

#make rasterstack persistent
RF_PREDICTION_RASTER<-list()
for (quarter in names(RF_models)){
  rasterstack<-list()
  Q50<-raster::raster(file.path(FINALSPDIR,paste0('SORP_FINAL_RF_I_QUART_',quarter,'.tif')))
  
  rasterstack$RFGLS_I_Q50<-Q50
  rasterstack<-raster::stack(rasterstack)
  RF_PREDICTION_RASTER[[quarter]]<-rasterstack  
}
save(RF_PREDICTION_RASTER,file = RFGLS_RASTER_PREDICTION,compress='gzip')