source('_SRC/SORP_settings.R')
source(file.path(SCRIPTDIR,'RF_settings.R'))

load(RFGLS_MODELS)
load(COVAR_STACKS)
areas<-rgdal::readOGR(files_TARGETAREAS)
areas_buffer<-rgeos::gBuffer(areas,byid=T, width=1E5)

RF_uncertainty<-data.frame()
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
  p_raster<-raster::mask(p_grid, areas_buffer)
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
  
  estimate <- RFGLS_predict(model, Xtest=p_cov,h=RFGLS_ncores, verbose=T)
  
  empty<-raster::setValues(p_raster$aspect,NA) #take raster from stack as template
  
  # this is quasi uncertainty
  trees<-estimate$predicted_matrix #contains all 1000 tree predictions for the validation set
  uncertainty_trees<-data.frame()
  for (r in 1:nrow(trees)){
    line_scaled<-trees[r,] 
    line_pred<-inv_range01(line_scaled,max(data$I),min(data$I))
    std.dev<-sd(line_pred)
    std.err<-std.dev/sqrt(length(line_pred))
    q<-quantile(line_pred)
    newline<-data.frame(mean(line_pred),t(q),std.dev,std.err)
    uncertainty_trees<-rbind(uncertainty_trees,newline)
  }
  summary_names<-c('I_pred_avg','I_pred_scaled_min','I_pred_scaled_Q25','I_pred_scaled_Q50','I_pred_scaled_Q75','I_pred_scaled_max',
                   'I_pred_scaled_std.dev','I_pred_scaled_std.err')
  names(uncertainty_trees)<-summary_names
  
  new_validation<-data.frame(quart=quarter, xyz, uncertainty_trees)
  RF_uncertainty<-rbind(RF_uncertainty,new_validation)
  
  #### create prediction rasters of mean, q25, q75 and se for sub regions
  stack<-list()
  y<-uncertainty_trees$I_pred_avg
  y_q25<-uncertainty_trees$I_pred_scaled_Q25
  y_q75<-uncertainty_trees$I_pred_scaled_Q75
  y_se<-uncertainty_trees$I_pred_scaled_std.err
  
  y_all[valid_xyz]<-y #insert the predicted values at the right position
  stack$I_pred_scaled_avg<-raster::setValues(empty,y_all) #and assign predicted values
  
  y_all[valid_xyz]<-y_se #insert the predicted values at the right position
  stack$I_pred_scaled_se<-raster::setValues(empty,y_all) #and assign predicted values
  
  y_all[valid_xyz]<-y_q25 #insert the predicted values at the right position
  stack$I_pred_scaled_Q25<-raster::setValues(empty,y_all) #and assign predicted values
  
  y_all[valid_xyz]<-y_q75 #insert the predicted values at the right position
  stack$I_pred_scaled_Q75<-raster::setValues(empty,y_all) #and assign predicted values
  stack<-raster::stack(stack)
  stack<-raster::mask(stack, areas)
  for (val in names(stack)){
    r<-stack[[val]]
    raster::writeRaster(r,file.path(FINAL_SP_AREA_DIR,paste0('SORP_',quarter,'_AREAS_',val)), format ='GTiff', overwrite=T)
  }
}

RF_uncertainty$pretty_quart<-'Q1'
RF_uncertainty$pretty_quart[RF_uncertainty$quart=='B']<-'Q2'
RF_uncertainty$pretty_quart[RF_uncertainty$quart=='D']<-'Q4'

save(RF_uncertainty,file = RFGLS_UNCERTAINTY_DATA,compress='gzip')

#make rasterstack persistent
RF_AREA_PREDICTION_RASTER<-list()
for (quarter in names(RF_models)){
  rasterstack<-list()
  rasters<-list.files(path=FINAL_SP_AREA_DIR,pattern=paste0('SORP_',quarter,'_AREAS_*'),full.names = T)
  for (file in rasters){
    name<-strsplit(basename(file),'\\.')[[1]][1]
    val<-gsub(paste0('SORP_',quarter,'_AREAS_'),'',name)
    rasterstack[[val]]<-raster::raster(file)
  }
  rasterstack<-raster::stack(rasterstack)
  RF_AREA_PREDICTION_RASTER[[quarter]]<-rasterstack  
}
save(RF_AREA_PREDICTION_RASTER,file = RFGLS_RASTER_AREA_PREDICTION,compress='gzip')