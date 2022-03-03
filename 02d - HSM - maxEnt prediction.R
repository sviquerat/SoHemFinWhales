source('_SRC/SORP_settings.R')
source(file.path(SCRIPTDIR,'MAXENT_settings.R'))

load(MAXENT_MODEL_SELECTION)
SURVEYAREA<-rgdal::readOGR(file.path(MERGEDDIR,'SHAPES','SORP_BOUNDARY_100K_BUFFER.gpkg'))
SURVEYAREA@proj4string<-ANT_POL_STEREO
SURVEYAREA<-sp::spTransform(SURVEYAREA,IBCSO)

#model prediction
#https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md
#Liu, C., M. White & G. Newell, 2011. Measuring and comparing the accuracy of species distribution models with presence-absence data. Ecography 34: 232-243.

hard_max<-function(x){
  x <- x[!is.na(x)]
  x<-as.numeric(x)
  tb<-table(x)
  succ<-tb['1']
  fail<-tb['0']
  return (tb['1']>tb['0'] )
}

thd_sd<-function(x){
  x <- x[!is.na(x)]
  n <-length(x) #number of trials
  p <- sum(x[x==1]) / n #probability of successes
  sd <- sqrt(n*p*(1-p))
  return (sd)
}

for (q in names(MAXENT_FINAL_MODELS)){
  selection <- MAXENT_FINAL_MODELS[[q]]
  modelName<-selection$modelName
  model<-selection$model
  predictors<-selection$predictors
  evaluations<-selection$evaluation_list
  
  pred_stack<-list()
  threshold_stack<-list()
  for (i in 1:length(model@models)){
    m<-model@models[[i]]
    ped <- predict(m, predictors)
    pred_stack[[i]]<-ped
    thd <- dismo::threshold(evaluations[[i]], "spec_sens")  #  the threshold at which the sum of the sensitivity (true positive rate) and specificity (true negative rate) is highest
    threshold_stack[[i]]<- ped >= thd
  }
  
  pred_stack<-raster::stack(pred_stack)
  threshold_stack<-raster::stack(threshold_stack)
  
  Q50<-raster::calc(pred_stack, fun=median,na.rm=T)
  Q75<-raster::calc(pred_stack, fun=q75)
  Q25<-raster::calc(pred_stack, fun=q25)
  SD<-raster::calc(pred_stack, fun=sd)
  
  thd_50<-raster::calc(threshold_stack,fun = hard_max)
  thd_SD<-raster::calc(threshold_stack, fun = thd_sd)
  
  Q50_masked<-Q50*thd_50 #mask 0 probability of occurrences
  
  #crop to survey area and save
  raster::writeRaster(raster::mask(Q50, SURVEYAREA), file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_Q50.tif')), driver='GTiff', overwrite=T)
  raster::writeRaster(raster::mask(Q25,SURVEYAREA), file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_Q25.tif')), driver='GTiff', overwrite=T)
  raster::writeRaster(raster::mask(Q75, SURVEYAREA), file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_Q75.tif')), driver='GTiff', overwrite=T)
  raster::writeRaster(raster::mask(SD, SURVEYAREA), file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_SD.tif')), driver='GTiff', overwrite=T)
  raster::writeRaster(raster::mask(thd_50, SURVEYAREA), file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_threshold_Q50.tif')), driver='GTiff', overwrite=T)
  raster::writeRaster(raster::mask(thd_SD, SURVEYAREA), file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_threshold_SD.tif')), driver='GTiff', overwrite=T)
}

#make raster persistent
MAXENT_PREDICTION_RASTER<-list()
for (q in c('A','B','D')){
  rasterstack<-list()
  rasterstack$HSM_Q50<-raster::raster(file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_Q50.tif')))
  rasterstack$HSM_Q50_sd<-raster::raster(file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_SD.tif')))
  rasterstack$HSM_Q25<-raster::raster(file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_Q25.tif')))
  rasterstack$HSM_Q75<-raster::raster(file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_Q75.tif')))
  rasterstack$HSM_thd_50<-raster::raster(file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_threshold_Q50.tif')))
  rasterstack$HSM_thd_sd<-raster::raster(file.path(FINALSPDIR,paste0('SORP_HSM_',q,'_threshold_SD.tif')))
  rasterstack<-raster::stack(rasterstack)
  MAXENT_PREDICTION_RASTER[[q]]<-rasterstack
}
save(MAXENT_PREDICTION_RASTER,file=MAXENT_RASTER_PREDICTION, compress='gzip')

