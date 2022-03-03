source('_SRC/SORP_settings.R')

load(COVAR_STACKS)
load(FINAL_COVARS)
load(RFGLS_VALIDATION_DATA)
load(RFGLS_RASTER_PREDICTION)
load(MAXENT_RASTER_PREDICTION)

areas<-rgdal::readOGR(files_TARGETAREAS)

results<-data.frame()
for (q in analysis_quarts){
  adj_I<-RF_PREDICTION_RASTER[[q]]$RFGLS_I_Q50*MAXENT_PREDICTION_RASTER[[q]]$HSM_thd_50 #combine threshold mask and rfgls abundance prediction
  adj_D<- adj_I / (prod(raster::res(adj_I)) / 1E6) #density per pixel - one pixel is 5000 x 5000 m
  raster::writeRaster(adj_I,file.path(FINALSPDIR,paste0('SORP_adj_N_',q)), format ='GTiff', overwrite=T)
  raster::writeRaster(adj_D,file.path(FINALSPDIR,paste0('SORP_adj_D_',q)), format ='GTiff', overwrite=T)
  
  stack<-list(RF_PREDICTION_RASTER[[q]],MAXENT_PREDICTION_RASTER[[q]],covar_static_stack[[static_covCols]],covar_env_stack[[q]], N_adj=adj_I, D_adj = adj_D)
  stack<-raster::stack(stack)
  xyz<-raster::rasterToPoints(stack)
  results<-rbind(results,data.frame(area='Study Area',quarter = q, xyz))
  
  area_list<-list()
  for (aoi in unique(areas$areaname)){
    area_stack<-raster::crop(stack, subset(areas,areaname==aoi))
    
    png(file.path(FINALAREADIR,paste0('SORP_',q,'_',aoi,'_predictions.png')),3600,3600,res=300)
    plot(area_stack)
    graphics.off()
    
    xyz<-raster::rasterToPoints(area_stack)
    results<-rbind(results,data.frame(area=aoi, quarter = q, xyz))
    export_vars<-c("RFGLS_I_Q50","HSM_Q50","HSM_Q50_sd","HSM_Q25","HSM_Q75","HSM_thd_50","HSM_thd_sd",'D_adj','N_adj')
    for (var in export_vars){
      r<-area_stack[[var]]
      raster::writeRaster(r,file.path(FINAL_SP_AREA_DIR,paste0('SORP_',q,'_',aoi,'_',var)), format ='GTiff', overwrite=T)
    }
  }
}

results$pretty_quart<-'Q1'
results$pretty_quart[results$quart=='B']<-'Q2'
results$pretty_quart[results$quart=='D']<-'Q4'

survey_area_results<-subset(results,area=='Study Area')
area_results<-subset(results,area != 'Study Area')

summary_areas<-data.frame()
for (aoi in unique(area_results$area)){
  for (q in unique(area_results$quarter)){
    df<-subset(area_results, area==aoi & quarter==q)
    D<-mean(df$D_adj,na.rm=T)
    D_5<-quantile(df$D_adj,.05,na.rm=T)
    D_95<-quantile(df$D_adj,.75,na.rm=T)
    
    N<-D*areas$AREA_km2[areas$areaname==aoi]
    N_5<-D_5*areas$AREA_km2[areas$areaname==aoi]
    N_95<-D_95*areas$AREA_km2[areas$areaname==aoi]
    
    newline<-data.frame(area=aoi, quarter=q, pretty_quart=unique(df$pretty_quart), N = round(N,0), N_5 = round(N_5,0), N_95 = round(N_95,0), 
                        D = round(D,4), D_5 = round(D_5,4), D_95 = round(D_95,4))
    summary_areas<-rbind(summary_areas,newline)
  }
}
openxlsx::write.xlsx(summary_areas,file=file.path(FINALDIR,'SORP_AREA_ESTIMATES.xlsx'))

save(summary_areas,survey_area_results, area_results, file = FINAL_PREDICTION_RESULTS)