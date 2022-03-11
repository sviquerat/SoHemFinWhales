source('_SRC/SORP_settings.R')

load(COVAR_STACKS)
load(FINAL_COVARS)
load(RFGLS_VALIDATION_DATA)
load(RFGLS_RASTER_AREA_PREDICTION)
load(MAXENT_RASTER_PREDICTION)

areas<-rgdal::readOGR(files_TARGETAREAS)

results<-data.frame()
for (quarter in analysis_quarts){
  stack_rf<-RF_AREA_PREDICTION_RASTER[[quarter]]
  stack_mx<-MAXENT_PREDICTION_RASTER[[quarter]]
  for (aoi in unique(areas$areaname)){
    area_stack<-raster::mask(stack_rf, subset(areas,areaname==aoi))
    area_stack_mx<-raster::mask(stack_mx, subset(areas,areaname==aoi))
    area_stack<-raster::stack(area_stack,area_stack_mx)
    area_stack<-raster::crop(area_stack, subset(areas,areaname==aoi))
    area_stack$N_adj<-area_stack$I_pred_scaled_avg*area_stack$HSM_thd_50
    area_stack$N_adj_se<-area_stack$I_pred_scaled_se*area_stack$HSM_thd_50
    area_stack$D_adj<-area_stack$N_adj/25
    area_stack$D_adj_se<-area_stack$N_adj_se/25
    
    # identify cells with highest 10 % of group size predictions
    r<-area_stack$N_adj
    limit<-.9
    threshold<-quantile(raster::getValues(r),limit,na.rm=T)
    r[r<threshold]<-0
    r[r>=threshold]<-1
    area_stack$N_highest_10<-r
    
    png(file.path(FINALAREADIR,paste0('SORP_',quarter,'_',aoi,'_predictions.png')),3600,3600,res=300)
    plot(area_stack)
    graphics.off()
    
    xyz<-raster::rasterToPoints(area_stack)
    results<-rbind(results,data.frame(area=aoi, quarter = quarter, xyz))

    export_vars<-c("I_pred_scaled_avg","I_pred_scaled_se",'D_adj','N_adj','D_adj_se','N_adj_se','N_highest_10',
                   "HSM_Q50","HSM_Q50_sd","HSM_Q25","HSM_Q75","HSM_thd_50","HSM_thd_sd")
    for (var in export_vars){
      r<-area_stack[[var]]
      raster::writeRaster(r,file.path(FINAL_SP_AREA_DIR,paste0('SORP_',quarter,'_',aoi,'_',var)), format ='GTiff', overwrite=T)
    }
  }
}

results$pretty_quart<-'Q1'
results$pretty_quart[results$quart=='B']<-'Q2'
results$pretty_quart[results$quart=='D']<-'Q4'

summary_areas<-data.frame()
for (aoi in unique(results$area)){
  for (q in unique(results$quarter)){
    df<-subset(results, area==aoi & quarter==q)
    df<-subset(df, !is.na(df$D_adj))
    densities<-df$D_adj
    
    D<-mean(densities)
    D_se<-mean(df$D_adj_se)
    D_5<-D-1.95*D_se
    if(D_5<0){D_5<-0}
    D_95<-D+1.95*D_se
    
    geo_area<-areas$AREA_km2[areas$areaname==aoi]
    N<-D*geo_area
    N_5<-D_5*geo_area
    N_95<-D_95*geo_area
    
    newline<-data.frame(area=aoi, geo_area = geo_area, quarter=q, pretty_quart=unique(df$pretty_quart), N = round(N,0), N_5 = round(N_5,0), 
                        N_95 = round(N_95,0), D = round(D,4), D_5 = round(D_5,4), D_95 = round(D_95,4))
    summary_areas<-rbind(summary_areas,newline)
  }
}

pretty_summary<-data.frame(name=summary_areas$area)
pretty_summary$geo_area<-round(summary_areas$geo_area,0)
pretty_summary$quart<-summary_areas$pretty_quart
pretty_summary$N<-paste0(summary_areas$N, '\n(',summary_areas$N_5,' - ', summary_areas$N_95, ')')
pretty_summary$D<-paste0(summary_areas$D, '\n(',summary_areas$D_5,' - ', summary_areas$D_95, ')')
pretty_summary<-pretty_summary[order(pretty_summary$name,pretty_summary$quart),]
openxlsx::write.xlsx(pretty_summary,file=file.path(FINALDIR,'SORP_AREA_ESTIMATES.xlsx'))

save(summary_areas,results, file = FINAL_PREDICTION_RESULTS)
