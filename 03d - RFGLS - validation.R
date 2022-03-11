source('_SRC/SORP_settings.R')
source('_SRC/_FUNCTIONS/snailplots.R')

source(file.path(SCRIPTDIR,'RF_settings.R'))

load(RFGLS_VALIDATION_DATA)
RF_validation$accuracy<-RF_validation$I_obs/RF_validation$I_pred
RF_DIAGNOSTICS<-data.frame()
for (quarter in unique(RF_validation$quart)){
  val<-subset(RF_validation, quart==quarter)
  val$ID<-1:nrow(val)
  y<-val$accuracy
  df<-create_snail_data(val$accuracy)
  area<-polar_area(df$x,df$y)
  
  png(file.path(FINALGFXDIR,paste0('SORP_RFGLS_SNAIL_ACCURACY_QUART_',quarter,'.png')),6000,3000,res=300)
  par(mfrow=c(1,2))
  snailplot(val$accuracy, type='n', grid_radii = seq(0,max(val$accuracy),.5))
  draw_snail(val$accuracy,type='p')
  draw_snail(val$accuracy,type='areas',color_ramp = rainbow)
  draw_snail(val$accuracy,type='o')
  draw_circle(border='red',lwd=2)
  
  legend('topright',legend=c(
    paste0('Area: ',round(area$area_data,2)),
    paste0('Area at radius 1: ',round(area$area_comparison,2)),
    paste0('accuracy: ',100*round(area$ratio,4), '%')
  ))
  title(paste0('Measure of RF-GLS predictive capability for ',unique(val$pretty_quart)))
  hist(val$accuracy,breaks=seq(0,max(val$accuracy),max(val$accuracy)/50),
       xlab='',main = paste0('Histogram of RF-GLS predictive capability for ',unique(val$pretty_quart)))
  graphics.off()
  
  qframe<-data.frame(t(quantile(val$accuracy)))
  names(qframe)<-c('accuracy_min','accuracy_q05','accuracy_q50','accuracy_q95','accuracy_max')
  newline<-data.frame(quarter=pub_quarter_names[[quarter]],N_obs = nrow(val), I_sum_obs=sum(val$I_obs), 
                      I_sum_pred = sum(val$I_pred),I_sum_pred_round = sum(round(val$I_pred,0)),qframe)
  RF_DIAGNOSTICS<-rbind(RF_DIAGNOSTICS,newline)
}

RF_DIAGNOSTICS$gs_obs<-RF_DIAGNOSTICS$I_sum_obs/RF_DIAGNOSTICS$N_obs
RF_DIAGNOSTICS$gs_pred<-RF_DIAGNOSTICS$I_sum_pred/RF_DIAGNOSTICS$N_obs
openxlsx::write.xlsx(RF_DIAGNOSTICS,file=file.path(FINALDIR,'SORP_RFGLS_VALIDATION.xlsx'))