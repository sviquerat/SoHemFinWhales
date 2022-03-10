source('_SRC/SORP_settings.R')
source('_SRC/_FUNCTIONS/snailplots.R')

source(file.path(SCRIPTDIR,'RF_settings.R'))

load(RFGLS_VALIDATION_DATA)

RF_validation$accuracy<-RF_validation$I_obs/RF_validation$I_pred
for (quarter in unique(RF_validation$quart)){
  val<-subset(RF_validation, quart==quarter)
  val$ID<-1:nrow(val)
  y<-val$accuracy
  df<-create_snail_data(val$accuracy)
  area<-polar_area(df$x,df$y)
  
  png(file.path(FINALGFXDIR,paste0('SORP_RFGLS_SNAIL_ACCURACY_QUART_',quarter,'.png')),3000,3000,res=300)
  snailplot(val$accuracy, type='n')
  draw_snail(val$accuracy,type='p')
  draw_snail(val$accuracy,type='l',color_ramp = rainbow)
  legend('topright',legend=c(
    paste0('Area: ',round(area$area_data,2)),
    paste0('Area at radius 1: ',round(area$area_comparison,2)),
    paste0('accuracy: ',100*round(area$ratio,4), '%')
  ))
  title(unique(val$pretty_quart))
  graphics.off()
}

median(val$accuracy)

quantile(val$accuracy)
hist(val$accuracy)
head(val)

plot(val$cell_y~val$cell_x)
