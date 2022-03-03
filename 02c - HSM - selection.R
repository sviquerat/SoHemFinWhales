source(file.path(getwd(),'_SRC','SORP_settings.R'))
source(file.path(SCRIPTDIR,'MAXENT_settings.R'))

load(file = MAXENT_MODELS)
load(file = FINAL_COVARS)

MAXENT_FINAL_MODELS<-list()

final_model_data<-data.frame()

#### Quarter A:
quarter<-'A'
selected_model<-'model07'

final_model<-MAXENT_run_single_model(MAXENT_RESULTS,quarter,selected_model)
final_model_data<-rbind(final_model_data, final_model$condensed_results)
plotData<-MAXENT_pool_replicates(final_model$evaluation_list)

png(file.path(MAXENTGFX,paste0('SORP_MAXENT_FINAL_DIAGS_',quarter,'.png')),3600,3600,res=300)
MAXENT_repl_plot(plotData)
graphics.off()

png(file.path(MAXENTGFX,paste0('SORP_MAXENT_FINAL_PREDICTORS_',quarter,'.png')),3600,3600,res=300)
plot(final_model$predictors, col=heat.colors(256))
graphics.off()

MAXENT_FINAL_MODELS[[quarter]]<-final_model

#### Quarter B:
quarter<-'B'
selected_model<-'model18'

final_model<-MAXENT_run_single_model(MAXENT_RESULTS,quarter,selected_model)
final_model_data<-rbind(final_model_data, final_model$condensed_results)
plotData<-MAXENT_pool_replicates(final_model$evaluation_list)

png(file.path(MAXENTGFX,paste0('SORP_MAXENT_FINAL_DIAGS_',quarter,'.png')),3600,3600,res=300)
MAXENT_repl_plot(plotData)
graphics.off()

png(file.path(MAXENTGFX,paste0('SORP_MAXENT_FINAL_PREDICTORS_',quarter,'.png')),3600,3600,res=300)
plot(final_model$predictors, col=heat.colors(256))
graphics.off()

MAXENT_FINAL_MODELS[[quarter]]<-final_model

#### Quarter D:
quarter<-'D'
selected_model<-'model10'

final_model<-MAXENT_run_single_model(MAXENT_RESULTS,quarter,selected_model)
final_model_data<-rbind(final_model_data, final_model$condensed_results)
plotData<-MAXENT_pool_replicates(final_model$evaluation_list)

png(file.path(MAXENTGFX,paste0('SORP_MAXENT_FINAL_DIAGS_',quarter,'.png')),3600,3600,res=300)
MAXENT_repl_plot(plotData)
graphics.off()

png(file.path(MAXENTGFX,paste0('SORP_MAXENT_FINAL_PREDICTORS_',quarter,'.png')),3600,3600,res=300)
plot(final_model$predictors, col=heat.colors(256))
graphics.off()

MAXENT_FINAL_MODELS[[quarter]]<-final_model

#### save stuff
rownames(final_model_data)<-1:nrow(final_model_data)
MAXENT_final_model_results<-final_model_data
save(MAXENT_FINAL_MODELS, MAXENT_final_model_results, file = MAXENT_MODEL_SELECTION, compress='gzip')
openxlsx::write.xlsx(MAXENT_final_model_results,file.path(MAXENTDIR,'SORP_MAXENT_FINAL_MODEL_DIAGS.xlsx'))
