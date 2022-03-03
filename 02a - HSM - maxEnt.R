source(file.path(getwd(),'_SRC','SORP_settings.R'))
source(file.path(SCRIPTDIR,'MAXENT_settings.R'))

load(SAMPLEGRID)
load(FINAL_COVARS)
load(COVAR_STACKS)

definitions<-list(
  c("sst"),
  c('chla'),
  c("sst",'chla'),
  c("depth"),
  c('depth','chla'),
  c('depth','sst'),
  c('depth','DIST2_SHELF'),
  c('depth','sst','chla'),
  c("depth",'slope'),
  c("depth",'slope','sst'),
  c("depth",'slope','sst','DIST2_SHELF'),
  c("depth",'slope','chla'),
  c("depth",'slope','chla','DIST2_SHELF'),
  c("depth",'slope','chla','sst'),
  c("aspect","depth","tpi",'chla','sst','DIST2_SHELF'),
  c("aspect","tri",'chla','sst','DIST2_SHELF'),
  c("aspect","tri",'DIST2_SHELF'),
  c("aspect","slope",'DIST2_SHELF'),
  c("aspect","slope"),
  c("slope")
)

names(definitions)<-paste0('model',formatC(1:length(definitions),width=2,flag='0'))

#dump definitions to table in final dir
model_def<-data.frame()
for (name in names(definitions)){
  model_cov<-paste(definitions[[name]],collapse=' + ')
  model_def<-rbind(model_def, data.frame(modelName = name, def = model_cov))
}
openxlsx::write.xlsx(model_def,file.path(FINALDIR,'SORP_MODEL_DEFINITION.xlsx'))

MAXENT_RESULTS<-list()
results<-data.frame()
quart_obs<-data.frame() #number of obs and absences per quarter

print('Running Maxent models')
print(MAXENTargs)

for (q in analysis_quarts){
  #these are lists per quarter containing results for each model definition
  modelData<-list()
  modelEvaluation<-list()
  modelPredictors<-list()
  
  allowed_covars<-analysis_covars[[q]]
  
  print(paste0('Quarter ',q))
  
  ### big loop across all model definitions
  for (idx in 1:length(definitions)){
    def<-definitions[idx][[1]]
    
    #### situations where we want to skip the model run
    if (q %in% c('B','C')){
      if (any(c('chla','sst') %in% def)){ #skip all models in B & D that require sst or chla covars
        print('skipping env covars for sparsely covered quarters (B & D)')
        next
      }
    }
    
    if (!all(def %in% allowed_covars)){ #skip all models that require covars we kicked out in the covar selection for the given quarter
      print(paste0(
        'Covars allowed for this quarter (',paste(allowed_covars,collapse=", "),
        ') do not match requested model covars (',paste(def,collapse=", "),')')
      )
      next
    }
    
    modelName<-names(definitions)[idx]
    
    ### create raster stack of covars used in this model
    predictors<-list()
    pred<-covar_static_stack
    pred$sst<-covar_env_stack[[q]]$sst
    pred$chla<-covar_env_stack[[q]]$chla
    for (cov in def){
      predictors[[cov]]<-pred[[cov]]
    }
    predictors<-raster::stack(predictors)
  
    ### get subset of complete data 
    data<-sample_grid[sample_grid$quart==q,c('I','binom','cell_x','cell_y',def)]
    data<-data[complete.cases(data[,def]),]
    
    pp<-sp::SpatialPoints(cbind(data$cell_x,data$cell_y),IBCSO)
    pp<-sp::SpatialPointsDataFrame(pp,data.frame(binom = data$binom))

    occ<-subset(pp,binom==1)@coords #occurrence points as SP
    abs<-subset(pp,binom==0)@coords #background points as SP
    
    # the final data block that will be needed for the re run
    dataBlock<-list(x=predictors, p=occ, a=abs,args=MAXENTargs)
    
    N_occ = nrow(occ) #number of occurrence records
    N_abs = nrow(abs) #number of pseudo-absence records
    quart_obs<-rbind(quart_obs,data.frame(quart=q, modelName=modelName, N_occ = N_occ, N_abs = N_abs))
    
    ### some printed stuff about the data used in the model
    print('Number of observations:')
    print(N_occ+N_abs)
    print('Occurence / absence records:')
    print(paste0(N_occ,' / ',N_abs))
    print(paste0('1 : ', round(N_abs/N_occ,2), ' ratio of obs : abs'))
    print(paste0(round(N_occ/(N_abs+N_occ)*100,4), ' % of data are obs'))
    print(paste0(modelName,' - ', paste(def,collapse=' + ')))
    
    # using Bootstrap as replicatetype works, crossvalidate fails sometimes
    capture.output(
      model<-dismo::maxent(x=predictors,p=occ,a=abs,args=MAXENTargs,silent=T,na.rm=T)
    )
    
    replicates<-list()
    model_results<-data.frame()
    for (rep in 1:length(model@models)){
      capture.output(
        e <- dismo::evaluate(p = occ, a = abs, x=predictors,model = model@models[[rep]])
      )
      replicates[[rep]]<-e
      newline<-data.frame(quart = q, id=idx, replicate=rep, modelName = modelName, n_presence = e@np, n_absences = e@na, 
                          sensitivity_threshold = MAXENT_sensitivity, cor = e@cor, AUC = e@auc)
      newline<-cbind(newline,threshold(e))
      model_results<-rbind(model_results, newline)
    }
    names(replicates)<-1:length(model@models)
    
    meanline<-model_results[1,1:7]
    meanline$replicate='averaged'
    q25line<-q75line<-meanline
    q25line$replicate<-'Quartile 25'
    q75line$replicate<-'Quartile 75'
    
    q25line<-data.frame(quart = q, id=idx, replicate='Quartile 25', modelName = modelName, n_presence = e@np, n_absences = e@na, 
                        sensitivity_threshold = MAXENT_sensitivity)
    q75line<-data.frame(quart = q, id=idx, replicate='Quartile 75', modelName = modelName, n_presence = e@np, n_absences = e@na, 
                        sensitivity_threshold = MAXENT_sensitivity)
    means<-colMeans(x=model_results[,8:15], na.rm = TRUE)    
    Q25<-apply(X=model_results[,8:15], 2, FUN = q25)
    Q75<-apply(X=model_results[,8:15], 2, FUN = q75)
    
    meanline<-cbind(meanline,t(means))
    q25line<-cbind(q25line,t(Q25))
    q75line<-cbind(q75line,t(Q75))
    model_results<-rbind(model_results,meanline,q25line,q75line)
    results<-rbind(results,model_results)
    
    modelData[[modelName]]<-dataBlock #into models
    modelEvaluation[[modelName]]<-replicates
    modelPredictors[[modelName]]<-predictors
    
    rm(model) # free up RAM
  }
  MAXENT_RESULTS[[q]]<-list(data=modelData, evaluation = modelEvaluation, predictors = modelPredictors)
  rownames(results)<-NULL
}
quart_obs$total<-quart_obs$N_occ+quart_obs$N_abs
quart_obs$p_occ<-quart_obs$N_occ/quart_obs$tota

MAXENT_summary<-results
MAXENT_quarter_summary<-quart_obs

save(MAXENT_RESULTS,MAXENT_summary,definitions,MAXENT_quarter_summary,file = MAXENT_MODELS,compress='gzip')
