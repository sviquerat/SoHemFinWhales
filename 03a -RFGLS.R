source('_SRC/SORP_settings.R')
source(file.path(SCRIPTDIR,'RF_settings.R'))

load(SAMPLEGRID)
load(FINAL_COVARS)

data.rf <- sample_grid

analysis_cols<-c("cell_x","cell_y",'sst','chla',"DIST2_SHELF","aspect","depth","slope","tpi")

data.GLS<-data.rf[,c('cell_x','cell_y','I', 'quart',analysis_cols)] # create smaller data frame with only the neccessary columns
data.GLS<-subset(data.GLS, I > 0)

RF_models<-list() # list that will hold all models
for (quarter in analysis_quarts){
  
  data<-subset(data.GLS, quart==quarter)
  covCols<-analysis_covars[[quarter]]
  RFGLS_mtry=ceiling(length(covCols)/2) # consider half of all available covariates for one tree of the forest
  
  data<-data[,c('cell_x','cell_y','I', covCols)]
  data<-data[complete.cases(data[,c('cell_x','cell_y','I', covCols)]),]
  
  print(paste0('Quartile : ',quarter))
  print(paste0('Cells : ',nrow(data)))
  print(paste0('covars : ',paste0(covCols,collapse=', ')))
  print(paste0('RF tries : ',RFGLS_mtry))
  
  # creation of data for RFGLS
  coords<-as.matrix(data[,c('cell_x','cell_y')])
  
  # recommendation from Datta et al. (pers.comm):
  y<-range01(data$I) #can be reversed with: inv_range01(y,max(data$I),min(data$I))
  
  X<-as.matrix(data[,covCols])
  colnames(X)<-covCols
  
  # We're using scaled covariates in the RFGLS model - however, when predicting, we need to know the full range of covariates 
  # from the pred grid and scale each quarterly subset using the centers and scales of the full range of possible values.
  for (col in colnames(X)){
    idx<-which(colnames(X)==col)
    X[,idx]<-scale_manual(X[,idx],pred_centers[[quarter]][col],pred_scales[[quarter]][col])
  }
  # run the model
  model <- RFGLS_estimate_spatial(coords, y, X, mtry=RFGLS_mtry, h=RFGLS_ncores, n_omp=RFGLS_n_omp, cov.model = "matern",
                                  ntree=RFGLS_ntrees, n.neighbors=RFGLS_n.neighbors, nrnodes=RFGLS_nrnodes,
                                  search.type = RFGLS_search.type, param_estimate = TRUE,verbose=T)
  RF_models[[quarter]]<-list(model=model, data=data, centers = pred_centers[[quarter]], scales = pred_scales[[quarter]], covars=covCols)
}
save(RF_models,data.GLS,scale_manual,file = RFGLS_MODELS,compress='gzip')