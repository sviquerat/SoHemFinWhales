source('_SRC/SORP_settings.R')
library(sp)

load(PRED_GRID)
load(SAMPLEGRID)
load(COVAR_STACKS) #covars are in IBCSO projection

static_covCols<-c("aspect","depth","slope","tpi","DIST2_SHELF")
env_covCols<-c('chla','sst')

analysis_cols<-c(static_covCols,env_covCols)

morans<-list() #autocorrelation
correlation_matrices<-list() #spearman's rho
cluster<-list() #cluster objects
sample_grid$quarter<-get_pub_quarter_names(sample_grid$quart)

for (q in unique(sample_grid$quart)){
  df <- sample_grid[sample_grid$quart==q,]
  if(nrow(df)<20){next}
  
  #### Moran's I for spatial autocorrelation
  ## tests for the null hypothesis that there is zero spatial autocorrelation present in the variable at alpha = .05
  data.dists <- as.matrix(dist(cbind(df$cell_x, df$cell_y)))
  data.dists.inv <- 1/data.dists
  diag(data.dists.inv) <- 0
  data.dists.bin <- (data.dists > 0 & data.dists <= hex_spacing) #are two points adjacent or not?
  mor<-data.frame()
  for (var in analysis_cols){
    res <-ape::Moran.I(df[[var]], data.dists.inv,na.rm=T)
    res_binary <-ape::Moran.I(df[[var]], data.dists.bin,na.rm=T)
    newline<-data.frame(quarter=q, type='inverse euclidean',var=var,observed=res$observed, expected = res$expected,sd=res$sd, p_value=res$p.value, autocorrelation = F)
    if (res$p.value<.05){newline$autocorrelation = T}
    newline_b<-data.frame(quarter=q, type='binary',var=var,observed=res_binary$observed, expected = res_binary$expected,sd=res_binary$sd, p_value=res_binary$p.value, autocorrelation = F)
    if (res_binary$p.value<.05){newline_b$autocorrelation = T}
    mor<-rbind(mor,newline,newline_b)
  }
  morans[[q]]<-mor
  
  #plain spearman & spearman^2 rho correlations
  cormat<-cor(df[,analysis_cols],use='complete.obs',method='spearman')
  correlation_matrices[[q]]<-cormat
  
  v <- as.formula(paste("~ ", paste(analysis_cols, collapse="+")))
  tree<-Hmisc::varclus(v, data=df)
  x <- tree$hclust  ## retrieve hclust object
  groups <- cutree(x, h=.5) #all vars in unique groups
  cluster[[q]]<-list(model_tree=tree, groups = groups)
}

covar_check<-list(cluster=cluster,morans=morans,correlation_matrices=correlation_matrices)
analysis_covars<-list() #list containing all covars available for respective quarter

png(file.path(EXP_GFX_DIR,'SORP_pairs.png'),5000,5000,res=300)
p<-GGally::ggpairs(sample_grid[,c('quarter',analysis_cols)], aes(colour = quarter, alpha = 0.2))
p<-p+MAINTHEME
print(p)
graphics.off()

for (q in unique(sample_grid$quart)){
  df <- sample_grid[sample_grid$quart==q,]
  tree<-cluster[[q]]$model_tree
  groups<-cluster[[q]]$groups
  moran<-morans[[q]]
  
  if (is.null(moran)) {next}
  
  cormat<-correlation_matrices[[q]]
  moran_vars<-unique(moran[!moran$autocorrelation,]$var)
  ind_groups<-names(groups)[!duplicated(groups)]
  
  print(paste0('For Quarter ',q,':'))
  
  if (length(moran_vars)>0){
    print('potential autocorrelation:')
    print(moran_vars)
    
    png(file.path(EXP_GFX_DIR,paste0(q,'_SORP_moran_pvalue.png')),3600,3600,res=300)
    p<-ggplot(data=moran[!moran$autocorrelation,],aes(x=as.factor(var),y=p_value, fill=type))
    p<-p+geom_bar(stat='identity',position='dodge')+MAINTHEME
    p<-p+xlab('')+ylab('p value of null hypothesis')
    p<-p+labs(fill = '')
    print(p)
    graphics.off()
  }
  
  print('identified vars:')
  print(ind_groups)
  analysis_covars[[q]]<-ind_groups
  
  png(file.path(EXP_GFX_DIR,paste0(q,'_SORP_pairs_plot.png')), 5000,5000,res=300)
  p<-GGally::ggpairs(df[,c('quarter',analysis_cols)], aes(colour = quarter, alpha = 0.4))
  p<-p+MAINTHEME
  print(p)
  graphics.off()
  
  png(file.path(EXP_GFX_DIR,paste0(q,'_SORP_moran_pvalue_all.png')),3600,3600,res=300)
  p<-ggplot(data=moran,aes(x=as.factor(var),y=p_value, fill=type))
  p<-p+geom_bar(stat='identity',position='dodge')+MAINTHEME
  p<-p+xlab('')+ylab('p value of null hypothesis')
  p<-p+labs(fill = '')
  print(p)
  graphics.off()
  
  png(file.path(EXP_GFX_DIR,paste0(q,'_SORP_cluster_similarity.png')),3600,3600,res=300)
  plot(tree)
  abline(h=.5,col='red')
  graphics.off()
  
  png(file.path(EXP_GFX_DIR,paste0(q,'_SORP_spearman_rho.png')),4000,4000,res=300)
  p<-ggHeatmap(cormat,legend_name = expression(paste(plain('Spearman '),rho)),lower_limit=-.5,upper_limit = .5)
  print(p)
  graphics.off()
  
  png(file.path(EXP_GFX_DIR,paste0(q,'_SORP_spearman_rho_sq.png')),4000,4000,res=300)
  p<-ggHeatmap(cormat^2,legend_name = expression(paste(plain('Spearman '),rho^2)),lower_limit=0,upper_limit = 1,force_symmetry = F)
  print(p)
  graphics.off()
  
  cormat<-cor(df[,ind_groups],use='complete.obs',method='spearman')
  
  png(file.path(EXP_GFX_DIR,paste0(q,'_selected_SORP_spearman_rho.png')),4000,4000,res=300)
  p<-ggHeatmap(cormat,legend_name = expression(paste(plain('Spearman '),rho)),lower_limit=-.5,upper_limit = .5)
  print(p)
  graphics.off()
  
  png(file.path(EXP_GFX_DIR,paste0(q,'_selected_SORP_spearman_rho_sq.png')),4000,4000,res=300)
  p<-ggHeatmap(cormat^2,legend_name = expression(paste(plain('Spearman '),rho^2)),lower_limit=0,upper_limit = 1,force_symmetry = F)
  print(p)
  graphics.off()
}

analysis_quarts<-names(analysis_covars)

# p_cov<-predGrid@data[,static_covCols]
# p_cov_scaled<-scale(p_cov,center=T,scale=T)
# static_centers<-attr(p_cov_scaled,"scaled:center")
# static_scales<-attr(p_cov_scaled,"scaled:scale")

#get centers of predgrid vars 
pred_scales<-list()
pred_centers<-list()
for (q in analysis_quarts){
  pg<-rgdal::readOGR(dsn=file.path(RESSPDIR,paste0('SORP_prediction_grid_QUART_',q,'.gpkg')))
  covars<-names(pg)[!names(pg) %in% 'CellID']
  p_cov<-pg@data[,env_covars]
  p_cov_scaled<-scale(p_cov,center=T,scale=T)
  pred_centers[[q]]<-attr(p_cov_scaled,"scaled:center")
  pred_scales[[q]]<-attr(p_cov_scaled,"scaled:scale")
}

save(analysis_quarts,analysis_covars,covar_check,static_covCols,env_covCols,pred_scales,pred_centers,
     file=FINAL_COVARS,compress='gzip')
#analysis_covars as table