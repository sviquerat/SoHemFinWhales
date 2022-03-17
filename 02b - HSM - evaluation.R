source(file.path(getwd(),'_SRC','SORP_settings.R'))
source(file.path(SCRIPTDIR,'MAXENT_settings.R'))

load(file = MAXENT_MODELS)
load(file = FINAL_COVARS)

#### model evaluation
results<-MAXENT_summary

summary<-subset(results,replicate %in% c('Quartile 25', 'Quartile 75','averaged'))

#combine q25, q50 and q75 into one table
condensed_table<-data.frame()
for (q in unique(summary$quart)){
  for (modelname in unique(summary$modelName[summary$quart==q])){
    df<-subset(summary, quart==q & modelName == modelname)
    combined<-MAXENT_results_merger(df,1:7)
    condensed_table<-rbind(condensed_table, combined)
  }
}
openxlsx::write.xlsx(condensed_table,file.path(MAXENTDIR,'SORP_MAXENT_MODEL_RESULTS_SUMMARY.xlsx'))

df<-subset(results,!(replicate %in% c('Quartile 25', 'Quartile 75','averaged')))
openxlsx::write.xlsx(df,file.path(MAXENTDIR,'SORP_MAXENT_MODEL_RESULTS_REPLICATES.xlsx'))


pooled_data<-data.frame()
for (q in names(MAXENT_RESULTS)){
  print(paste0('Diagnostic plots for Quarter: ',q))
  QUART<-MAXENT_RESULTS[[q]]
  EVAL<-QUART$evaluation
  PREDS<-QUART$predictors
  for (modelName in names(QUART$data)){
    print(modelName)
    e<-EVAL[[modelName]]
    plotData<-MAXENT_pool_replicates(e)
    
    png(file.path(MAXENTMODELGFX,paste0('SORP_MAXENT_DIAGS_EVAL_',q,'_',modelName,'.png')),3600,3600,res=300)
    MAXENT_repl_plot(plotData)
    graphics.off()
    
    plotData<-plotData$pooled
    plotData$quart<-q
    plotData$modelName<-modelName
    pooled_data<-rbind(pooled_data,plotData)
  }
}

for (i in 1:nrow(df)){
  vars<-definitions[[df$modelName[i]]]
  df$fullModelName[i]<-paste0(df$modelName[i], ' - ', paste0(vars,collapse='+'))
}

for (varname in names(results)[8:15]){
  df$Y<-df[[varname]]
  
  png(file.path(MAXENTMODELGFX,paste0('SORP_MAXENT_POOLED_EVAL_',varname,'.png')),5600,3600,res=300)
  p <- ggplot(df, aes(x=modelName, y=Y,fill=fullModelName)) + geom_violin(scale='width',draw_quantiles=c(.25,.75))
  p <- p+facet_wrap(.~quart)
  
  p<-p+theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),text = element_text(size=15))+ylab(varname)
  p<-p+MAINTHEME+theme(legend.position = "right")
  p <- p + theme(legend.title = element_blank(),axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p)
  graphics.off()
}

scree_data<-data.frame()
for (q in unique(df$quart)){
  df_quart<-df[df$quart==q,8:15]
  
  b<-prcomp(df_quart,scale=T)
  
  pc_data<-data.frame(quart=q,fullModelName=df$fullModelName[df$quart==q],modelName = df$modelName[df$quart==q],data.frame(b$x))
  
  p<-ggbiplot::ggbiplot(b,groups=pc_data$fullModelName,ellipse=F,alpha=.3,circle=F,scale=0)
  p<-p+ggforce::geom_mark_ellipse(data=pc_data,aes(x=PC1,y=PC2,label=modelName,colour=fullModelName), 
                                  expand = unit(0.5,'mm'),con.border='all')
  p<-p+facet_wrap(.~quart)
  p<-p+MAINTHEME+theme(legend.position = "right")
  p <- p + theme(legend.title = element_blank())
  
  png(file.path(MAXENTGFX,paste0('SORP_MAXENT_MODEL_PCA_',q,'.png')),5600,3600,res=300)
  print(p)
  graphics.off()
  
  importance<-summary(b)
  scree_data<-rbind(scree_data,data.frame(quarter = q, t(importance$importance[2,])))
}  

scree_data[,2:8]<-round(scree_data[,2:8],4)
scree_data<-scree_data[order(scree_data$quarter),]
openxlsx::write.xlsx(scree_data,file.path(MAXENTDIR,'SORP_MAXENT_MODEL_SCREE.xlsx'))