library(dismo)
#### functions used during MAXENT modelling ####

MAXENT_eval_plots<-function(evaluation_model){
  par(mfrow=c(3,4))
  plot(evaluation_model, 'ROC')
  plot(evaluation_model, 'TPR') #true positive rate
  plot(evaluation_model, 'TNR') #true negative rate
  plot(evaluation_model, 'FPR') #false positive rate
  plot(evaluation_model, 'FNR') #false negative rate
  plot(evaluation_model, 'ODP') #overall diagnostic power
  plot(evaluation_model, 'CCR') #correct classification rate
  plot(evaluation_model, 'MCR') #missclassification rate
  plot(evaluation_model, 'OR') #odds ratio
  plot(evaluation_model, 'kappa') #Cohen's kappa
  boxplot(evaluation_model)
  density(evaluation_model)
  par(mfrow=c(1,1))
}

MAXENT_pool_replicates<-function(evaluation_model_list){
  pars<-data.frame()
  classification<-data.frame()
  
  vars<-c('t','confusion','prevalence','ODP','CCR','TPR','TNR','FPR','FNR','PPP','NPP','MCR','OR','kappa')
  auc<-0
  for (i in 1:length(evaluation_model_list)){
    e<-evaluation_model_list[[i]]
    values<-data.frame(replicate=1:length(e@t))
    auc<-auc+e@auc
    classification<-rbind(classification,data.frame(p=e@absence,type=0))
    classification<-rbind(classification,data.frame(p=e@presence,type=1))
    for (var in vars){
      values[[var]]<-attr(e,var)
    }
    pars<-rbind(pars, values)
  }
  auc<-round(auc/i,4)
  return(list(pooled = pars, classification = classification, auc = auc))
}

MAXENT_repl_plot<-function(pooled_data){
  pars<-pooled_data$pooled
  classification<-pooled_data$classification
  auc<-pooled_data$auc
  
  par(mfrow=c(3,4))
  MAXENT_bulk_plot(pars,'FPR','TPR',ylab='True positive rate',xlab='False positive rate',ylim=c(0,1),main='')
  title(main=paste0('AUC: ',auc))
  abline(a=0,b=1)
  
  MAXENT_bulk_plot(pars,'t','TPR',ylab='True positive rate',xlab='threshold',ylim=c(0,1))
  MAXENT_bulk_plot(pars,'t','TNR',ylab='True negative rate',xlab='threshold',ylim=c(0,1))
  MAXENT_bulk_plot(pars,'t','FPR',ylab='False positive rate',xlab='threshold',ylim=c(0,1))
  MAXENT_bulk_plot(pars,'t','FNR',ylab='False negative rate',xlab='threshold',ylim=c(0,1))
  MAXENT_bulk_plot(pars,'t','ODP',ylab='Overall diagnostic power',xlab='threshold')
  MAXENT_bulk_plot(pars,'t','CCR',ylab='Correct classification rate',xlab='threshold')
  MAXENT_bulk_plot(pars,'t','MCR',ylab='missclassification rate',xlab='threshold') 
  MAXENT_bulk_plot(pars,'t','OR',ylab="Odds ratio",xlab='threshold')
  MAXENT_bulk_plot(pars,'t','kappa',ylab="Cohen's kappa",xlab='threshold')
  
  x1<-density(classification$p[classification$type==0])
  x2<-density(classification$p[classification$type==1], bw=x1$bw)
  
  plot(x1$y~x1$x,ylim=c(0,max(x1$y,x2$y)),type='l',col='blue',lty=2,xlab='predicted value', ylab=paste0('Density (bandwidth = ',round(x1$bw,4),')'))
  lines(x2$y~x2$x,type='l',col='red',lty=1)
  legend('topleft',legend=c('absence','presence'),col=c('blue','red'),lty=c(2,1))
  
  par(mfrow=c(1,1))
}

MAXENT_bulk_plot<-function(data, xvar,yvar,ma_window=5,ylab=yvar,xlab=xvar,main=NULL,...){
  df<-MAXENT_create_plot_data(data,xvar,yvar, ma_window=ma_window)
  tmax<-round(max(c(df$y,df$x)),4)
  if (is.null(main)){main = paste0(ylab, " - max at: ",tmax)}
  plot(y~x,data=df,col='red',pch=16,cex=.05,ylab=ylab,xlab=xlab,main=main,type='l',...)
  
}

MAXENT_results_merger<-function(data){
  pos<-which(data$y==max(data$y))
  pos2<-which(data$x[pos]==min(data$x[pos]))
  return (data$x[pos[pos2]])
}

MAXENT_create_plot_data<-function(data, xvar,yvar,ma_window=5){
  df<-data.frame(y=data[[yvar]], x = data[[xvar]])
  df<-df[order(df$x),]
  df<-df[complete.cases(df),]
  df<-ma(df,n=ma_window)
  names(df)<-c('y','x')
  return(df)
}

MAXENT_run_single_model<-function(results,quarter, modelName){
  res<-results[[quarter]]
  dataBlock<-res$data[[modelName]]
  obs = SpatialPoints(dataBlock$p,IBCSO)
  abs = SpatialPoints(dataBlock$a,IBCSO)
  predictors<-dataBlock$x
  capture.output(
    model<-dismo::maxent(x=predictors,p=obs,a=abs,args=dataBlock$args,silent=T)
  )
  
  model_results<-data.frame()
  eval_list=list()
  for (rep in 1:length(model@models)){
    capture.output(
      e <- dismo::evaluate(p = obs, a = abs, x=predictors,model = model@models[[rep]])
    )
    eval_list[[rep]]<-e
    newline<-data.frame(quart = quarter, replicate=rep, modelName = modelName, n_presence = e@np, n_absences = e@na, 
                        sensitivity_threshold = MAXENT_sensitivity, cor = e@cor, AUC = e@auc)
    newline<-cbind(newline,threshold(e))
    model_results<-rbind(model_results, newline)
  }
  
  meanline<-model_results[1,1:6]
  meanline$replicate='averaged'
  q25line<-q75line<-meanline
  q25line$replicate<-'Quartile 25'
  q75line$replicate<-'Quartile 75'
  
  q25line<-data.frame(quart = quarter, replicate='Quartile 25', modelName = modelName, n_presence = e@np, n_absences = e@na, 
                      sensitivity_threshold = MAXENT_sensitivity)
  q75line<-data.frame(quart = quarter, replicate='Quartile 75', modelName = modelName, n_presence = e@np, n_absences = e@na, 
                      sensitivity_threshold = MAXENT_sensitivity)
  
  means<-colMeans(x=model_results[,7:14], na.rm = TRUE)    
  Q25<-apply(X=model_results[,7:14], 2, FUN = q25)
  Q75<-apply(X=model_results[,7:14], 2, FUN = q75)
  
  meanline<-cbind(meanline,t(means))
  q25line<-cbind(q25line,t(Q25))
  q75line<-cbind(q75line,t(Q75))
  model_results<-rbind(model_results,meanline,q25line,q75line)
  
  df<-subset(model_results,replicate %in% c('Quartile 25', 'Quartile 75','averaged'))
  combined<-MAXENT_results_merger(df)
  return(list(modelName = modelName, model = model,predictors = dataBlock$x, results=model_results, condensed_results = combined, evaluation_list=eval_list))
}

MAXENT_results_merger<-function(results, static_cols=1:6){
  combined<-results[1,static_cols]
  for (i in (max(static_cols)+1):ncol(results)){
    var<-names(results)[i]
    q25_data<-subset(results,replicate =='Quartile 25')[,i]
    q50_data<-subset(results,replicate =='averaged')[,i]
    q75_data<-subset(results,replicate =='Quartile 75')[,i]
    combined[[var]] <- q50_data
    combined[[paste0(var,'_q25')]]<-q25_data
    combined[[paste0(var,'_q50')]]<-q50_data
    combined[[paste0(var,'_pretty')]]<-paste0(round(q50_data,4),' (',round(q25_data,4),' - ',round(q75_data,4),')')
  }
  return(combined)
}

#source(file.path(SCRIPTFUNCDIR,'separation_plots.R'))
