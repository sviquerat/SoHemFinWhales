modeldiag<-function(fitted,observed,...){
  roc <- pROC::roc(observed,fitted,smoothed = TRUE,ci=TRUE, ci.alpha=0.9, stratified=FALSE,plot=F, 
                   auc.polygon=TRUE, max.auc.polygon=T, grid=TRUE,print.auc=TRUE, show.thres=T)
  
  auc<-as.numeric(roc$auc)
  newLine<-data.frame(ROC_AUC = auc,AUC_LO = roc$ci[1],AUC_HI = roc$ci[3])
  return(newLine)
  }

separationplot<-function(fitted,observed,...){
  roc <- pROC::roc(observed,fitted,smoothed = TRUE,ci=TRUE, ci.alpha=0.9, stratified=FALSE,plot=F, 
                   auc.polygon=TRUE, max.auc.polygon=T, grid=TRUE,print.auc=TRUE, show.thres=T)
  
  auc<-as.numeric(roc$auc)
  newLine<-data.frame(ROC_AUC = auc,AUC_LO = roc$ci[1],AUC_HI = roc$ci[3])
  AUC<-paste0('AUC:',round(roc$auc,3),' (95% CI: ',round(roc$ci[1],3), ' - ', round(roc$ci[3],3), ')' )
  
  col0<-'#D3D3D3'
  col1<-'#424242'
  col3<-'#9CA8B3'
  linecol<-'white'
  
  layout( matrix(c(1,3,2,3),nrow=2) )
  
  plot(c(0,1),c(1,0),type='n',xlim=c(1,0),ylim=c(0,1),asp=1,ann=F,axes=F)
  grid()
  polygon(x=c(0,0,1,1),y=c(0,1,1,0),col=col0)
  polygon(y=c(roc$sensitivities,0),x=c(roc$specificities,0),col=col3,border=NA)
  lines(roc$sensitivities~roc$specificities,col='red',lwd=2)
  abline(a=1,b=-1,col=col0,lwd=2)
  text(.5,.5,AUC,bty='n',cex=1,srt=45)
  axis(1,pos=0)
  axis(2,pos=1)
  title('Area under curve',xlab='Specificity',ylab='Sensitivity')
  
  qqplot(fitted,observed,pch=16,col=adjustcolor('blue',alpha=.2),ann=F,cex=.5)
  X_<-seq(0,1,.01)
  Y_<-ecdf(fitted)(X_)
  abline(v=median(fitted),h=median(observed),col=c('red','green'))
  lines(Y_~X_,col='orange',lwd=2)
  title('QQ - plot of absence / presence probability',xlab='predicted presence / absence',ylab='observed presence / absence')
  
  
  sep<-separationplot::separationplot(fitted,observed,shuffle = T,line=F,newplot = F,col0=col0,col1=col1)
  abline(h=median(sep$pred),col='red')
  lines(sep$pred~sep$position,col='white',lwd=2)
  #polygon(y=c(p$pred,0,0),x=c(p$position,max(p$position),0),col=adjustcolor('white',.3),lwd=2)
  title('Separation Plot',ylab = 'predicted probability')
  axis(2,pos=0)
  legend('bottom',legend=c('observed Absences','observed Presences'),col=c(col0,col1),bty='n',pch=19,horiz=T)
  
  return(newLine)
}
