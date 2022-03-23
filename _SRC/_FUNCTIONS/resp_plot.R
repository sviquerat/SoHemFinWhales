require(mgcv)
require(stats)

resp_plot<-function(x_pred,y_pred,x_obs,y_obs,type='gaussian',col_mainline='red', obs_quantile_limit = .9,
                    col_absence='red', col_presence ="green", col_quantile_limit = 'blue', smooth_model = 'lowess', SHOW_AXIS=T){
  pred_df<-data.frame(y=y_pred,x=x_pred)
  obs_df<-data.frame(y=y_obs,x=x_obs)
  
  y_range<-range(pred_df$y)
  x_range<-range(pred_df$x)
  
  if (type == 'logistic'){
    y_range<-c(0,1)
    m<-mgcv::gam(y~x,data=pred_df,family=binomial(link='logit'))
    smooth<-data.frame(predict(m,pred_df,type='link',se.fit=T))
    fit<-m$family$linkinv(smooth$fit)
    smooth_df<-data.frame(fit=fit,x=pred_df$x)
  }
  if (type == 'beta'){
    y_range<-c(0,1)
    m<-mgcv::gam(y~s(x),data=pred_df,family=betar)
    smooth<-data.frame(predict(m,pred_df,type='link',se.fit=T))
    fit<-m$family$linkinv(smooth$fit)
    smooth_df<-data.frame(fit=fit,x=pred_df$x)
  }
  if (type=='gaussian'){
    m<-stats::loess(pred_df$y ~ pred_df$x)
    smooth_df<-data.frame(fit=m$fitted,x=as.numeric(m$x))
  }
  
  smooth_df<-smooth_df[order(smooth_df$x),]
  
  plot(y~x,data=pred_df,ylim=c(y_range[1],y_range[2]),xlim=c(x_range[1],x_range[2]),type='n',axes=F,ann=F)
  grid()
  
  if(SHOW_AXIS){
    title(ylab=y_name,xlab=x_name)
    axis(1)
    axis(2)
  }
  
  points(y~x,data=pred_df,pch=16,col=adjustcolor('grey',.3),cex=.5) #predicted data
  
  if (type == 'logistic'){
    points(y~x,data=obs_df[obs_df$y==0,],col=adjustcolor(col_absence,.5),pch=16,cex=.25)
    points(y~x,data=obs_df[obs_df$y==1,],col=adjustcolor(col_presence,.5),pch=16,cex=.25)
    x_threshold<-smooth_df$x[which.min(abs(smooth_df$fit - .5))]
    abline(v=x_threshold, h=mean(smooth_df$fit),col=adjustcolor('black',.9))
  }
  
  if (type == 'beta'){
    points(y~x,data=obs_df[obs_df$y==0,],col=adjustcolor(col_absence,.5),pch=16,cex=.25)
    points(y~x,data=obs_df[obs_df$y==1,],col=adjustcolor(col_presence,.5),pch=16,cex=.25)  
    x_threshold<-smooth_df$x[which.max((smooth_df$fit < mean(smooth_df$fit)))] #at which point drops the ppresence below mean?
    abline(v=x_threshold,h=mean(smooth_df$fit),lty=2,col=adjustcolor('black',.9))
  }
  
  if (type == 'gaussian'){
    points(obs_df$y~obs_df$x,col=adjustcolor(col_presence,.5),pch=16,cex=.5)
    points(y~x,data=pred_df[pred_df$y>=quantile(pred_df$y,obs_quantile_limit),],pch=16,col=col_quantile_limit,cex=.5)
  }
  
  lines(smooth_df$fit~smooth_df$x,col=col_mainline, lty=1,lwd=3) #the smooth
}
