source('_SRC/SORP_settings.R')
library(raster)

PUBDIR<-file.path(FINALDIR,'PUB')
SUPPDIR<-file.path(PUBDIR,'SUPP')
dir.create(SUPPDIR, showWarnings = F)

#### resp plots ####
load(COVAR_STACKS)
load(FINAL_COVARS)
load(RFGLS_RASTER_AREA_PREDICTION)
load(MAXENT_RASTER_PREDICTION)
load(SAMPLEDATA1977)
areas<-rgdal::readOGR(files_TARGETAREAS)

exclude<-c("DIST2_MAINLAND","DIST2_SGSS",'DIST2_WAP_ISLANDS')
covar_static_stack<-raster::subset(covar_static_stack, names(covar_static_stack)[!(names(covar_static_stack) %in% exclude)])

xyz<-data.frame()
for (q in analysis_quarts){
  r<-list(MAXENT_PREDICTION_RASTER[[q]]$HSM_Q50,MAXENT_PREDICTION_RASTER[[q]]$HSM_thd_50,RF_AREA_PREDICTION_RASTER[[q]]$I_pred_scaled_avg,covar_static_stack,covar_env_stack[[q]])
  r<-raster::stack(r)
  r$Nadj<-r$I_pred_scaled_avg*r$HSM_thd_50
  r<-raster::mask(r,areas)
  for (aoi in unique(areas$areaname)){
    a<-raster::crop(r,subset(areas,areaname==aoi))
    xyz<-rbind(xyz,data.frame(quart=as.character(pub_quarter_names[q]), area=aoi,rasterToPoints(a)))
  }
}

responses<-c('HSM_Q50','HSM_thd_50','Nadj')
responses_pretty<-c(expression(italic(p[presence])),expression(italic(p[threshold])),expression(italic(N[adj])))
covars_all<-c("depth","aspect","roughness","slope","tpi","tri","DIST2_SHELF" ,"sst","chla")
covars_pretty<-c("depth [m]","aspect [°]","roughness","slope [°]","tpi","tri","distance to shelf edge [m]" ,"sst [°C]","chla [1 / dm³]")

col_islands<-list('Elephant Island' = '#01ff16',
                  "South Orkneys"='#401f5e',
                  "South Shetland"='#ff012b',
                  "South Georgia"='#ffbb01')

y_plots<-length(responses)

for (q in analysis_quarts){
  obs_data<-subset(sampleData,quart==q)
  print(q)
  
  covars<-covars_all[ covars_all %in% analysis_covars[[q]] ]
  x_plots<-length(covars)
  
  area_count<-0
  for (aoi in unique(xyz$area)){
    area_count<-area_count+1
    
    print(aoi)
    dat<-subset(xyz,quart==pub_quarter_names[[q]] & area==aoi)
    
    png(file.path(SUPPDIR,paste0('SUPP_SORP_RESPONSE_PLOTS_',q,'_',aoi,'.png')),x_plots*2000,y_plots*2000,res=300)   
    par(mfrow=c(y_plots,x_plots),cex=3,oma = c(5,5,0,5) +0.1,mar = c(0,0,1,1) + 0.1)
    resp_count<-0
    
    for (y_name in responses){
      resp_count<-resp_count+1
      pretty_y_name<-responses_pretty[which(responses == y_name)]
      
      type<-'gaussian'
      obs_y<-obs_data$I
      if (y_name == 'HSM_thd_50'){
        obs_y<-obs_data$binom
        type <- 'logistic'
      }
      if (y_name == 'HSM_Q50'){
        obs_y<-obs_data$binom
        type <-'beta'
      }
      print(y_name)
      print(type)
      covar_count<-0
      for (x_name in covars){
        covar_count<-covar_count+1
        print(x_name)
        pretty_x_name<-covars_pretty[which(covars == x_name)]
        
        df<-data.frame(x=dat[[x_name]],y=dat[[y_name]])
        df<-df[complete.cases(df),]
        
        obs<-data.frame(y=obs_y,x=obs_data[[x_name]])
        
        resp_plot(x_pred=df$x,y_pred=df$y,x_obs=obs$x,y_obs=obs$y, type, col_mainline='orange',SHOW_AXIS = F)
        
        if(resp_count && area_count == 1){
          axis(3,labels=NA)
        }
        
        if(resp_count == y_plots){
          axis(1)
          mtext(pretty_x_name,1,line=3,cex=4)
        }
        
        if(covar_count==1){
          axis(2)
          mtext(pretty_y_name,2,line=3,cex=4)
        }
        
        if(covar_count==x_plots){
          axis(4,labels=NA)
          mtext(paste0(aoi,'\n',pub_quarter_names[[q]]),4,line=2,cex=4, outer=T)
        }
        box()
      }
    }
    graphics.off()
  }
}

#### only validation data ####
load(MAXENT_MODEL_SELECTION)
load(RFGLS_VALIDATION_DATA)

val<-data.frame()
for (q in analysis_quarts){
  obs<-RF_validation[RF_validation$quart==q,]
  
  m<-MAXENT_FINAL_MODELS[[q]]$model@models
  selection <- MAXENT_FINAL_MODELS[[q]]
  evaluations<-selection$evaluation_list
  
  res<-matrix(nrow=nrow(obs),ncol=length(m),NA)
  thresh<-matrix(nrow=nrow(obs),ncol=length(m),NA)
  for (i in 1:length(m)){
    res[,i]<-dismo::predict(m[[i]],x=obs) 
    thresh[,i]<-res[,i] > dismo::threshold(evaluations[[i]],"spec_sens")
  }
  
  obs$HSM_Q50<-apply(res,1, FUN = mean)
  obs$HSM_thd_50<-apply(thresh,1, FUN = hard_max)
  obs$I_pred_scaled_avg<-round(obs$I_pred ,0)
  val<-rbind(val,obs)
}
val$binom<-0
val$binom[val$I_obs>0]<-1
val$id<-1:nrow(val)

responses<-c('HSM_Q50','HSM_thd_50','I_pred_scaled_avg')
responses_pretty<-c(expression(italic(p[presence])),expression(italic(p[threshold])),expression(italic(N[RFGLS])))
x_plots<-length(covars)
y_plots<-length(responses)

for (q in analysis_quarts){
  dat<-subset(val,quart==q)
  print(q)
  
  png(file.path(SUPPDIR,paste0('SUPP_SORP_RESPONSE_OBS_',q,'.png')),x_plots*1000,y_plots*1000,res=100)   
  par(mfrow=c(x_plots,y_plots))
  for (y_name in responses){
    for (x_name in covars){
      if (!(x_name %in% names(dat))){next}
      pretty_x_name<-covars_pretty[which(covars == x_name)]
      pretty_y_name<-responses_pretty[which(responses == y_name)]
      
      df<-data.frame(x=dat[[x_name]],y=dat[[y_name]])
      df<-df[complete.cases(df),]
      y_range<-c(0,ceiling(range(df$y)[1]))
      
      if (y_name == 'HSM_thd_50'){
        y_range<-c(0,1)
        m<-mgcv::gam(y~x+1,data=df,family=binomial(link='logit'))
      }
      if (y_name == 'HSM_Q50'){
        y_range<-c(0,1)
        m<-mgcv::gam(y~s(x)+1,data=df,family=betar)
      }
      if (!(y_name %in% c('HSM_Q50','HSM_thd_50'))){
        m<-mgcv::gam(y~s(x)+1,data=df,family=gaussian)
      }
      y_<-data.frame(predict(m,df,type='link',se.fit=T))
      y_$fit<-m$family$linkinv(y_$fit)
      y_$se.fit<-m$family$linkinv(1.95*y_$se.fit)
      y_$x<-df$x
      y_<-y_[order(y_$x),]
      obs<-data.frame(Y=dat$I_obs,X=dat[[x_name]])
      
      plot(y~x,data=df,ylim=c(y_range[1],y_range[2]),xlab=pretty_x_name,ylab=pretty_y_name,type='n',axes=T)
      rug(dat[[x_name]][dat$binom==0],side=1,col='red2')
      rug(dat[[x_name]][dat$binom==1],side=3,col='seagreen1')
      vert_x<-c(y_$x,rev(y_$x))
      vert_y<-c(y_$fit+y_$se.fit,rev(y_$fit)-y_$se.fit)
      polygon(vert_x,vert_y, col = adjustcolor('grey',.3),border=NA)
      points(y~x,data=df,pch=16,col=adjustcolor('grey',.5))
      
      if (y_name == 'HSM_Q50'){
        points(dat$binom~dat[[x_name]],col=adjustcolor(col_islands[[aoi]],.5),pch=16,cex=.25)
        x_threshold<-y_$x[which.min(abs(y_$fit - .5))]
        abline(v=x_threshold)
      }
      
      if (y_name == 'HSM_thd_50'){
        points(dat$binom~dat[[x_name]],col=adjustcolor(col_islands[[aoi]],.5),pch=16,cex=.25)
      }
      
      if (!(y_name %in% c('HSM_Q50','HSM_thd_50'))){
        points(y~x,data=df[df$y>quantile(df$y,.9),],pch=16,col=adjustcolor(col_islands[[aoi]],.7))
        points(obs$Y~obs$X,col=adjustcolor(col_islands[[aoi]],.5),pch=16,cex=.5)
      }
      
      lines(y_$fit~y_$x,col=col_islands[[aoi]], lty=2)
      axis(1)
      
    }
  }
  graphics.off()
}