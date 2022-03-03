source('_SRC/SORP_settings.R')

load(SAMPLEDATA1977)
load(SAMPLEGRID)
load(COVAR_STACKS)
load(FINAL_COVARS)
load(RFGLS_VALIDATION_DATA)
load(RFGLS_RASTER_PREDICTION)
load(MAXENT_RASTER_PREDICTION)
load(FINAL_PREDICTION_RESULTS)
areas<-rgdal::readOGR(files_TARGETAREAS)




p<-ggplot(data=summary_areas,aes(group=pretty_quart,y=N,x=pretty_quart,fill=pretty_quart))
p<-p+geom_bar(stat='identity') + theme(legend.position='none')
p<-p+labs(x='',y = 'adjusted total abundance of fin whales')
p<-p+facet_wrap(.~area, scales='free')
p

p<-ggplot(data=summary_areas,aes(group=pretty_quart,y=N,x=area,fill=area))
p<-p+geom_bar(stat='identity') + theme(legend.position='none')
p<-p+labs(x='',y = 'adjusted total abundance of fin whales')
p<-p+facet_wrap(.~pretty_quart, scales='free')
p



covars<-c(static_covCols,env_covCols)

df<-area_results[,c('adj_I','area','pretty_quart', covars)]
df<-df[complete.cases(df),]
df$ID<-rownames(df)
m_df<-reshape2::melt(df, measure.vars=covars,id.vars='ID')
m_df<-sqldf::sqldf('select * from m_df as a left join df as b on a.ID=b.ID;')
m_df<-m_df[,1:6]
[m_df$area=='Elephant Island',]

for (aoi in unique(m_df$area)){
  p<-ggplot(data=m_df[m_df$area==aoi,], aes(x=value, y=adj_I, color=variable))
  p<-p+geom_point(color='grey',alpha=.2)
  p<-p+geom_smooth()
  p<-p+facet_grid(pretty_quart~area+variable,scales='free')
  p<-p+labs(x='covar value', y='adjusted abundance estimate') + theme(legend.position = 'none')
  p<-p+MAINTHEME 
  
  png(file.path(FINALGFXDIR,paste0('SORP_RESPONSE_PREDICTED_ABUNDANCE_',aoi,'.png')),3000,3000,res=300)
  print(p)
  graphics.off()
}

png(file.path(FINALGFXDIR,paste0('SORP_RESPONSE_PREDICTED_PRESENCE_THD_',var,'_',q,'.png')),3000,3000,res=300)
print(h)
graphics.off()

}


pair_names<-c('pretty_quart','I_obs','I_pred','aspect', 'depth', 'slope', 'tpi', 'DIST2_SHELF', 'chla', 'sst')
png(file.path(FINALGFXDIR,'SORP_RFGLS_VALIDATION_PAIRS.png'),5000,5000,res=300)
p<-GGally::ggpairs(RF_validation[,pair_names], aes(colour = pretty_quart, alpha = 0.2))
p<-p+MAINTHEME
print(p)
graphics.off()


source('_SRC/SORP_settings.R')

SURVEYAREA<-rgdal::readOGR(files_SURVEYAREA)
SURVEYAREA<-sp::spTransform(SURVEYAREA,ANT_POL_STEREO)
WORLD<-rgdal::readOGR(files_WORLD)
WORLD<-sp::spTransform(WORLD,ANT_POL_STEREO)
load(PRED_GRID)

# resp plots - into visualise?
y<-Q50_masked@data@values
pframe<-data.frame(p_hsm=y)
for (var in names(predictors)){
  p_map<-predictors[[var]]*thd_50
  pframe[[var]]<-p_map@data@values
}
p<-GGally::ggpairs(pframe, aes(alpha = 0.2))
p
#### Tables and summary plots ####
load(PRESENCEDATA)
load(ABSENCEDATA)

data_1977<-rbind(data,data.absence)
data_1977<-subset(data_1977,year>1977)
data_1977$dataSource<-gsub('PS[0-9].*','Herr',data_1977$source)
data_1977$dataSource[data_1977$dataSource=='Icefish2016']<-'Herr'
data_1977$dataSource<-gsub('BAS - .*','BAS',data_1977$dataSource)

# this is including all data beyond the survey area!
records_table<-sqldf::sqldf('select dataSource,"" as Type_of_data, "" as A,  sum(binom) as N from data_1977 group by dataSource')
absences<-sqldf::sqldf('select dataSource, "" as Type_of_data, "y" as A from data_1977 where binom == 0 group by dataSource')
records_table$A[records_table$dataSource %in% absences$dataSource]<-'y'
openxlsx::write.xlsx(records_table,file.path(FINALDIR,'SORP_SUMMARY_RECORDS.xlsx'))

#### quarterly predictions ####
load(SAMPLEDATA1977)
load(PRED_GRID)

sampleData$month_name<-month.abb[sampleData$month]

records_quart_month<-sqldf::sqldf('select quart, month, month_name, sum(I) as I, count(nullif(binom,0)) as R, 0 as IR, count(nullif(binom,1)) as A from sampleData group by quart, month_name')
records_quart_month$IR[records_quart_month$R>0]<-round(records_quart_month$I/records_quart_month$R[records_quart_month$R>0],4)
for (mn in unique(records_quart_month$month_name)){
  df<-subset(sampleData,month_name==mn & binom == 1)
  se<-sd(df$I)/sqrt(nrow(df))
  records_quart_month$IR[records_quart_month$month_name==mn] <- paste0(format(records_quart_month$IR[records_quart_month$month_name==mn],nsmall=4), 
                                                                         ' ± ', format(round(se,4),nsmall=4))
}
records_quart_month<-records_quart_month[order(records_quart_month$month),]
total<-data.frame(quart='', month='',month_name='total',I=sum(records_quart_month$I),R=sum(records_quart_month$R),
                  IR = paste0(format(round(mean(sampleData$I),4),nsmall=4), ' ± ', format(round(sd(sampleData$I)/sqrt(nrow(sampleData)),4),nsmall=4)),A=sum(records_quart_month$A))

records_quart_month<-rbind(records_quart_month,total)

openxlsx::write.xlsx(records_quart_month[,-which(names(records_quart_month)=='month')],file.path(FINALDIR,'SORP_SUMMARY_PER_MONTH.xlsx'))

df<-reshape2::melt(records_quart_month[1:12,],measure.vars=c('I','R'))
df$month_name<-factor(df$month_name,levels=month.abb)
df$group<-'Number of fin whales'
df$group[df$variable=='R']<-'Number of records'
p<-ggplot2::ggplot(data=df,aes(x=month_name,y=value,fill=group))
p<-p+geom_bar(stat="identity", position=position_dodge())+scale_fill_brewer(palette="Set2")+MAINTHEME
p<-p+labs(x='',y='',fill='')

png(file.path(FINALGFXDIR,'SORP_SUMMARY_MONTHS.png'),2000,2000,res=300)
print(p)
dev.off()

records_quart_year<-sqldf::sqldf('select quart, year, sum(I) as I, count(nullif(binom,0)) as R, 0 as IR, count(nullif(binom,1)) as A from sampleData group by quart, year')
df<-reshape2::melt(records_quart_year,measure.vars=c('I','R'))
df$group<-'Number of fin whales'
df$group[df$variable=='R']<-'Number of records'

p<-ggplot2::ggplot(data=df,aes(x=year,y=value,fill=group))
p<-p+geom_bar(stat="identity", position=position_dodge())+scale_fill_brewer(palette="Set2")+MAINTHEME
p<-p+labs(x='',y='',fill='')
print(p)


p<-ggplot2::ggplot(data=df,aes(x=quart,y=value,fill=group))
p<-p+geom_bar(stat="identity", position=position_dodge())+scale_fill_brewer(palette="Set2")+MAINTHEME
p<-p+labs(x='',y='',fill='')
print(p)

for (QUART in c('A','B','C','D')){
  print(QUART)
  
  load(file = file.path(RESDATDIR,paste0('SORP_FINAL_predictions_quart_',QUART,'.RData')))
  
  predData$predictions$I_soft_Q50<-round(predData$predictions$I_soft_Q50,0) #maybe this helps with the high sum of abundances
  predData$validation$I_soft_Q50<-round(predData$validation$I_soft_Q50,0) #maybe this helps with the high sum of abundances
  
  pData<-subset(predData$predictions,!is.na(predData$predictions$aboveThreshold))
  sData<-subset(predData$validation,!is.na(predData$validation$aboveThreshold))
  sData<-subset(sData,!is.na(sData$x))
  
  png(file.path(DIAGDIR,paste0('SORP_FINAL_diagnostics_GOF_',QUART,'.png')),3400,3400,res=300)
  par(mfrow=c(2,1))
  plot(I_soft_Q50~I,data=sData,pch=16,col=adjustcolor('blue',alpha=.2),xlim=c(0,max(c(I_soft_Q50,I))),ylim=c(0,max(c(I_soft_Q50,I))),ann=F)
  m<-lowess(sData$I_soft_Q50~sData$I)
  lines(m$y~m$x,col='red')
  abline(a=0,b=1,col='green',lwd=2)
  legend('topleft',legend=paste0("Pearson's correlation factor: ",round(cor(sData$I_soft_Q50,sData$I),4)))
  title(main='predicted versus oberved number of Fin whales',xlab=expression(I[observed]),ylab=fancyRESPONSES$I,sub=QUART)
  b<-hist(pData$aboveThreshold,plot=F)
  dens<-density(pData$aboveThreshold,from=0,to=1)
  dens$y_<-dens$y/max(dens$y)*max(b$counts)
  plot(b$counts~b$mids,xlim=c(0,1),col=adjustcolor('blue',.4),ann=F,type='n')
  grid()
  plot(b,xlim=c(0,1),col=adjustcolor('blue',.4),ann=F,add=T)
  lines(dens$y_~dens$x,col='red',lwd=2)
  box()
  title(main=paste0('Probability of Fin Whale occurence being non-random '),sub=QUART,xlab=fancyRESPONSES$aboveThreshold,ylab='counts')
  graphics.off()
  
  png(file.path(DIAGDIR,paste0('SORP_FINAL_diagnostics_OccurenceProb_',QUART,'.png')),3400,3400,res=300)
  img<-akima::interp(x=sData$I_soft_Q50,y=sData$I,z=sData$aboveThreshold*100,duplicate='median')
  filled.contour(img,key.title=title(sub=fancyRESPONSES$aboveThreshold))
  title(main='Probability of fin whale occurence > 50 %',xlab=expression(I[observed]),ylab=fancyRESPONSES$I,sub=QUART)
  graphics.off()
  
  presData<-subset(sampleData,dataType == 'presence' & month %in% quarts[[QUART]])
  absData<-subset(sampleData,dataType == 'absence' & month %in% quarts[[QUART]])
  
  vertices<-subset(sampleData,month %in% quarts[[QUART]])
  vertices<-vertices[chull(vertices$y,vertices$x),]
  LL<-sp::SpatialPoints(cbind(vertices$x,vertices$y),ANT_POL_STEREO)
  HULL<-points2polygons(LL,name=QUART)
  SMALLAREA<-rgeos::gDifference(SURVEYAREA,HULL)
  SMALLAREA<-sp::SpatialPolygonsDataFrame(SMALLAREA,data=data.frame(name=QUART))
  rgdal::writeOGR(HULL,dsn=file.path(FINALSPDIR,paste0('CHULL_',QUART,'.gpkg')), layer = paste0('CHULL ',QUART), driver='GPKG', overwrite_layer = T)
  rgdal::writeOGR(SMALLAREA,dsn=file.path(FINALSPDIR,paste0('MASK_',QUART,'.gpkg')), layer = paste0('MASK ',QUART), driver='GPKG', overwrite_layer = T)

  ABS<-subset(predGrid,CellID %in% absData$CellID)
  PRES<-subset(predGrid,CellID %in% presData$CellID)
  q_months<-month.abb[quarts[[QUART]]]
  subtitle<-paste0(QUART,' - ',paste0(q_months,collapse=' / '))
  
  for (resp in RESPONSES){
    print(resp)
    fileName<-paste0('SORP_FINAL_',resp,'_',QUART)
    pos<-which(names(pData) %in% c('x','y',resp))
    pg<-pData[,pos]
    pR<-pg[complete.cases(pg),]
    
    #pR<-p[,c('x','y',resp)]
    coordinates(pR) <- ~ x + y
    raster::crs(pR) <- ANT_POL_STEREO
    r<-raster::raster(raster::extent(pR),res = RASTERRES)
    r<-raster::rasterize(pR,r,field=resp)
    raster::crs(r) <- ANT_POL_STEREO
    raster::writeRaster(r,file.path(FINALSPDIR,fileName),format='GTiff',overwrite=T)
    
    #create subsets of rasters
    
    png(file.path(FINALGFXDIR,paste0(fileName,'.png')),2400,2400,res=300)
    plot(r,main=fancyRESPONSES[[resp]],sub=subtitle,xlab='',ylab='',col = rev(resp.colours[[resp]](25)))
    plot(HULL,add=T,lty=2,lwd=3,border='white')
    plot(SMALLAREA,add=T,col=adjustcolor('grey',alpha=.5),border=NA)
    ignore<-polyColourPlot(WORLD,'SOV_A3',add=T)
    points(y~x,absData,col=adjustcolor('black',alpha=.4),pch=16,cex=.4)
    points(y~x,presData,col=adjustcolor('blue',alpha=.4),pch=4,cex=1)
    graphics.off()
    
    png(file.path(DETAILDIR,paste0(fileName,'_DETAIL.png')),2400,2400,res=300)
    par(mfrow=c(1,2))
    plot(r,main=fancyRESPONSES[[resp]],sub=paste0(subtitle,'\nSouth Georgia'),xlab='',ylab='',col = rev(resp.colours[[resp]](25)),xlim=EXTENT_SG[1:2],ylim=EXTENT_SG[3:4])
    ignore<-polyColourPlot(WORLD,'SOV_A3',add=T)
    plot(ABS,col=adjustcolor('black',alpha=.3),border=NA,add=T)
    plot(PRES,col=adjustcolor('blue',alpha=.3),border =NA,add=T)

    plot(r,main=fancyRESPONSES[[resp]],sub=paste0(subtitle,'\nWAP'),xlab='',ylab='',col = rev(resp.colours[[resp]](25)),xlim=EXTENT_WAP_ISLAND[1:2],ylim=EXTENT_WAP_ISLAND[3:4])
    ignore<-polyColourPlot(WORLD,'SOV_A3',add=T)
    plot(ABS,col=adjustcolor('black',alpha=.2),border=NA,add=T)
    plot(PRES,col=adjustcolor('blue',alpha=.4),border =NA,add=T)
    graphics.off()
  }
}

# refer to all the scripts and re run the plots with pub settings!


png(file.path(EXP_GFX_DIR,'SORP_pairs.png'),4200,4200,res=150)
p<-GGally::ggpairs(sample_grid[,c('quarter',analysis_cols)], aes(colour = quarter, alpha = 0.2))
p<-p+MAINTHEME
print(p)
graphics.off()