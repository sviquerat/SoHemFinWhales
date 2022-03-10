source('_SRC/SORP_settings.R')

PUBDIR<-file.path(FINALDIR,'PUB')
SUPPDIR<-file.path(PUBDIR,'SUPP')
dir.create(SUPPDIR, showWarnings = F)

#### copy files from various locations into pubdir ####
file<-file.path(FINALDIR,'SORP_AREA_ESTIMATES.xlsx')
file.copy(file, file.path(PUBDIR,paste0('TABLE_6_',basename(file))),overwrite = T)

file<-file.path(FINALDIR,'SORP_MODEL_DEFINITION.xlsx')
file.copy(file, file.path(PUBDIR,paste0('TABLE_4_',basename(file))),overwrite = T)

file<-file.path(MAXENTDIR,'SORP_MAXENT_FINAL_MODEL_DIAGS.xlsx')
#grep only pretties
mx_summary<-openxlsx::read.xlsx(file)
df<-mx_summary[,1:6]
df$quart<-as.character(pub_quarter_names[df$quart])
cols<-grep('.*_pretty',names(mx_summary))
df<-cbind(df, mx_summary[,cols])
names(df)<-gsub('_pretty','',names(df))
#combine with model definitions
openxlsx::write.xlsx(df, file.path(PUBDIR,paste0('TABLE_5_',basename(file))))

file<-file.path(MAXENTDIR,'SORP_MAXENT_MODEL_RESULTS_REPLICATES.xlsx')
file.copy(file, file.path(SUPPDIR,paste0('SUPP_',basename(file))),overwrite = T)

file<-file.path(MAXENTDIR,'SORP_MAXENT_MODEL_RESULTS_SUMMARY.xlsx')
mx_summary<-openxlsx::read.xlsx(file)
df<-mx_summary[,1:7]
df$quart<-as.character(pub_quarter_names[df$quart])
cols<-grep('.*_pretty',names(mx_summary))
df<-cbind(df, mx_summary[,cols])
names(df)<-gsub('_pretty','',names(df))
openxlsx::write.xlsx(df, file.path(SUPPDIR,'SUPP_SORP_MAXENT_MODEL_RESULTS_SUMMARY.xlsx'))

files<-list.files(MAXENTGFX, '.*_FINAL_DIAGS.*.png', full.names = T)
for (f in files){
  file.copy(f, file.path(SUPPDIR,paste0('SUPP_',basename(f))),overwrite = T)
}

files<-list.files(MAXENTMODELGFX, '.*_DIAGS_EVAL.*.png', full.names = T)
for (f in files){
  file.copy(f, file.path(SUPPDIR,paste0('SUPP_',basename(f))),overwrite = T)
}

files<-list.files(MAXENTGFX, '.*_PCA_.*.png', full.names = T)
for (f in files){
  file.copy(f, file.path(SUPPDIR,paste0('SUPP_',basename(f))),overwrite = T)
}

files<-list.files(MAXENTMODELGFX, '.*_DIAGS_EVAL_.*.png', full.names = T)
for (f in files){
  if(grepl('.*model.*',basename(f))){next}
  file.copy(f, file.path(SUPPDIR,paste0('SUPP_',basename(f))),overwrite = T)
}

#pub figures
files<-list.files(MAXENTMODELGFX, '.*_DIAGS_EVAL_.*.png', full.names = T)
for (f in files){
  if(grepl('.*_AUC.png',basename(f))){file.copy(f, file.path(PUBDIR,paste0('FIG_5_',basename(f))),overwrite = T)}
  if(grepl('.*_kappa.png',basename(f))){file.copy(f, file.path(PUBDIR,paste0('FIG_6_',basename(f))),overwrite = T)}
}

#### provider summary table (table 1) ####
load(PRESENCEDATA)
load(ABSENCEDATA)

data_1977<-rbind(data,data.absence)
data_1977<-subset(data_1977,year>1977)

data_1977$dataSource<-gsub('PS[0-9].*','Herr',data_1977$source)
data_1977$dataSource[data_1977$dataSource=='Icefish2016']<-'Herr'
data_1977$dataSource<-gsub('BAS - PHAROS.*','PHAROS',data_1977$dataSource)
data_1977$dataSource<-gsub('BAS - .*','BAS',data_1977$dataSource)
data_1977$dataSource<-gsub('2019 CCAMLR SURVEY','CCAMLR SURVEY',data_1977$dataSource)
data_1977$dataSource<-gsub('Cethus','Fundación Cethus',data_1977$dataSource)
data_1977$dataSource<-gsub('Herr','GERMAN SURVEYS',data_1977$dataSource)
data_1977$dataSource<-gsub('IWC_SOWER','IWC IDCR / SOWER',data_1977$dataSource)
data_1977$dataSource<-gsub('_','\ ',data_1977$dataSource)

# this is including all data beyond the survey area!
records_table<-sqldf::sqldf('select dataSource,"" as Type_of_data, sum(binom) as N, sum(best) as I, "" as A from data_1977 group by dataSource')
absences<-sqldf::sqldf('select dataSource, "" as Type_of_data, "y" as A from data_1977 where binom == 0 group by dataSource')
records_table$A[records_table$dataSource %in% absences$dataSource]<-'y'
records_table<-records_table[order(records_table$dataSource),]
records_table$A[records_table$A=='']<-'-'
records_table<-rbind(records_table,data.frame(dataSource='Total',Type_of_data='', N=sum(records_table$N), I=sum(records_table$I), A=''))
openxlsx::write.xlsx(records_table,file.path(PUBDIR,'TABLE_1_SORP_SUMMARY_RECORDS.xlsx'))

df<-data.frame(dataSource=records_table[1:(nrow(records_table)-1),1], years='', source = '')
openxlsx::write.xlsx(df,file.path(SUPPDIR,'SUPP_DATA_REFERENCES.xlsx'))

#### sample data prep ####
load(SAMPLEDATA1977)
sampleData$month_name<-month.abb[sampleData$month]
sampleData$pretty_quart<-as.character(pub_quarter_names[sampleData$quart])
sd<-subset(sampleData, !is.na(CellID))

#### summary by month (fig 4, Table 3) ####
records_quart_month<-sqldf::sqldf('select pretty_quart as Quarter, month, month_name as Month, sum(G) as G, sum(I) as I,0 as gs,
                                  count(nullif(binom,0)) as Cpresence,count(nullif(binom,1)) as Cabsence 
                                  from sd group by pretty_quart, month_name')
records_quart_month<-records_quart_month[order(records_quart_month$Quarter,records_quart_month$month),]
records_quart_month<-records_quart_month[,-which(names(records_quart_month) == 'month')]
total<-data.frame(Quarter='', Month='Total',G=sum(records_quart_month$G),I=sum(records_quart_month$I),gs=0,
                  Cpresence=sum(records_quart_month$Cpresence),Cabsence=sum(records_quart_month$Cabsence))
records_quart_month<-rbind(records_quart_month,total)
records_quart_month$gs<-round(records_quart_month$I/records_quart_month$G,2)
#could add se
openxlsx::write.xlsx(records_quart_month,file.path(PUBDIR,'TABLE_3_SORP_SUMMARY_RECORDS_Month.xlsx'))

df<-records_quart_month[1:12,]
df$q<-df$Quarter
order<-c(paste0(df$Month[df$Quarter=='Q1'],collapse=' - '),
         paste0(df$Month[df$Quarter=='Q2'],collapse=' - '),
         paste0(df$Month[df$Quarter=='Q3'],collapse=' - '),
         paste0(df$Month[df$Quarter=='Q4'],collapse=' - '))
names(order)<-c('Q1','Q2','Q3','Q4')

df$Quarter[df$Quarter=='Q1']<-paste0(df$Month[df$Quarter=='Q1'],collapse=' - ')
df$Quarter[df$Quarter=='Q2']<-paste0(df$Month[df$Quarter=='Q2'],collapse=' - ')
df$Quarter[df$Quarter=='Q3']<-paste0(df$Month[df$Quarter=='Q3'],collapse=' - ')
df$Quarter[df$Quarter=='Q4']<-paste0(df$Month[df$Quarter=='Q4'],collapse=' - ')
df$Quarter<-factor(paste0(df$q,'\n',df$Quarter),levels = paste0(names(order),'\n', order))

df<-reshape2::melt(df,id.vars=c('Quarter','q'),measure.vars=c('G','I', 'Cpresence','Cabsence'))

df$group[df$variable=='I']<-'I - Number of fin whales'
df$group[df$variable=='G']<-'G - Number of records'
df$group[df$variable=='Cpresence']<-'Number of presence cells'
df$group[df$variable=='Cabsence']<-'Number of absence cells'
df$group<-factor(df$group,levels = sort(unique(df$group)))

p<-ggplot2::ggplot(data=df,aes(x=Quarter,y=value,fill=group))
p<-p+geom_col(position=position_dodge())+scale_fill_brewer(palette="Set2")+MAINTHEME
p<-p+labs(x='',y='',fill='')

png(file.path(PUBDIR,'FIG_4_SORP_SUMMARY_MONTHS.png'),8000,6000,res=600)
print(p)
dev.off()


#### summary by year (Fig 3) ####
records_quart_year<-sqldf::sqldf('select year as Year, sum(G) as G, sum(I) as I,0 as gs,
                                  count(nullif(binom,0)) as Cpresence,count(nullif(binom,1)) as Cabsence 
                                  from sd group by year')
records_quart_year<-records_quart_year[order(records_quart_year$Year),]

total<-data.frame(Year='Total',G=sum(records_quart_year$G),I=sum(records_quart_year$I),gs=0,
                  Cpresence=sum(records_quart_year$Cpresence),Cabsence=sum(records_quart_year$Cabsence))
records_quart_year<-rbind(records_quart_year,total)
records_quart_year$gs<-round(records_quart_year$I/records_quart_year$G,2)


df<-records_quart_year[1:(nrow(records_quart_year)-1),]
df<-reshape2::melt(df,id.vars=c('Year'),measure.vars=c('G','I', 'Cpresence','Cabsence'))

df$group[df$variable=='I']<-'I - Number of fin whales'
df$group[df$variable=='G']<-'G - Number of records'
df$group[df$variable=='Cpresence']<-'Number of presence cells'
df$group[df$variable=='Cabsence']<-'Number of absence cells'
df$group<-factor(df$group,levels = sort(unique(df$group)))

p<-ggplot2::ggplot(data=df,aes(x=Year,y=value,fill=group))
p<-p+geom_col(position=position_dodge())+scale_fill_brewer(palette="Set2")+MAINTHEME
p<-p+labs(x='',y='',fill='')+theme(axis.text.x = element_text(angle=90))

png(file.path(PUBDIR,'FIG_3_SORP_SUMMARY_YEAR.png'),12000,6000,res=600)
print(p)
dev.off()

#RF DIAGS?

#### NEXT #####

SURVEYAREA<-rgdal::readOGR(file.path(MERGEDDIR,'SHAPES','SORP_BOUNDARY_100K_BUFFER.gpkg'))
SURVEYAREA@proj4string<-ANT_POL_STEREO
SURVEYAREA<-sp::spTransform(SURVEYAREA,IBCSO)

areas<-rgdal::readOGR(files_TARGETAREAS)

for (q in unique(sampleData$quart)){
  print(q)
  pretty_quart<-pub_quarter_names[[q]]
  print(pretty_quart)
  presData<-subset(sampleData,dataType == 'presence' & quart == q)
  absData<-subset(sampleData,dataType == 'absence' & quart == q)
  
  vertices<-subset(sampleData,quart == q)
  vertices<-vertices[chull(vertices$data_y,vertices$data_x),]
  LL<-sp::SpatialPoints(cbind(vertices$data_x,vertices$data_y),IBCSO)
  
  HULL<-points2polygons(LL,name=pretty_quart, IBCSO)
  SMALLAREA<-rgeos::gDifference(SURVEYAREA,HULL)
  SMALLAREA<-sp::SpatialPolygonsDataFrame(SMALLAREA,data=data.frame(name=pretty_quart))
  rgdal::writeOGR(HULL,dsn=file.path(FINALSPDIR,paste0('CHULL_',q,'.gpkg')), layer = paste0('CHULL ',q), driver='GPKG', overwrite_layer = T)
  rgdal::writeOGR(SMALLAREA,dsn=file.path(FINALSPDIR,paste0('MASK_',q,'.gpkg')), layer = paste0('MASK ',q), driver='GPKG', overwrite_layer = T)
}

#### data hulls done

ABS<-subset(predGrid,CellID %in% absData$CellID)
PRES<-subset(predGrid,CellID %in% presData$CellID)
q_months<-month.abb[quarts[[QUART]]]
subtitle<-paste0(QUART,' - ',paste0(q_months,collapse=' / '))

}


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