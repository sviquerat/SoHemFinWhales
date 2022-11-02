setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #only works in rstudio / posit
source('_SRC/SORP_settings.R')

PUBDIR<-file.path(FINALDIR,'PUB')
SUPPDIR<-file.path(PUBDIR,'SUPP')
dir.create(SUPPDIR, showWarnings = F)

#### copy files from various locations into pubdir ####
#### tables ####
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
#add line break before ci
df$modelName<-gsub('model','m',df$modelName)
killcols<-which(names(df) %in% c('replicate','sensitivity_threshold'))
df<-df[,-killcols]
pub_names<-c('quart','id',	'Cpresence',	'Cabsences',	'cor',	'AUC',	'κmax',	'ssens',	'omission',	'prevalence',	'sspec',	'sensitivity')
names(df)<-pub_names
openxlsx::write.xlsx(df, file.path(SUPPDIR,paste0('SUPP_SORP_MAXENT_FINAL_MODEL_RESULTS_SUMMARY.xlsx')))
pub_names<-c('quart','id',	'Cpresence',	'Cabsences',	'AUC',	'κmax')
df<-df[,which(names(df) %in% pub_names)]
openxlsx::write.xlsx(df, file.path(PUBDIR,paste0('TABLE_5_',basename(file))))

file<-file.path(FINALDIR,'SORP_RFGLS_VALIDATION.xlsx')
rfgls_summary<-openxlsx::read.xlsx(file)
df<-rfgls_summary[,c(1:3,5)]
df$accuracy<-paste0(round(rfgls_summary$accuracy_q50,4), '\n(',round(rfgls_summary$accuracy_q05,4),' - ',round(rfgls_summary$accuracy_q95,4),')')
df<-df[order(df$quarter),]
df$IQR<-round(rfgls_summary$accuracy_q95-rfgls_summary$accuracy_q05,4)
openxlsx::write.xlsx(df, file.path(PUBDIR,paste0('TABLE_6_',basename(file))))

file<-file.path(FINALDIR,'SORP_AREA_ESTIMATES.xlsx')
file.copy(file, file.path(PUBDIR,paste0('TABLE_7_',basename(file))),overwrite = T)

file<-file.path(MAXENTDIR,'SORP_MAXENT_MODEL_RESULTS_REPLICATES.xlsx')
mx_summary<-openxlsx::read.xlsx(file)
df<-mx_summary
df$quart<-as.character(pub_quarter_names[df$quart])
df$modelName<-gsub('model','m',df$modelName)
killcols<-which(names(df) %in% c('id','sensitivity_threshold'))
df<-df[,-killcols]
pub_names<-c('quart','replicate','id',	'Cpresence',	'Cabsences',	'cor',	'AUC',	'κmax',	'ssens',	'omission',	'prevalence',	'sspec',	'sensitivity')
names(df)<-pub_names
openxlsx::write.xlsx(df, file.path(SUPPDIR,paste0('SUPP_',basename(file))),overwrite = T)

file<-file.path(MAXENTDIR,'SORP_MAXENT_MODEL_RESULTS_SUMMARY.xlsx')
mx_summary<-openxlsx::read.xlsx(file)
df<-mx_summary[,1:7]
df$quart<-as.character(pub_quarter_names[df$quart])
cols<-grep('.*_pretty',names(mx_summary))
df<-cbind(df, mx_summary[,cols])
names(df)<-gsub('_pretty','',names(df))
df$modelName<-gsub('model','m',df$modelName)
killcols<-which(names(df) %in% c('replicate','id','sensitivity_threshold'))
df<-df[,-killcols]
pub_names<-c('quart','id',	'Cpresence',	'Cabsences',	'cor',	'AUC',	'κmax',	'ssens',	'omission',	'prevalence',	'sspec',	'sensitivity')
names(df)<-pub_names
openxlsx::write.xlsx(df, file.path(SUPPDIR,'SUPP_SORP_MAXENT_MODEL_RESULTS_SUMMARY.xlsx'))


#### figures ####
files<-list.files(EXP_GFX_DIR, '._selected_SORP_spearman_rho_sq.png', full.names = T)
for (f in files){
  file.copy(f, file.path(SUPPDIR,paste0('SUPP_',basename(f))),overwrite = T)
}

files<-list.files(EXP_GFX_DIR, '.*SORP_pairs.*.png', full.names = T)
for (f in files){
  file.copy(f, file.path(SUPPDIR,paste0('SUPP_',basename(f))),overwrite = T)
}

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

files<-list.files(MAXENTMODELGFX, '.*_DIAGS_EVAL_.*.png', full.names = T)
for (f in files){
  if(grepl('.*_AUC.png',basename(f))){file.copy(f, file.path(PUBDIR,paste0('FIG_4_',basename(f))),overwrite = T)}
  if(grepl('.*_kappa.png',basename(f))){file.copy(f, file.path(PUBDIR,paste0('FIG_5_',basename(f))),overwrite = T)}
}

files<-list.files(path = MAXENTGFX, pattern='MAXENT_FINAL_DIAGS_.*.png', full.names = T)
i<-0
for (f in files){
  i<-i+1
  sub<-letters[i]
  file.copy(f, file.path(PUBDIR,paste0('FIG_6',sub,'_',basename(f))),overwrite = T)
}

#### redo violin plots for maxent diags ####
file<-file.path(MAXENTDIR,'SORP_MAXENT_MODEL_RESULTS_REPLICATES.xlsx')
def<-openxlsx::read.xlsx(file.path(FINALDIR,'SORP_MODEL_DEFINITION.xlsx'))
mx_res<-openxlsx::read.xlsx(file)
target_vars<-c('AUC','kappa')
df<-mx_res[,c(1:4,which(names(mx_res) %in% target_vars))]

fnames<-sqldf::sqldf('select * from df as a left join def as b on a.modelName = b.modelName;')
df$fancy_model<-paste0(df$modelName, ' - ', fnames$def)
df$quart<-as.character(pub_quarter_names[df$quart])

p<-ggplot(data=df,aes(x=modelName,y=AUC, fill=fancy_model))
p<-p+geom_violin()+MAINTHEME
p<-p+facet_wrap(.~quart)+labs(fill=NULL)
p<-p+theme(axis.text.x = element_text(angle = 90),legend.position = 'right',axis.title.x=element_blank())

png(file.path(PUBDIR,'FIG_4_AUC.png'),12000,5000,res=600)
print(p)
dev.off()

png(file.path(PUBDIR,'FIG_4_AUC_small.png'),6000,2500,res=300)
print(p)
dev.off()

p<-ggplot(data=df,aes(x=modelName,y=kappa, fill=fancy_model))
p<-p+geom_violin()+MAINTHEME
p<-p+facet_wrap(.~quart)+labs(fill=NULL)
p<-p+theme(axis.text.x = element_text(angle = 90),legend.position = 'right',axis.title.x=element_blank())

png(file.path(PUBDIR,'FIG_5_KAPPA.png'),12000,5000,res=600)
print(p)
dev.off()

png(file.path(PUBDIR,'FIG_5_KAPPA_small.png'),6000,2500,res=300)
print(p)
dev.off()

#### snail plots ####
files<-list.files(FINALGFXDIR, 'SORP_RFGLS_SNAIL_ACCURACY_QUART_..png', full.names = T)
for (f in files){
  file.copy(f, file.path(SUPPDIR,paste0('SUPP_',basename(f))),overwrite = T)
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

png(file.path(PUBDIR,'FIG_2_SORP_SUMMARY_MONTHS.png'),8000,6000,res=600)
print(p)
dev.off()

png(file.path(PUBDIR,'FIG_2_SORP_SUMMARY_MONTHS_small.png'),4000,3000,res=300)
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

png(file.path(PUBDIR,'FIG_1_SORP_SUMMARY_YEAR.png'),12000,6000,res=600)
print(p)
dev.off()

png(file.path(PUBDIR,'FIG_1_SORP_SUMMARY_YEAR_small.png'),6000,3000,res=300)
print(p)
dev.off()

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