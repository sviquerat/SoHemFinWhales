source('_SRC/SORP_settings.R')
library(sp)

load(PRESENCEDATA)
load(ABSENCEDATA)
load(PRED_GRID)

data<-rbind(data,data.absence)
data$dataSource<-data$source

#making sure we use the right projection
LL<-sp::SpatialPoints(cbind(data$lon,data$lat), WGS84)
LL<-sp::spTransform(LL,IBCSO)
data$x<-LL@coords[,1]
data$y<-LL@coords[,2]

data$d_x<-data$x
data$d_y<-data$y
data<-data[,-which(names(data) %in% c('x','y','source','weight'))]
LL<-SpatialPoints(cbind(data$d_x,data$d_y),IBCSO)

#### calculate centroid coordinates for each prediction grid cell
xy<-rgeos::gCentroid(predGrid,byid=TRUE)
predGrid$centroid_x<-xy@coords[,1]
predGrid$centroid_y<-xy@coords[,2]

res <- sp::over(LL,predGrid) # combine records with prediction grid data
data<-cbind(data,res)

data$chla<-NA
data$sst<-NA
for (yr in unique(data$year)){
  for (mn in unique(data$month)){
    rows<-which(data$year==yr & data$month==mn)
    if (length(rows)>0){
      pp<-LL[rows,]
      month_name<-month.abb[mn]
      files<-list.files(ENV_CHLA_DIR,paste0('CHL-A_',yr,'_',month_name,'.tif'),full.names=T,recursive=T)
      if(length(files)==0){
        print(paste0('No chla data found for month: ',month_name,' and year: ',yr))
        files<-list.files(ENV_CHLA_DIR,paste0('CHL-A_',month_name,'.tif'),full.names=T)
      }
      #check if empty, then check month before /after otherwise monthly average
      r<-raster::raster(files[1])
      pp<-sp::spTransform(pp,r@crs)
      chla<-raster::extract(r,pp)
      data$chla[rows]<-chla
      files<-list.files(ENV_SST_DIR,paste0('SST_',yr,'_',month_name,'.tif'),full.names=T,recursive=T)
      if(length(files)==0){
        print(paste0('No sst data found for month: ',month_name,' and year: ',yr))
        files<-list.files(ENV_SST_DIR,paste0('SST_',month_name,'.tif'),full.names=T)
      }
      r<-raster::raster(files[1])
      sst<-raster::extract(r,pp)
      data$sst[rows]<-sst
    }
  }
}

data_all_augmented<-data
data<-subset(data_all_augmented,!is.na(CellID)) #remove all records beyond pred grid boundary

data$quart<-'A'
data$quart[data$month %in% 4:6]<-'B'
data$quart[data$month %in% 7:9]<-'C'
data$quart[data$month %in% 10:12]<-'D'

# all data before being merged to cellids and quarter (after 1977)
sampleData<-sqldf::sqldf('select CellID, year, month,quart,count(*) as N, sum(binom) as G, sum(best) as I,median(d_x) as data_x, median(d_y) as data_y, median(sst) as sst, median(chla) as chla from "data" where year >= 1977 group by CellID, year, month')
sampleData<-merge(sampleData,predGrid@data,by='CellID',all.x=T)
#sampleData<-subset(sampleData,!is.na(CellID))
sampleData$binom<-0
sampleData$binom[sampleData$G>0]<-1
sampleData$gs<-sampleData$I/sampleData$G
sampleData$gs[sampleData$G==0]<-0
sampleData$dataType <- 'presence'
sampleData$dataType[sampleData$G==0] <- 'absence'
save(sampleData,file=SAMPLEDATA1977,compress='gzip')

# all data aggregated by cellid and quarter
data_grid<-sqldf::sqldf('select CellID, quart,count(*) as N, sum(binom) as G, sum(best) as I,median(d_x) as data_x, median(d_y) as data_y, centroid_x as cell_x, centroid_y as cell_y, median(sst) as sst, median(chla) as chla from "data" where year >= 1977 group by CellID, quart')
data_grid$p<-data_grid$G/data_grid$N
data_grid$binom<-0
data_grid$binom[data_grid$G>0]<-1
data_grid$gs<-data_grid$I/data_grid$G
data_grid$gs[data_grid$G==0]<-0
data_grid$dataType <- 'presence'
data_grid$dataType[data_grid$G==0] <- 'absence'
data_grid<-merge(data_grid,predGrid@data,by='CellID',all.x=T)
sample_grid<-data_grid

save(sample_grid,file=SAMPLEGRID,compress='gzip')

for (q in unique(data_grid$quart)){
  df<-subset(data_grid,quart==q)
  pg<-predGrid
  pdata<-pg@data
  sdata<-sqldf::sqldf('select * from pdata as a left join df as b on a.CellID = b.CellID')
  pg@data<-sdata
  pg<-subset(pg, !is.na(pg$binom))
  rgdal::writeOGR(pg,dsn=file.path(RESSPDIR,paste0('SORP_data_grid_',q,'.gpkg')),layer=paste0('SORP_data_grid_',q),driver='GPKG', overwrite_layer = T)
}
