#### projections ####
WGS84<-sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0')
ANT_POL_STEREO<-sp::CRS('+init=epsg:3031')
IBCSO<-sp::CRS('+proj=stere +ellps=WGS84 +lat_0=-90 +lon_0=0dE +lat_ts=-65') # EPSG:9354 not yet implemented in most libraries

#### data settings ####
MONTHS<-list(1,2,3,4,5:11,12)
names(MONTHS)<-c("Jan","Feb","Mar","Apr","May-Nov","Dec")
RASTERRES<-10000

quarts<-list('A'=c(1,2,3),'B'=c(4,5,6),'C'=c(7,8,9),'D'=c(10,11,12))

#### class description ####
classVars<-list('None','Low (1 to <5 individuals)','Medium (5 to <10 individuals)','High (>10 individuals)')
fancyClasses<-list(expression(hat(p)[none]),expression(hat(p)[low]),expression(hat(p)[medium]),expression(hat(p)[high]))
classRange<-list( none=function(){return(0)},low=function(){return (1:4)},medium=function(){return (5:9)},high=function(){return (10:100)})
names(classVars)<-names(classRange)

#### spatial extents ####
EXTENT_WAP_ISLAND<-list(xlim=c(-2.8E6,-2.5E6),ylim=c(1.4E6,1.95E6))
EXTENT_WAP_ISLAND<-raster::extent(EXTENT_WAP_ISLAND$xlim[1],EXTENT_WAP_ISLAND$xlim[2],EXTENT_WAP_ISLAND$ylim[1],EXTENT_WAP_ISLAND$ylim[2])
EXTENT_SG<-list(xlim=c(-2.6E6,-2.2E6),ylim=c(3.05E6,3.35E6))
EXTENT_SG<-raster::extent(EXTENT_SG$xlim[1],EXTENT_SG$xlim[2],EXTENT_SG$ylim[1],EXTENT_SG$ylim[2])

#### RESULTS ####
RESPONSES<-c("prob_Q50","aboveThreshold", "I_soft_Q50",'I_Q50')
fancyRESPONSES<-list(expression(hat(p)[occurence]),
                     expression(hat(p)["above Threshold"]),expression(hat(I)[predicted]),expression(hat(I)[RFGLS]))
names(fancyRESPONSES)<-RESPONSES

#### colour palettes ####
occurencePalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))

resp.colours<-list(colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral")),
              colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral")),colorRampPalette(RColorBrewer::brewer.pal(9, "Spectral")),
              colorRampPalette(RColorBrewer::brewer.pal(10, "PiYG")))
names(resp.colours)<-RESPONSES

cls.colours<-list(colorRampPalette(RColorBrewer::brewer.pal(10, "PuOr")),colorRampPalette(RColorBrewer::brewer.pal(10, "PiYG")),
                   colorRampPalette(RColorBrewer::brewer.pal(10, "BrBG")),colorRampPalette(RColorBrewer::brewer.pal(10, "RdGy")))
names(cls.colours)<-names(classVars)
areaPalette <- colorRampPalette(RColorBrewer::brewer.pal(6, "Dark2"))

# probably obsolete
# colSamples<-hcl.colors(15,palette='TealRose')
# colPred<-hcl.colors(25,palette='Emrld')
# colWhale<-hcl.colors(25,palette='Lajolla')
# colSSIM<-hcl.colors(25,palette='Lajolla')
# coldepth<-rev(hcl.colors(25,palette='Blues'))
# ggSSIMPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))

# depthPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))

#### discrete colours ####
# probably obsolete
# classColours <- c('#2b83ba','#fdae61','#abdda4','#d7191c')
# names(classColours) <- names(classVars) <- names(fancyClasses) <- names(classRange) <- c('none','low','medium','high')
# col.survey<-'#B2D3A8'