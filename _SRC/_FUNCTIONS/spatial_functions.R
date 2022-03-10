require(sp)
require(scales)

polyColourPlot<-function(SPPDF,columnName,myColorRampPalette = colorRampPalette(c("#f7f6fd","#4635d0")), alphaValue=1, NAcol = NA, border = 'light gray', lwd=.5,prettyLevels=5,...){
  values<-as.numeric(SPPDF[[columnName]])
  color.match = myColorRampPalette(length(unique(values)))
  lookupTable = sort(unique(values))
  colour = scales::alpha(color.match[match(values, lookupTable)],alphaValue)
  colour[is.na(colour)]<-NAcol
  out<-data.frame(value=values,colour=colour)
  out<-subset(out,!is.na(out$value))
  out<-out[!duplicated(out$colour),]
  out<-out[order(out$value),]
  plot(SPPDF, col=colour, border=border, lwd=0.5, ...)
  return(out)
}

points2polygons <- function(SPpoints,name, proj4string) {
  p<-Polygon(LL@coords)
  p<-Polygons(list(p),name)
  p<-SpatialPolygons(list(p),proj4string = proj4string)
  data<-data.frame(Name=name)
  row.names(data)<-names(p)
  p<-SpatialPolygonsDataFrame(p, data)
  return (p)
}
