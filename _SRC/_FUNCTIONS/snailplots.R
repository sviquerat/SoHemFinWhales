range01 <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

inv_range01 <- function(scaled_x,max_x,min_x=0){
  return( scaled_x*(max_x-min_x) + min_x )
}

polarcoordinate<-function(input,phi,SCALE=T){
  value<-input
  rad <- phi * pi /180
  if (SCALE) {value<-range01(value)}
  x<-value*cos(rad)
  y<-value*sin(rad)
  return(list(x=x,y=y,degs=phi,rad=rad,radii=value,input=input))
}

plotCircle<-function(radii=seq(0,.75,.25),angularres=5,...){
  phi<-seq(0,360,angularres)
  for (r in radii){
    polygon(polarcoordinate(r,phi,SCALE=F),...)
  }
}

addLabels<-function(radii=seq(.25,.75,.25),angle=c(90,180),labels=NULL,...){
  for (r in radii){
    text(polarcoordinate(r,angle,SCALE=F),labels=r,...)
  }
}

addreCastLabels<-function(radii=seq(.25,.75,.25),angle=c(90,180),labels=NULL,...){
  for (i in 1:length(radii)){
    r<-radii[i]
    label<-labels[i]
    text(polarcoordinate(r,angle,SCALE=F),labels=label,...)
  }
}


plotCoordinateSystem<-function(){
  plotCircle(col=adjustcolor('blue',alpha=.2),border=NA)
  plotCircle(1)
  plotDiagonals(lty=2)
}

plotDiagonals<-function(...){
  plotLine(225,45,...)
  plotLine(135,315,...)
}

plotInternal<-function(...){
  plotLine(180,0,...)
  plotLine(270,90,...)
}

plotLine<-function(angle1,angle2,...){
  lines(polarcoordinate(1,c(angle1,angle2),SCALE=F),...)
}

snailPlot<-function(y,r=max(y),...){
  y<-y[!is.na(y)]
  y<-sort(y)
  res<-360 / length(y) 
  degs<-res*seq(1,length(y),1)
  coords<-polarcoordinate(y,degs)
  par(xaxs='i',yaxs='i'); ## "internal" axis spacing, meaning no extended range, and slightly adjust margins
  plot(NA,xlim=c(-1,1),ylim=c(-1,1),pch=4,asp=1,ann=F,...)
  plotCoordinateSystem()
  polygon(c(0,coords$x),c(0,coords$y),border=NA,col=adjustcolor('orange',alpha=.8))
  plotInternal(col='red',lty=2,lwd=1.5)
  addLabels()
  castFromTo(coords$radii,coords$input)
  totalArea<-sum(rep(1,length(coords$radii)))
  coveredArea<-totalArea-sum(coords$radii)
  axis(1)
  title(sub=paste0(round(coveredArea/totalArea,2)*100,'% covered'))
}

hillPlot<-function(y,r=max(y)){
  y<-y[!is.na(y)]
  y<-sort(y)
  y_<-range01(y)
  plot(y_,ylim=c(0,1),ann=F,type='n')
  polygon(c(1:length(y_),rev(1:length(y))),c(y_,rep(0,length(y_))),border=NA,col=adjustcolor('orange',alpha=.8))
  totalArea<-length(y_)
  coveredArea<-totalArea-sum(y_)
  title(sub=paste0(round(coveredArea/totalArea,2)*100,'% covered'))
}