polar_coordinates<-function(r,angle,IS_DEG=T){
  theta<-angle
  if (IS_DEG){
    theta<-angle*(2*pi)/360
  }
  out<-data.frame(l=rep(r,length(angle)),theta=theta,x=NA,y=NA)
  for (th in theta){
    out$x[out$theta==th & out$l == r]<-r*cos(th)
    out$y[out$theta==th & out$l == r]<-r*sin(th) 
  }
  return(out)
}

draw_circular_grid<-function(grid_radii=seq(0,1,.25),grid_angles=seq(0,360,45),grid_spike_length=20*max(grid_radii),...){
  for (r in grid_radii){
    plotrix::draw.circle(0,0,r,...)
  }
  grid<-polar_coordinates(grid_spike_length,grid_angles,...)
  for (spike in 1:nrow(grid)){
    lines(c(0,grid$y[spike])~c(0,grid$x[spike]),...)
  }
}

draw_circle<-function(radius=1,x=0,y=0,border=adjustcolor('grey',.75),col=NA,...){
  plotrix::draw.circle(x,y,radius,border=border,col=col,...)
}

draw_snail<-function(y,type='p',col=adjustcolor('grey',.75),pch=16,color_ramp = NULL,...){
  polar<-create_snail_data(y)
  if (is.null(color_ramp)){
    colors=rep(col,nrow(polar))
  }
  else{
    colors=color_ramp(nrow(polar))
  }
  if (type=='areas'){
    polygons<-poly_area(polar$x,polar$y)
    for (i in 1:length(polygons)){
      polygon(polygons[[i]],col=colors[i],border=NA)
    }
  }
  if(type=='p'){
    for (i in 1:nrow(polar)){
      points(polar$y[i]~polar$x[i],pch=pch,col=colors[i])
    }
  }
  if(type=='l'){ #line plot
    for (i in 1:nrow(polar)){
      lines(c(0,polar$y[i])~c(0,polar$x[i]),col=colors[i])
    }
  }
  if(type=='o'){ #outline only
    lines(c(0,polar$y)~c(0,polar$x),col=col)
  }
  if(type=='poly'){ #polygon
    polygon(x=c(0,polar$x),y=c(0,polar$y),col=col,border=NA)
  }
}

create_snail_data<-function(y){
  y<-y[!is.na(y)]
  step_size<-2*pi/length(y)
  theta<-seq(0,2*pi,step_size)
  theta<-theta[1:(length(theta)-1)]
  polar<-data.frame(ID=1:length(y), theta=NA, r=y)
  polar<-polar[order(-polar$r),]
  polar$theta<-theta
  df<-data.frame()
  for (i in 1:nrow(polar)){
    newline<-cbind(polar[i,],polar_coordinates(polar$r[i],polar$theta[i], IS_DEG=F))
    df<-rbind(df,newline)
  }
  return(df)
}

get_radius<-function(x1,y1,x0=0,y0=0){
  return(sqrt(abs(x1-x0)^2+abs(y1-y0)^2))
}

poly_area<-function(x,y){
  polygons<-list()
  for (i in 2:length(x)){
    poly<-list()
    poly$x<-c(0,x[i-1],x[i],0)
    poly$y<-c(0,y[i-1],y[i],0)
    polygons[[i]]<-poly
  }
  return(polygons)
}

polar_area<-function(x,y, comp_radius=1,method='length'){
  if (method=='length'){
    #this is equivalent to average of value
    area<-sum(get_radius(x,y))
    area_comp<-length(x)*comp_radius
  }
  if(method=='area'){
    area<-pracma::polyarea(x,y)
    area_comp<-pi*comp_radius
  }
  return(
    list(ratio=area/area_comp,area_data=area, area_comparison=area_comp, comparison_radius = comp_radius,method=method)
  )
}

snailplot<-function(y,type='poly',scale_y=1,grid_radii=NULL,limits=NULL){
  par(pty="s")
  
  if (is.null(limits)){
    limits<-max(y,na.rm=T)
  }
  
  if (is.null(grid_radii)){
    grid_radii = quantile(y,na.rm=T)
  }
  
  #range<-max(y,na.rm=T)*scale_y
  #range<-limits*scale_y
  range<-limits
  plot(1,1,type='n',ann=F, xlim=c(-range,range),ylim=c(-range,range))
  draw_circular_grid(grid_radii=grid_radii)
  draw_snail(y, type=type)
}
