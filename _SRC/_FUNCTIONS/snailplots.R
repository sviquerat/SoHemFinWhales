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

draw_snail<-function(y,type='p',col=adjustcolor('grey',.75),pch=16,color_ramp = NULL,...){
  polar<-create_snail_data(y)
  if (is.null(color_ramp)){
    colors=rep(col,nrow(polar))
  }
  else{
    colors=color_ramp(nrow(polar))
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

polar_area<-function(x,y, comp_radius=1){
  area<-pracma::polyarea(x,y)
  area_comp<-pi*comp_radius
  return(
    list(ratio=area/area_comp,area_data=area, area_comparison=area_comp, comparison_radius = comp_radius)
  )
}

snailplot<-function(y,type='poly',scale_y=1,grid_radii=quantile(y,na.rm=T)){
  par(pty="s")
  range<-max(y,na.rm=T)*scale_y
  plot(1,1,type='n',ann=F, xlim=c(-range,range),ylim=c(-range,range))
  draw_circular_grid(grid_radii=grid_radii)
  draw_snail(y, type=type)
}
