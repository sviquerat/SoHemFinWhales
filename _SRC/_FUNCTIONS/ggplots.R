require(ggplot2)
require(ggpubr)
require(reshape2)
#require(ggnewscale)
#require(GGally)
#require(gridExtra)
#require(ggridges)
#require(dplyr)

MAINTHEME<-ggpubr::theme_pubclean()+theme(text = element_text(size=15))

reorder_mat <- function(mat){
  # Use correlation between variables as distance
  dd <- as.dist((1-mat)/2)
  hc <- hclust(dd)
  mat <-mat[hc$order, hc$order]
  return(mat)
}

ggCorrespondence<-function(table,xlab='rows',ylab='cols',PROPORTION=F){
  melted<-reshape2::melt(table,na.rm=T)
  melted$N<-melted$value
  melted$correspondence<-melted$value
  ySums<-sqldf::sqldf('select Var1,sum(value) as total from melted group by Var1')
  xSums<-sqldf::sqldf('select Var2,sum(value) as total from melted group by Var2')
  for (cat in unique(melted$Var1)){
    melted$total[melted$Var1==cat]<-ySums$total[ySums$Var1==cat]
  }
  melted$p<-0
  melted$p[melted$total>0]<-melted$value[melted$total>0]/melted$total[melted$total>0]
  melted$correspondence<-melted$p
  if (PROPORTION){
    melted$value<-melted$p
  }
  p<-ggplot(data = melted, aes(Var2, Var1, fill = value))
  p<-p+geom_tile(color = "white")
  p<-p+scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(0,1), space = "Lab", name="Correspondence") 
  p<-p+theme_minimal()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
  p<-p+coord_fixed()
  if (PROPORTION){
    p<-p+geom_text(aes(Var2, Var1, label = paste0(N,'\n',round(value,4)*100,'%')), color = "black", size = 4)
  }else{
    p<-p+geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
  }
  p<-p+xlab(xlab)+ylab(ylab)
  p<-p+MAINTHEME
  return(p)
}

ggHeatmap<-function(cormat,legend_name='Heatmap',lower_limit=min(cormat,na.rm=T),upper_limit=max(cormat,na.rm=T),
                    mid_point=lower_limit+((upper_limit-lower_limit)/2),removeTri=T,force_symmetry=T,low_col='blue',mid_col='white',high_col='red'){
  require(reshape2)
  upper_limit=upper_limit
  lower_limit=lower_limit
  mid_point=mid_point
  if (force_symmetry){
    lower_limit<- -upper_limit
    mid_point<-0
  }
  
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
    return(cormat)
  }
  
  #cormat <- reorder_mat(cormat)
  if (removeTri){
    cormat <- get_upper_tri(cormat)
  }
  melted<-reshape2::melt(cormat,na.rm=T)
  melted$value<-round(melted$value,3)
  p<-ggplot(data = melted, aes(Var2, Var1, fill = value))
  p<-p+geom_tile(color = "white")
  p<-p+scale_fill_gradient2(low = low_col, high = high_col, mid = mid_col, midpoint = mid_point, 
                            limit = c(lower_limit,upper_limit), space = "Lab", name=legend_name)
  p<-p+theme_minimal()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
  p<-p+coord_fixed()
  p<-p+geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
  p<-p+MAINTHEME + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
  p<-p+xlab('')+ylab('')
  p<-p+guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5, angle=45))
  return(p)
}

ggViolin<-function(data,category, response, group=category, fill = category, WRAP='~.',...){
  p<-ggpubr::ggviolin(data, category, response,group=group,fill=fill, add = "none",draw_quantiles = seq(0.25,0.75,.25))
  p<-p+facet_wrap(as.formula(WRAP))
  p<-p+MAINTHEME
  return(p)
}

# ridgePlot<-function(DATA, xName,yName,xLevels, targetColour,fancyName,quantileBreaks=4){
  # DATA$p<-DATA[[yName]]
  # DATA$X<-ordered(DATA[[xName]],levels=rev(xLevels))
  # cols <- colorRampPalette(c('white',targetColour))(quantileBreaks)
  
  # p <- ggplot(DATA, aes(x = p, y = X, fill = factor(stat(quantile))))
  # p <- p +  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T)
  # p <- p + scale_fill_manual(values = cols, name='Quartile',labels=paste0(seq(0,75,25), ' - ',seq(25,100,25),'%'))
  # p <- p + xlab(fancyName) + ylab('')
  # return(p)
# }

# ggArrowplot<-function(scores, loadings, axes=1:2, axis_label='prop', axistype=2,offset=.15,outside_label=F,circle.col='black',...){
#   coords<-scores[,axes]
#   axisnames=colnames(scores[,axes])
#   vx <- colSums(loadings^2)
#   info_loadings<-rbind(`SS loadings` = vx,
#                        `Proportion Var` = vx/nrow(loadings),
#                        `Cumulative Var` = cumsum(vx/nrow(loadings)))
#   xName<-xAxisName<-axisnames[1] 
#   yName<-yAxisName<-axisnames[2]
#   
#   if(!is.na(axis_label)){
#     if (axis_label=='prop'){
#       xAxisName<-paste0(xName, ' (', round(info_loadings[2,1],2)*100,'%)')
#       yAxisName<-paste0(yName, ' (', round(info_loadings[2,2],2)*100,'%)')
#     }
#     if (axis_label=='cum'){
#       xAxisName<-paste0(xName, ' (cumSum: ', round(info_loadings[3,1],2)*100,'%)')
#       yAxisName<-paste0(yName, ' (cumSum: ', round(info_loadings[3,2],2)*100,'%)')
#       
#     }
#     if (axis_label=='SS'){
#       xAxisName<-paste0(xName, ' (SS: ', round(info_loadings[1,1],2),')')
#       yAxisName<-paste0(yName, ' (SS: ', round(info_loadings[1,2],2),')')
#     }
#   }
#   
#   n<-360
#   pts.circle <- as.data.frame(t(sapply(1:n,function(r)c(cos(2*r*pi/n),sin(2*r*pi/n)))))
#   names(pts.circle)<-c('x','y')
#   
#   ld<-as.data.frame(loadings[,1:ncol(loadings)])
#   ld$var<-rownames(loadings)
#   ld$x_end=loadings[,1]
#   ld$y_end=loadings[,2]
#   ld$x<-ld$y<-0
#   ld$angle<-NA
#   ld$angle[ld$y_end==0]<-90
#   ld$angle[ld$x_end==0]<-0
#   ld$angle[is.na(ld$angle)]<-atan(ld$y_end[is.na(ld$angle)]/ld$x_end[is.na(ld$angle)])*180/pi
#   
#   x0 <- min(c(0,ld$x_end))-offset
#   y0 <- min(c(0,ld$y_end))-offset
#   x1 <- max(c(ld$x_end,1))+offset
#   y1 <- max(c(ld$y_end,1))+offset
#   
#   p<-ggplot(data=pts.circle)+geom_point(aes(x=x,y=y),size=1,alpha=.4,col=circle.col,shape=16)
#   p<-p+geom_hline(yintercept=0,linetype=axistype) + geom_vline(xintercept=0,linetype=axistype)
#   p<- p + geom_segment(data=ld,aes(x = x, y = y, xend = x_end, yend = y_end),size=1,arrow = arrow(length = unit(6,'pt')))
#   if (outside_label){
#     ld$circle_x<-1*cos(ld$angle*pi/180)
#     ld$circle_y<-1*sin(ld$angle*pi/180)
#     ld$end_x_extra<-2
#     ld$end_y_extra<-ld$end_x_extra*ld$y_end/ld$x_end
#     p <- p + geom_segment(data=ld,aes(x = x, y = y, xend = end_x_extra, yend = end_y_extra,color=var),size=1,alpha=.3)
#     p<-p+guides(color=F)
#     p<-p+geom_text(data=ld,aes(x=circle_x,y=circle_y,angle=angle,label=var),check_overlap=F,hjust=0,vjust=0)
#     offset<-1.5*offset
#     x0 <- min(c(0,ld$circle_x))-offset
#     y0 <- min(c(0,ld$circle_y))-offset
#     x1 <- max(c(ld$circle_x,1))+offset
#     y1 <- max(c(ld$circle_y,1))+offset
#     
#   }else{
#     p<-p+geom_text(data=ld,aes(x=x_end,y=y_end,angle=angle,label=var),check_overlap=F,hjust=0) 
#   }
#   p<-p+MAINTHEME
#   p<-p+theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
#   p <- p + coord_equal(xlim=c(x0,x1),ylim=c(y0,y1))
#   p<-p+xlab(xAxisName)+ylab(yAxisName)
#   return(p)
# }
# 
# ggDimPlot<-function(scores, loadings,classes, axes=1:2, axis_label=NA,point_shape=NULL, point_size=3, point_label = NULL,point_colour=NULL,hull=NULL,
#                     hull_alpha=.2, hull_outline=T,axistype=1,WRAP=NULL,COMBINED_PLOT=F,ADD_ARROWS=F,GUESS_CLUSTER=0,DRAW_POINTS=T,HULL_TYPE='SIMPLE_HULL',...){
#   #2d and 3d
#   coords<-scores[,axes]
#   axisnames=colnames(scores[,axes])
#   df<-cbind(classes,coords)
#   vx <- colSums(loadings^2)
#   info_loadings<-rbind(`SS loadings` = vx,
#                        `Proportion Var` = vx/nrow(loadings),
#                        `Cumulative Var` = cumsum(vx/nrow(loadings)))
#   xName<-xAxisName<-axisnames[1] 
#   yName<-yAxisName<-axisnames[2]
#   
#   df$X<-df[[xName]]
#   df$Y<-df[[yName]]
#   
#   if (GUESS_CLUSTER>1){
#     dist.matrix = dist(coords)
#     fit<-hclust(dist.matrix)
#     groups <- cutree(fit, k=GUESS_CLUSTER) # cut tree into 5 clusters
#     df$cluster<-as.factor(groups)
#     hull<-'cluster'
#   }
#   
#   if(!is.null(hull)){
#     hull_name<-hull
#     df$hull<-as.character(df[[hull_name]])
#     hull<-hull
#   }
#   
#   if(!is.null(point_shape)){
#     df[[point_shape]]<-as.factor(df[[point_shape]])
#   }
#   
#   if(!is.null(point_colour)){
#     df[[point_colour]]<-as.factor(df[[point_colour]])
#   }
#   
#   p<-ggplot(data=df,aes_string(x='X',y='Y'))
#   
#   
#   if(!is.null(hull) | GUESS_CLUSTER){
#     if (HULL_TYPE=='SIMPLE_HULL'){
#       #https://github.com/hrbrmstr/ggalt/blob/master/R/geom_encircle.r
#       require(ggalt)
#       p<-p+geom_encircle(data=df,aes_string(x='X',y='Y',fill=hull_name),linetype=0,alpha=hull_alpha,expand=0,s_shape=1)
#     }
#     if (HULL_TYPE=='ENCIRCLE'){
#       #https://github.com/hrbrmstr/ggalt/blob/master/R/geom_encircle.r
#       require(ggalt)
#       p<-p+geom_encircle(data=df,aes_string(x='X',y='Y',fill=hull_name),linetype=0,alpha=hull_alpha,spread=1)
#     }
#     if (HULL_TYPE=='GEOM_BAG'){
#       p<-p+geom_bag(data=df,aes_string(x='X',y='Y',fill=hull_name),linetype=0,bag_alpha=hull_alpha)
#     }else{
#       if (is.null(WRAP)){
#         hull_data <- df %>% group_by(hull) %>% slice(chull(X, Y))
#         hull_data[[hull_name]]<-as.factor(hull_data$hull)
#         p<-p+geom_polygon(data=hull_data,aes_string(fill=hull_name),alpha = hull_alpha) #,colour=hull_name
#         
#         if (hull_outline){
#           print('outline')
#           #p<-p+geom_polygon(data=hull_data,aes_string(colour=hull_name),alpha=0)
#         }
#       }
#     }
#   }
#   p<-p+new_scale_colour()
#   if (DRAW_POINTS){
#     p<-p+geom_point(size=point_size,aes_string(shape=point_shape,color=point_colour))
#     }
#   
#   if(!is.null(point_label)){
#     p<-p+geom_text(label=point_label)
#   }
#   if(!is.na(axis_label)){
#     
#     if (axis_label=='prop'){
#       xAxisName<-paste0(xName, ' (', round(info_loadings[2,1],2)*100,'%)')
#       yAxisName<-paste0(yName, ' (', round(info_loadings[2,2],2)*100,'%)')
#     }
#     if (axis_label=='cum'){
#       xAxisName<-paste0(xName, ' (cumSum: ', round(info_loadings[3,1],2)*100,'%)')
#       yAxisName<-paste0(yName, ' (cumSum: ', round(info_loadings[3,2],2)*100,'%)')
#       
#     }
#     if (axis_label=='SS'){
#       xAxisName<-paste0(xName, ' (SS: ', round(info_loadings[1,1],2),')')
#       yAxisName<-paste0(yName, ' (SS: ', round(info_loadings[1,2],2),')')
#     }
#   }
#   p <- p + coord_equal()
#   p<-p+xlab(xAxisName)+ylab(yAxisName)
#   p<-p+MAINTHEME
#   p<-p+geom_hline(yintercept=0, linetype=axistype,size=1)+geom_vline(xintercept=0, linetype=axistype,size=1)
#   p<-p+theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))
#   
#   if (COMBINED_PLOT){
#     loadings<-loadings[,axes]
#     ld<-as.data.frame(loadings[,1:ncol(loadings)])
#     ld$var<-rownames(loadings)
#     ld$x_end=loadings[,1]
#     ld$y_end=loadings[,2]
#     ld$x<-ld$y<-0
#     p<-p+geom_segment(data=ld,aes(x = x, y = y, xend = x_end, yend = y_end),colour='red',size=1,arrow = arrow(length = unit(6,'pt')))
#     a<-ggplot(data=df,aes_string(x='X',y='Y'))
#     
#     if (DRAW_POINTS){
#       a<-a+geom_point(alpha=.1)
#     }
#     
#     a<-a+geom_hline(yintercept=0) + geom_vline(xintercept=0)
#     a<- a + geom_segment(data=ld,aes(x = x, y = y, xend = x_end, yend = y_end, color=var),size=1,
#                          arrow = arrow(length = unit(6,'pt')))
#     a<-a+coord_cartesian(
#       xlim=c(min(c(ld$x,ld$x_end)),max(c(ld$ld$x,ld$x_end))),
#       ylim=c(min(c(ld$y,ld$y_end)),max(c(ld$y,ld$y_end))))
#     
#     lay <- rbind(c(1,2),c(1,3))
#     b<-ggAxisLoading(loadings,HOR=T)
#     return(grid.arrange(p,a,b,layout_matrix = lay))
#   }
#   
#   if (ADD_ARROWS){
#     #todo
#   }
#   
#   if (!is.null(WRAP)){
#     p<-p+facet_wrap(WRAP,labeller = label_value)
#     warning('Wrapping severely reduces data availability in each facet!')}
#   return(p)
# }
# 
# ggPairs<-function(data, columns=1:ncol(data), group=NULL, colour=group){
#   p<-GGally::ggpairs(data, columns=columns,aes_string(group=group, colour=colour))
#   p<-p+MAINTHEME
#   return(p)
# }
# 
# ggRidge<-function(data,category, response, group=category, fill = group, WRAP='~.',...){
#   p<-ggplot(data, aes_string(x = response, y = category, group = group,fill=fill)) + geom_density_ridges()
#   p<-p+MAINTHEME
#   p<-p+coord_cartesian(clip = "off")
#   p<-p+facet_wrap(as.formula(WRAP))
#   return(p)
# }
# 
# ggAxisLoading<-function(loading,axis=1:ncol(loading),cutoff=NULL,rounding=3,SORT=T,HOR=F){
#   loading<-loading[,axis]
#   if (!is.null(cutoff)){loading[loading>-cutoff & loading <cutoff]<-NA}
#   melted<-reshape2::melt(loading,na.rm=T)
#   melted$value<-round(melted$value,rounding)
#   p4<-ggplot(data = melted, aes(Var2, Var1, fill = value))
#   p4<-p4+geom_tile(color = "white")
#   p4<-p4+scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Axis loading") 
#   p4<-p4+theme_minimal()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
#   p4<-p4+coord_fixed()
#   p4<-p4+geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
#   p4<-p4+xlab(NULL)+ylab(NULL)
#   p4<-p4+MAINTHEME
#   if (HOR){p4<-p4+coord_flip()}
#   return(p4)
# }
# 
# ggScreePlot<-function(sdev){
#   screeData<-data.frame(x=1:length(sdev), PC=paste0('PC',1:length(sdev)),sdev=sdev)
#   p3<-ggplot(data=screeData, aes(x=x,y=sdev))
#   p3<-p3+geom_point() + geom_line(aes(x)) + xlab(NULL) + ylab('Standard deviation / Eigenvalue')
#   p3<-p3+MAINTHEME
#   p3<-p3 + scale_x_continuous(labels=screeData$PC,breaks=screeData$x)
#   return(p3)
# }
# 
# ggBox<-function(data, category, response, group=category, fill = category, WRAP=~.){
#   p<-ggplot(data=data,aes_string(x=category,y=response, group=group,fill=fill))
#   p<-p+geom_boxplot()
#   p<-p+facet_wrap(as.formula(WRAP))
#   p<-p+MAINTHEME
#   return(p)
# }

# ggStripchart<-function(data,category, response, group=category, fill = category, fun=NA,global.fun=F,  WRAP=~.){
#   p<-ggstripchart(data=data, x=category,y=response,color=group,shape=1)
#   if(!is.na(fun)){
#     p<-p+geom_crossbar(color='black',stat= 'summary', fun = fun, fatten=1.5, width=0.5, aes(ymax=..y..,ymin=..y..))
#   }
#   p<-p+facet_wrap(as.formula(WRAP))
#   p<-p+MAINTHEME
#   return(p)
# }

# ggBiPlot<-function(pca.data, classes, axis=1:2,...){
#   ggDimPlot(pca.data$x,pca.data$rotation,classes = classes,...)
# }
# 
# ggFaPlot<-function(fa.rot, classes,axis_label='prop',axistype=3,...){
#   ggDimPlot(fa.rot$scores,fa.rot$loadings,classes = classes,axis_label=axis_label,axistype=axistype,...)
# }

