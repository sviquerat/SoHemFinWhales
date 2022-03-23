require(ggplot2)
require(ggpubr)
require(reshape2)

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