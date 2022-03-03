#### ensure required packages are available ####

.requiredPackages<-c('ape','devtools','dismo','rJava', "tree","randomForest", "fmsb","sp","raster","rgdal",
                    'pROC','separationplot','reshape','ggplot2','MASS','rasterVis',"ggnewscale","GGally",
					"ggpubr","gridExtra","reshape2","ggridges","dplyr","scales","Hmisc","RandomForestsGLS")

for (pkg in .requiredPackages){
  if (! (pkg %in% installed.packages()) ) {
	print(paste0('Need to install package: ',pkg))
    install.packages(pkg,dep=T)
  }
}

#obsolete
# if (! ("randomForestExplainer" %in% installed.packages()) ) {
  # devtools::install_github("MI2DataLab/randomForestExplainer")
# }

getCitation<-function(BIB=F){
  out<-list()
  for (pkg in .requiredPackages){
    x<-citation(pkg)
    if(BIB){out[[pkg]]<-as.character(toBibtex(x))}
    else{out[[pkg]]<-as.character(x$textVersion)}
  }
  return(out)
}

