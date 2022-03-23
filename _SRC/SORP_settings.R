rm(list=ls()) # clean workspace
set.seed(666) # set random number seed so results are consistent

hex_spacing = 20000 #spacing between hex centroids (~ grid spacing)

pub_quarter_names<-list(A='Q1',B='Q2',C='Q3',D='Q4') #quarter names as they appear in the publication

get_pub_quarter_names<-function(quarter_vector){
  out<-c()
  for (i in 1:length(quarter_vector)){
    out<-c(out, pub_quarter_names[[quarter_vector[i]]])
  }
  return(out)
}

print('Setting up environment vars...')
source(file.path(getwd(),'_SRC','SORP_folders_filenames.R')) # set up folders and filenames

print('Checking required packages...')
source(file.path(SCRIPTDIR,'SORP_package_check.R'))

print('Setting up environment data...')
source(file.path(SCRIPTDIR,'SORP_data.R')) # needs tweaking

print('Loading functions...')
source(file.path(SCRIPTFUNCDIR,'functions.R'))
source(file.path(SCRIPTFUNCDIR,'betaFunctions.R'))
source(file.path(SCRIPTFUNCDIR,'spatial_functions.R'))
source(file.path(SCRIPTFUNCDIR,'ggplots.R'))
source(file.path(SCRIPTFUNCDIR,'resp_plot.R'))

print('All done!')