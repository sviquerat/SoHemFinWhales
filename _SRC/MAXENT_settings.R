require(dismo)
require(rJava)

print('loading functions associated with MAXENT...')
source(file.path(SCRIPTFUNCDIR,'MAXENT_functions.R'))

# setup JAVA for R
MAXENT_FILE = 'C:/Users/bax3447/Google Drive/maxent/maxent.jar'
JAVA_RE <- 'C:/Program Files/Java/jre1.8.0_251' # maybe replace with 'get latest java'?
Sys.setenv(JAVA_HOME=JAVA_RE) # for 64-bit version
file.copy(MAXENT_FILE, system.file("java", package="dismo"),overwrite = T)

#https://groups.google.com/g/maxent/c/yRBlvZ1_9rQ
MAXENTargs = c("jackknife=false","replicates=100","writeplotdata=false","writebackgroundpredictions=false", 
               'plots=false', 'replicatetype=Bootstrap')
MAXENT_sensitivity = .9
