require(tree)
require(randomForest)
require(RandomForestsGLS)
require(Hmisc)

print('loading functions associated with RFGLS...')
source(file.path(SCRIPTFUNCDIR,'RF_functions.R'))

# Settings for the RFGLS
RFGLS_ncores=2 # number of cores to use (only for openMP systems)
RFGLS_n_omp=3 #number of threads to use
RFGLS_ntrees=1000 # use that many trees for the forest
RFGLS_n.neighbors=18 # consider that many neighbours for the spatial structure
RFGLS_search.type='tree' # search algorithm to use
RFGLS_nrnodes=20 # maximum number of nodes allowed per tree

#### RFGLS directories ####
RFDIR<-file.path(RESDIR,'RF')
# RFDATDIR<-file.path(RFDIR,'DATA')
RFGFX<-file.path(RFDIR,'GFX')
# RFSHPDIR<-file.path(RFDIR,'SPATIAL')
RFMODELGFX<-file.path(RFGFX,'MODEL_DIAGNOSTICS')

dir.create(RFMODELGFX,recursive=T, showWarnings = F)
dir.create(RFGFX,recursive=T, showWarnings = F)
# dir.create(RFSHPDIR,recursive=T, showWarnings = F)
# dir.create(RFDATDIR,recursive=T, showWarnings = F)