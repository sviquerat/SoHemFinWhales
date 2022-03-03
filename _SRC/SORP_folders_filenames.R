#### General Directories ####
PROJECTDIR<-getwd() #the project directory
SCRIPTDIR<-file.path(PROJECTDIR,'_SRC') # the folder containing all the R scripts providing settings (and functions)
SCRIPTFUNCDIR<-file.path(SCRIPTDIR,'_FUNCTIONS') # folder containing all functions

DATDIR<-file.path(PROJECTDIR,'DATA') # folder containing all data
MERGEDDIR<-file.path(DATDIR,'SOURCES','_MERGED_DATA') # folder containing all provided and merged records of absence / presence
SRCTRUNCDIR<-file.path(MERGEDDIR,'SHAPES') # spatial files of records
PRESENCEDATA<-file.path(MERGEDDIR,'SORP_DATA_PRESENCE.RData')
ABSENCEDATA<-file.path(MERGEDDIR,'SORP_DATA_ABSENCE.RData')

SHPDIR<-file.path(DATDIR,'_SHAPES') # various shape files used throughout the analysis
STATICSHPDIR<-file.path(SHPDIR,'STATIC') # static shape files (here: antarctic coastline)
DEPTHDIR<-file.path(SHPDIR,'DEPTHS') #Folder containing IBCSO 2 raster
ENVDIR<-file.path(SHPDIR,'ENV') # folder containing environmental data
ENV_CHLA_DIR<-file.path(ENVDIR,'CHLA','CROPPED') # cropped chl-a data
ENV_SST_DIR<-file.path(ENVDIR,'SST','CROPPED') # cropped sst data

RESDIR<-file.path(PROJECTDIR,'RESULTS') # results folder (both temporary & final)
FINALDIR<-file.path(RESDIR,'_FINAL')
FINALGFXDIR<-file.path(FINALDIR,'GFX')
FINALAREADIR<-file.path(FINALGFXDIR,'AREAS')
FINALSPDIR<-file.path(FINALDIR,'SPATIAL')
FINAL_SP_AREA_DIR<-file.path(FINALSPDIR,'AREAS')
FINALDATADIR<-file.path(FINALDIR,'DATA')

DIAGDIR<-file.path(FINALGFXDIR,'DIAGNOSTICS')
DETAILDIR<-file.path(FINALGFXDIR,'DETAIL')

# dynamic
EXPDIR<-file.path(RESDIR,'_EXPLORATION')
EXP_GFX_DIR<-file.path(EXPDIR,'GFX')
RESDATDIR<-file.path(RESDIR,'DATA')
RESSPDIR<-file.path(RESDATDIR,'SPATIAL')

dir.create(RESDATDIR,recursive = T, showWarnings = F)
dir.create(RESSPDIR,recursive = T, showWarnings = F)
dir.create(EXPDIR,recursive = T, showWarnings = F)
dir.create(EXP_GFX_DIR,recursive = T, showWarnings = F)
dir.create(RESDATDIR,recursive = T, showWarnings = F)
dir.create(FINALGFXDIR,recursive = T, showWarnings = F)
dir.create(FINALSPDIR,recursive = T, showWarnings = F)
dir.create(FINALDATADIR,recursive = T, showWarnings = F)
dir.create(FINALAREADIR, showWarnings = F)
dir.create(FINAL_SP_AREA_DIR, showWarnings = F)
dir.create(DIAGDIR, showWarnings = F)
dir.create(DETAILDIR, showWarnings = F)

#### Static Data Filenames ####
files_TARGETAREAS <- file.path(SHPDIR,'TARGET_AREAS_EPSG9354.gpkg')
# files_ENV<-list.files(ENVDIR,pattern='*.asc$',full.names = T)
files_IBCSO<-list.files(DEPTHDIR,pattern='IBCSO.*.tif$',full.names = T) # IBCSO 2 depth raster up to 50°S
files_SURVEYAREA<-file.path(MERGEDDIR,'SHAPES','SORP_BOUNDARY_100K_BUFFER.gpkg')
files_WORLD <- file.path(SHPDIR,'STATIC','ne_50m_admin_0_countries.gpkg')

#### final data filenames ####
PRED_GRID<-file.path(RESDATDIR,'SORP_predGrid.RData')
SAMPLEDATA1977<-file.path(RESDATDIR,'SORP_sampleData1977.RData')
SAMPLEGRID<-file.path(RESDATDIR,'SORP_sampleData_grid1977.RData')
COVARS<-file.path(RESDATDIR,'SORP_covars.RData')
FINAL_COVARS<-file.path(RESDATDIR,'SORP_covars_final.RData')
COVAR_STACKS<-file.path(RESDATDIR,'SORP_covar_stacks.RData')
FINAL_PREDICTION_RESULTS<-file.path(FINALDATADIR,'SORP_prediction_results.RData')

#### MAXENT BASE DIRS
MAXENTDIR<-file.path(RESDIR,'MAXENT')
MAXENT_MODELS<-file.path(MAXENTDIR,'MAXENT_model_data.RData')
MAXENT_MODEL_SELECTION<-file.path(FINALDATADIR,'SORP_MAXENT_MODELS_FINAL.RData')
#MAXENT_PRED<-file.path(FINALDATADIR,'SORP_MAXENT_PREDICTIONS.RData')
MAXENT_RASTER_PREDICTION<-file.path(FINALDATADIR,'SORP_MAXENT_PREDICTIONS_RASTER.RData')

#### RFGLS BASE DIRS
RFGLS_VALIDATION_DATA<-file.path(FINALDATADIR,'SORP_RF_validation.RData')
RFGLS_MODELS<-file.path(FINALDATADIR,'SORP_RF_model_data.RData')
RFGLS_RASTER_PREDICTION<-file.path(FINALDATADIR,'SORP_RFGLS_PREDICTIONS_RASTER.RData')
