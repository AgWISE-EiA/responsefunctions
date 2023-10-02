

#################################################################################################################
# 1. Sourcing required packages 
#################################################################################################################
#################################################################################################################
packages_required <- c("doParallel", "foreach", "h2o", "tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics", "lme4",
                       "MuMIn")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))



#################################################################################################################
# 2.  helper functions 
#################################################################################################################
#################################################################################################################

#' Title read the train and test data, add AEZ on both and aggregate the test data across years 
#'
#' @param country 
#' @param useCaseName 
#' @param Crop 
#' @param weather if true it will merge whether data otherwise it returns only soil and DEM variables 
#' @param AOI area of interest or area for which we are developing the recommendation for
#' @param Planting_month_date the rest data weather data is to be defined by the planting and months and date
#'
#' @return
#'
#' @examples ML_dataPrepartion(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", Planting_month_date = "08-08")
ML_dataPrepartion <- function (country, useCaseName, Crop, weather = FALSE, AOI = FALSE, Planting_month_date = NULL){
  
  ###############################
  # 1. define path for input and output
  ###############################
  
  pathIn <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/data_4ML/", sep="")
  
  pathOut1 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  
  ###############################
  # 2. read data
  ###############################
  

  if(AOI == TRUE){
    Planting_month_date <- gsub("-", "_", Planting_month_date)
    ML_Data <- readRDS(paste(pathIn, "geoSpatial_4ML_AOI_", Planting_month_date, ".RDS", sep=""))
  }else{
    ML_Data <- readRDS(paste(pathIn, "geoSpatial_4ML_trial.RDS", sep=""))
  }
  
  
  fixedVars <- c("lon", "lat", "NAME_1", "NAME_2", "altitude", "slope", "TPI", "TRI",
                 "c_tot_top", "c_tot_bottom", "ca_top", "ca_bottom", "clay_tot_psa_top", "clay_tot_psa_bottom", 
                 "db_od_top", "db_od_bottom", "ecec_f_top", "ecec_f_bottom", "k_top", "k_bottom", "mg_top", "mg_bottom", 
                 "n_tot_ncs_top", "n_tot_ncs_bottom", "oc_top", "oc_bottom", "p_top", "p_bottom", "ph_h2o_top", "ph_h2o_bottom",
                 "s_top", "s_bottom", "sand_tot_psa_top", "sand_tot_psa_bottom", "silt_tot_psa_top", "silt_tot_psa_bottom", "SOM_top",
                 "SOM_bottom", "PWP_top", "PWP_bottom", "FC_top", "FC_bottom", "SWS_top", "SWS_bottom", "K_0_30", "N_0_30", "P_0_30", 
                 "Ptot_0_30", "texture_class_top", "texture_class_bottom")
  
  weatherVars <- c("longitude", "latitude", "plantingYear" ,"totalRF", "nrRainyDays", "Rain_month1", "Rain_month2", "Rain_month3", "Rain_month4", 
                   "Tmax_mean", "Tmax_month1", "Tmax_month2", "Tmax_month3", "Tmax_month4", 
                   "Tmin_mean", "Tmin_month1", "Tmin_month2", "Tmin_month3", "Tmin_month4")
  
   
  ###############################
  # 3. aggregate weather data across years 
  ###############################
  if(AOI ==TRUE){
    
    ML_Data_fixVars <- unique(ML_Data[, fixedVars])
    ML_Data_weatherVars <- ML_Data[, weatherVars]
    ## the aggregation will be done only for selected years based on 
    ## the season forecast: dry, wet or average years
    ML_Data_weatherVarsS <- suppressWarnings(as.data.frame(ML_Data_weatherVars %>% 
                                                                 group_by(longitude, latitude) %>%
                                                                 summarise_all(mean) %>%
                                                                 select(-plantingYear)))
    ML_Data <- unique(merge(ML_Data_fixVars, ML_Data_weatherVarsS, by=c("longitude", "latitude")))
  }
 
 if(AOI == FALSE){
   ML_Data <- ML_Data %>%
               mutate(longitude = as.numeric(lon),
                      latitude = as.numeric(lat))
 } 
  
  ###############################
  # 4. the AOI data should be filtered for the desired AEZ in RAB 
  ###############################
  AEZ <- readOGR(dsn="~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/Landing/AEZ",  layer="AEZ_DEM_Dissolve")
  RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
  RAW_AEZ_AOI <- suppressWarnings(raster::extract(RW_aez, ML_Data[, c("longitude", "latitude")]))
  ML_Data <- cbind(RAW_AEZ_AOI, ML_Data)
  ML_Data <- ML_Data %>%
    dplyr::select(-c(id.y, AEZs_no)) %>%
    mutate(AEZ = as.factor(Names_AEZs))
    
  return(ML_Data)
  
}



#' Title plot the variable importance
#'
#' @param modelML  the ML model
#' @param modelName the name of the model e.g. Random forest
#' @param pathOut1 is the path to save the plot
#'
#' @return it does not return anything but it will save the plot as pdf in pathOut1 
#' @export
#'
#' @examples
plotvarImp <- function(modelML, modelName, pathtosave){
  VarImp_model <- as.data.frame (h2o.varimp(modelML)) 
  VarImp_model$variable <- factor(VarImp_model$variable) %>%
    fct_reorder(VarImp_model$scaled_importance)
  
  VI_gg <- ggplot(VarImp_model[1:10,], aes(variable, scaled_importance))+
    geom_col(width = 0.5) +
    ggtitle(paste("Variable importance, ", modelName, sep=""))+
    xlab("") + ylab("")+
    coord_flip()+
    theme_minimal()+
    theme(axis.title = element_text(size=14), axis.text = element_text(size=12), 
          plot.title = element_text(hjust = 0.5, size=16), strip.text = element_text(size=14))
  ggsave(paste(pathtosave, "varImp_", modelName, ".pdf", sep=""), VI_gg, width=6, height = 6)
  
}
