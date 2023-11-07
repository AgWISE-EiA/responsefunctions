


### The AgWise modules data flow and scripts 

## 1. Field data from different sources is aggregated 
##### script: agwise-datacuration/dataops/datacuration/Scripts/useCases/UseCase_Rwanda_RAB/Potato/Compiling all potato fieldData Rwanda.
##### output data : agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RAB/Potato/transform/aggregated_fieldData.RDS"


## 2. linear mixed effects model is fitted and BLUPs are extracted
###### script: agwise-datacuration/dataops/datacuration/Scripts/useCases/UseCase_Rwanda_RAB/Potato/randomNoiseReduction_potato_RAB.R
###### output data in two locations as resulr for data cuaration module and as raw data for response function 
      ### agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RAB/Potato/result/compiled_fieldData.RDS"
      ### agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Potato/raw//compiled_fieldData.RDS


## 3. getting the secondary data from geo-spatial layers
###### script: 
      ### agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R
      ### agwise-datasourcing/dataops/datasourcing/Scripts/useCases/useCase_Rwanda_RAB/Potato/get_geoSpatialData_RAB_Potato.R
###### data: for crop models and machine learning application is saved in the rsult folder of this module and as raw data for response functions and potential yield modules
      ### agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/result/geo_4cropModel
      ### agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/result/geo_4ML

      ### agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Potato/raw/geo_4cropModel
      ### agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Potato/raw


## 4. running the crop models: this is not done for potato, we used yield response to the highest NPK rate +20% as yield ceiling. For the demo we can look at the maize work
####### script:   
    ### agwise-potentialyield/dataops/potentialyield/Script/useCases/UseCase_Rwanda_RAB/Maize/DSSAT/run_DSSAT_RAB_Maize.R
    ### agwise-potentialyield/dataops/potentialyield/Script/useCases/UseCase_Rwanda_RAB/Maize/DSSAT/get_CM_geo_RAB_Maize

####### data:
    ### agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Maize/Landing/DSSAT
    ### agwise-potentialyield/dataops/potentialyield/Data/useCase_Rwanda_RAB/Maize/result/DSSAT/Trial


## 5. modeling yield response to nutrients 
####### script 
    ### agwise-responsefunctions/dataops/responsefunctions/Scripts/generic
    ### agwise-responsefunctions/dataops/responsefunctions/Scripts/useCases/UseCase_Rwanda_RAB/Potato/agwise_QUEFTS_routine_Potato.R
 ## this last script is what is running below ...



#################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
packages_required <- c("plyr", "tidyverse", "ggplot2", "foreach","doParallel",
                       "limSolve", "lpSolve", "Rquefts", "rgdal", "randomForest","ranger","Metrics")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

#################################################################################################################
## set path and input counry crop and use case 
#################################################################################################################
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/agwise_ML_routine.R")



pathIn <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")

pathOut1 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")

if (!dir.exists(pathOut1)){
  dir.create(file.path(pathOut1), recursive = TRUE)
}

#################################################################################################################
## read the yield, soil, weather and AEZ data and merge
#################################################################################################################

## yield data
ds<- unique(readRDS(paste(pathIn, "compiled_fieldData.RDS", sep="")))
ds <- ds %>% dplyr::select(-c(N100, P100, K100, yieldEffectraw, yieldEffectBlup, FDID, refYBLUP, refY)) %>% unique()
ds$location <- paste(ds$lon, ds$lat, sep="_")
str(ds)

## add province and district
countryShp <- geodata::gadm(country, level = 2, path='.')
dd2 <- raster::extract(countryShp, ds[, c("lon", "lat")])[, c("NAME_1", "NAME_2")]
ds$province <- dd2$NAME_1
ds$district <- dd2$NAME_2


## AEZ data: this is not always available for all use cases
AEZ <- readOGR(dsn=paste(pathIn2, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
gpsPoints <- ds[, c("lon", "lat")]
gpsPoints$longitude <- as.numeric(gpsPoints$lon)
gpsPoints$latitude <- as.numeric(gpsPoints$lat)
RAW_AEZ_trial <- suppressWarnings(raster::extract(RW_aez, gpsPoints[, c("longitude", "latitude")]))
ds_AEZ <- RAW_AEZ_trial %>%
  select(c(Names_AEZs)) %>%
  cbind(ds)
ds_AEZ <- ds_AEZ %>%
  dplyr::rename(AEZ = Names_AEZs)



### read the soil data and link to yield + AEZ
solDem_trial <- readRDS((paste0(pathIn, 'geo_4ML/SoilDEM_PointData_trial.RDS')))
solDem_trial$location <- paste(solDem_trial$lon, solDem_trial$lat, sep="_")
solDem_trial <- solDem_trial %>% dplyr::select(-c(lon, lat, ID))

ML_train_Data <- merge(ds_AEZ, solDem_trial, by="location", all.x = TRUE) ## some data have lon, lat falling in water bodies and no vlaue sin digital soil maps

ML_train_Data <- ML_train_Data[!is.na(ML_train_Data$c_tot_top), ]

# removing variables with unlikely predictive value:
sdt <- ML_train_Data %>%
  dplyr::select(-c(plantingDate, harvestDate, TPI,  TRI, slope, fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30, NAME_1, NAME_2)) %>%
  mutate(province = "Iburengerazuba", district = "Rubavu") %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         TLID = as.factor(TLID),
         expCode = as.factor(expCode),
         TY = TY) %>%
  unique()
 
#select treatments with high nutrient rates (Increased NPK for RS-PFR-1, NPK_all for IFDC, NPK11 for SA-VAL-1): create ref yield class
ds_ref <- sdt %>%
  filter(refTreat == TRUE) %>%
  dplyr::group_by(expCode, TLID) %>%
  dplyr::summarise(refY = median(blup)) %>%
  mutate(refY = cut(refY, c(-Inf, 10, 20, 30, 40, Inf), labels = c("Very low", "Low", "Medium", "High", "Very high")))%>%
  unique()


#Topography and EAZ data, creating altitude classes:
tdt <- sdt %>%
  dplyr::select(c(expCode, TLID, altitude)) %>%
  mutate(alt = cut(altitude, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)))%>%
  unique()


ds_sdt <- sdt %>%
  left_join(tdt) %>%
  left_join(ds_ref) %>%
  dplyr::select(-c(altitude)) %>%
  unique()


#############################
# 3. Running reverse QUEFTS #
#############################
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")

## get soil INS
supply <- NULL
for(i in unique(ds_sdt$TLID)){
   #subsetting and preparing data for revQUEFTS:
  dsi <- ds_sdt[ds_sdt$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y" #Aim is to explain variation in the BLUP yields
  dsi$Y <- dsi$Y * 1000 * 0.21 #converting to kg DM/ha, assuming 79% moisture content
  
  yy <- dsi[dsi$refTreat == "TRUE", ]
  Yai <- mean(yy$Y) * 1.2 ## correcting for yield loss due to poor management and because fert.rates are not high enough to remove nutrient deficiency 
  
  #at least 3 rows of data are needed + attainable yield:
  if(length(unique(dsi$treat)) > 2 & !is.na(Yai)){
    
    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = "Potato")
    print(si)
    supply <- rbind(supply, data.frame(TLID = i,
                                       Ya = Yai,
                                       N_base_supply = si[1],
                                       P_base_supply = si[2],
                                       K_base_supply = si[3]))
  }
}

saveRDS(supply, paste(pathOut1, "soilINS_revQUEFTS.RDS", sep=""))
supply <- readRDS(paste(pathOut1, "soilINS_revQUEFTS.RDS", sep=""))
summary(supply)


#############################
# 4. visualizing the soil INS
#############################
INS <- supply %>%
  #adding lats and lons and data source:
  left_join(ds_sdt %>% dplyr::select(TLID, lat, lon, expCode, season) %>% unique()) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  #setting negative values to zero and maximal values to 750:
  mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
         #across(c(N_base_supply:K_base_supply), ~ ifelse(.x > 400, 400, .x)),
         #N_base_supply = ifelse(N_base_supply > 90, 90, N_base_supply),
         P_base_supply = ifelse(P_base_supply > 1000, 1000, P_base_supply),
         #K_base_supply = ifelse(K_base_supply > 105, 105, K_base_supply)
  ) %>%
  mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
  #remove incomplete rows:
  na.omit()
head(INS)


## Demonstrate ranges in supply by experiment and season combinations:
INS2 <- INS %>%
  gather(variable, value, N_base_supply:K_base_supply) %>%
  mutate(variable = factor(variable, levels = c("N_base_supply", "P_base_supply", "K_base_supply")),
         variable = revalue(variable, c("N_base_supply" = "N",
                                        "P_base_supply" = "P",
                                        "K_base_supply" = "K")),
         season = ifelse(grepl("A", season), "A", "B"))


gg1 <- ggplot(INS2, aes(x = expCode, y = value, fill = season)) + 
            geom_boxplot()+
            scale_fill_manual(values = c("grey90", "grey50"))+
            facet_wrap(~variable, nrow=1) +
            #ylim(0,500) +
            theme_gray() +
            ylab("Indigenous nutrient supply (kg/ha)\n") +
            theme(axis.title.y = element_text(size = 15, face="bold"),
                  axis.title.x = element_blank(),
                  legend.text = element_text(size = 14),
                  legend.title = element_text(size = 14, face = "bold"),
                  legend.position = c(0.07, 0.9),
                  axis.text = element_text(size = 14),
                  strip.text = element_text(size = 14, face="bold"))

    

  ggsave(paste(pathOut1, "INS_reverseQUEFTS.pdf", sep=""), gg1, width=8, height=8)
      
      

  #############################
  # 5. Predict indigenous nutrient supply from soil data. season_AB, #season could be considered or dropped and refY #reference yield class could be considered or dropped
  #############################
  
  ins <- ds_sdt %>%
     left_join(INS) %>%
    dplyr::select(-c(TLID, treat, N, P, K, blup, refTreat, expCode, 
                     lat, lon, season, AEZ, TY, Ya)) %>%
    mutate(season_AB = as.factor(season_AB))%>%
    unique() %>%  na.omit()
    


set.seed(777)
start <- Sys.time()
RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins, select = -c(P_base_supply, K_base_supply)), 
                     importance=TRUE, ntree=1000)

RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins, select = -c(N_base_supply, K_base_supply)),
                     importance=TRUE, ntree=1000)

RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins, select = -c(N_base_supply, P_base_supply)),
                     importance=TRUE, ntree=1000)

end <- Sys.time()
t_randomForest <- end - start

## ranger is faster
start <- Sys.time()
RF_N_ranger <- ranger(formula = log(N_base_supply) ~ ., 
                      data = subset(ins, select = -c(P_base_supply, K_base_supply)), 
                      num.trees=1000)
RF_P_ranger <- ranger(formula = log(P_base_supply) ~ ., 
                      data = subset(ins, select = -c(N_base_supply, K_base_supply)), 
                      num.trees=1000)
RF_K_ranger <- ranger(formula = log(K_base_supply) ~ ., 
                      data = subset(ins, select = -c(N_base_supply, P_base_supply)), 
                      num.trees=1000)
end <- Sys.time()
t_ranger <- end - start

 

rmse(ins$N_base_supply, exp(predict(RF_N_ranger, ins)$predictions))
rmse(ins$N_base_supply, exp(predict(RF_N, ins)))

rmse(ins$P_base_supply, exp(predict(RF_P_ranger, ins)$predictions))
rmse(ins$P_base_supply, exp(predict(RF_P, ins)))

rmse(ins$K_base_supply, exp(predict(RF_K_ranger, ins)$predictions))
rmse(ins$K_base_supply, exp(predict(RF_K, ins)))


#variable importance plots:
varImpPlot(RF_N)
varImpPlot(RF_P)
varImpPlot(RF_K)

#variance explained:
RF_N$rsq[1000]
RF_P$rsq[1000]
RF_K$rsq[1000]

#rmse
sqrt(RF_N$mse[1000])
sqrt(RF_P$mse[1000])
sqrt(RF_K$mse[1000])



#Leave-one-out cross-validation using ranger (much faster with minimal increase in RMSE):
ins2 <- ds_sdt %>%
  left_join(INS) %>%
  dplyr::select(-c(treat, N, P, K, blup, refTreat, 
                   lat, lon, AEZ, TY, Ya)) %>%
  mutate(season_AB = as.factor(season_AB))%>%
  unique() %>%  na.omit()


preds <- NULL
run <- 0

for(i in unique(ins2$TLID)){
  
  print(paste0(round(run/length(unique(ins2$TLID))*100), "% complete"))
  

  ins_train <- subset(ins2[ins2$TLID != i,], select = -c(TLID))
  ins_valid <- subset(ins2[ins2$TLID == i,], select = -c(TLID))
  
  RF_Nv <- ranger(formula = log(N_base_supply) ~ ., 
                 data = subset(ins_train, select = -c(P_base_supply, K_base_supply)), 
                 num.trees=500)
  
  RF_Pv <- ranger(formula = log(P_base_supply) ~ ., 
                 data = subset(ins_train, select = -c(N_base_supply, K_base_supply)), 
                 num.trees=500)
  
  RF_Kv <- ranger(formula = log(K_base_supply) ~ ., 
                 data = subset(ins_train, select = -c(N_base_supply, P_base_supply)), 
                 num.trees=500)
  
  preds <- rbind(preds, data.frame(TLID = i,
                                   N_pred = exp(predict(RF_Nv, ins_valid)$predictions),
                                   P_pred = exp(predict(RF_Pv, ins_valid)$predictions),
                                   K_pred = exp(predict(RF_Kv, ins_valid)$predictions)))
  run <- run + 1
  
}

saveRDS(preds, paste(pathOut1, "LOOCV_predictions_NPK_base_supply_afterlmer.RDS", sep=""))
preds <- readRDS(paste(pathOut1, "LOOCV_predictions_NPK_base_supply_afterlmer.RDS", sep=""))


pINS4 <- ins2 %>% 
  left_join(preds) %>%
  dplyr::select(TLID, N_base_supply, P_base_supply, K_base_supply, N_pred, P_pred, K_pred) %>%
  gather(variable, value, -TLID) %>%
  mutate(nutrient = substr(variable, 1, 1),
         nutrient = factor(nutrient, levels = c("N", "P", "K")),
         variable = ifelse(grepl("pred", variable), "RandomForest", "revQUEFTS")) %>%
  tidyr::spread(variable, value)%>%
  left_join(ins2 %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
  mutate(season = ifelse(grepl("A", season), "A", "B"),
         nutrient = as.factor(nutrient)) %>%
  na.omit() %>% 
  unique()


INS <- INS %>% 
  left_join(preds) 


#plot demonstrating reverse QUEFTS calculated N, P and K supply versus RF predictions using LOOCV:
pINS4$nutrient <- factor(pINS4$nutrient, levels=c("N", "P", "K"))

ggplot(pINS4, aes(x = RandomForest, y = revQUEFTS)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(fill = season, shape=expCode), size = 3) + 
  scale_shape_manual(values = 21:23) +
  scale_fill_manual(values = c("grey90", "grey50"))+
  facet_wrap(~nutrient, nrow=1, scales="free") +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6,
                        label.y = .975) +
  geom_text(data = pINS4 %>% 
              group_by(nutrient) %>% 
              dplyr::summarise(rmse = sqrt(sum((revQUEFTS - RandomForest)**2)/n()),
                        RandomForest = 0) %>%
              mutate(revQUEFTS = c(200, 200, 200)*0.95),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlim(0, NA)+
  ylim(0, 200)+
  xlab("\nRandom Forest predicted soil nutrient supply [kg/ha]")+
  ylab("Reverse QUEFTS calculated supply [kg/ha]\n") +
  theme_gray() +
  theme(strip.text = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        legend.position = "none",
        axis.title = element_text(size = 15, face="bold"))


#################################################
# 6. Predict yield and compare different models #
#################################################
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")

ins3 <- ds_sdt %>% 
  left_join(supply) %>%
  left_join(preds) %>%
  unique()


py <- NULL
ni <- 0

for(i in unique(ins2$TLID)){
  
  print(paste0(round(ni/length(unique(ins2$TLID))*100), "% complete"))
  ni <- ni+1
  
  # dsi <- ds[ds$TLID == i,]
  dsi <- ins3[ins3$TLID == i, ]
  
  fri <- data.frame("N" = dsi$N,
                    "P" = dsi$P,
                    "K" = dsi$K, 
                    "reftreat" = dsi$refTreat)
  
  #different supply estimates:
  ## 1.  INS from reverse QUEFTS
  sqi <- unique(dsi[dsi$TLID == i, c("N_base_supply", "P_base_supply", "K_base_supply")])
  
  ## 2. INS from random forest
  spi <- unique(dsi[dsi$TLID == i, c("N_pred", "P_pred", "K_pred")])
  
  ## 3. One INS value across the whole area ( the median of the reverse QUEFTS INS)
  sni <- ddply(INS, .(), summarise, 
               N_base_supply_1 = median(N_base_supply), 
               P_base_supply_1 = median(P_base_supply),
               K_base_supply_1 = median(K_base_supply))
  sni <- sni[, c("N_base_supply_1", "P_base_supply_1", "K_base_supply_1")]
  

  # ayi <- unique(dsi[dsi$refTreat == TRUE, ]$Ya)
  ayi <- unique(as.numeric(dsi[dsi$TLID == i,]$refY) * 10 * 1000 * 0.21)
  
  #yield predicted using reverse Quefts calculated supply
  yqi <- runQUEFTS(nut_rates = fri[, 1:3],
                   supply = as.numeric(sqi),
                   crop = "Potato",
                   Ya = ayi,
                   SeasonLength = 120)
  
  #yield predicted using supply obtained from predictions by RF
  ypi <- runQUEFTS(nut_rates = fri[, 1:3],
                   supply = as.numeric(spi),
                   crop = "Potato",
                   Ya = ayi,
                   SeasonLength = 120)
  
  #yield predicted by a naive model using median values of NPK supply across all TLIDs:
  yni <- runQUEFTS(nut_rates = fri[, 1:3],
                   supply = as.numeric(sni),
                   crop = "Potato",
                   Ya = ayi,
                   SeasonLength = 120)
  
  py <- rbind(py, data.frame(TLID = i,
                             N = fri$N,
                             P = fri$P,
                             K = fri$K,
                             refTreat = fri$reftreat,
                             Yq = yqi / 1000 / 0.21, #yield predicted using revQUEFTS supply
                             Yp = ypi / 1000 / 0.21, #yield predicted using RF predicted supply
                             Yn = yni / 1000 / 0.21, #yield predicted using median nutrient supply
                             Yb = dsi$blup))          #yield blup
                             
}

py <- unique(py)

ds3 <- unique(readRDS(paste(pathIn, "compiled_fieldData.RDS", sep=""))) %>%
  select(-c(refY, refYBLUP, yieldEffectraw, yieldEffectBlup))


py %>% left_join(ds3 %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
  mutate(refY = ifelse(N > 75 & P > 30 & K > 50, "Reference treatment", "Other treatments")) %>%
  ggplot(aes(x = Yp, y = Yq)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  
  #facet_wrap(~expCode+season, ncol=3) + 
  xlab("\nPredicted tuber yield (t/ha)")+
  ylab("Reverse QUEFTS calculated potato tuber yield (t/ha)\n")+
  xlim(0, 62.5)+
  ylim(0, 62.5)+
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))




pyr <- py %>%
  gather(variable, value, Yq:Yb) %>%
  group_by(TLID, N, P, K, variable,refTreat) %>%
  dplyr::summarise(value = mean(value)) %>%
  mutate(treat = ifelse(refTreat == TRUE, "ref", "other")) %>%
  group_by(TLID, variable) %>%
  mutate(refY = mean(ifelse(treat == "ref", value, NA), na.rm=TRUE),
         dY = refY - value) %>%
  filter(treat != "ref") %>%
  dplyr::select(-treat, -value, -refY) %>%
  spread(variable, dY) %>%
  gather(variable, value, c(Yq, Yp, Yn)) %>%
  mutate(variable = mapvalues(variable, 
                              from = c("Yq", "Yp", "Yn"),
                              to = c("supply from reverse QUEFTS", "supply by RF prediction", "simple medians for supply"))) %>%
  ungroup()



pyr %>%
  filter(variable != "Yo") %>%
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") %>%
  mutate(variable = as.character(variable)) %>%
  ggplot(aes(x = value, y = Yb)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = pyr %>% 
              filter(variable != "Yo") %>% 
              group_by(variable) %>% 
              dplyr::summarise(rmse = sqrt(sum((value - Yb)**2)/n()),
                               value = -1,
                               Yb = 22),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
  ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))






############################################################################################
# 8. Alternative approach: Predict yield directly with Random Forest a la Jordan & Camilla #
############################################################################################


#Prepare dataset...
dsp <- ds %>%
  mutate(Y = blup) %>%
  group_by(TLID, season, N, P, K) %>%
  dplyr::summarise(Y = mean(Y)) %>%
  ungroup() %>%
  mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
  dplyr::select(-season) %>%
  join(sdt) %>%
  join(ds_ref) %>%
  join(tdt) %>%
  na.omit()

#First model that includes district and refY
RF0 <- randomForest(Y ~ ., subset(dsp, select=-c(TLID, expCode)), importance=TRUE, ntree=200)
sqrt(RF0$mse[length(RF0$mse)])
RF0
varImpPlot(RF0)



#Simpler model without district and refY
RF1 <- randomForest(Y ~ ., subset(dsp, select=-c(TLID, expCode, district, refY, season_AB)), importance=TRUE, ntree=200)
sqrt(RF1$mse[length(RF1$mse)])
RF1
varImpPlot(RF1)


predRFs <- NULL

#Obtain yield estimates using LOOCV:
for(i in unique(dsp$TLID)){
  
  dsp_train <- subset(dsp[dsp$TLID != i,], select = -c(TLID, expCode, TY, blup, season, altitude))
  dsp_valid <- subset(dsp[dsp$TLID == i,], select = -c(TLID, expCode, TY, blup, season, altitude))
  
  #RF0 <- randomForest(Y ~ ., dsp_train, ntree=500)
  #RF1 <- randomForest(Y ~ ., subset(dsp_train, select = -c(district, refY)), ntree=500)
  
  RF0 <- ranger(formula = Y ~ ., data = dsp_train, num.trees = 200) 
  RF1 <- ranger(formula = Y ~ ., data = subset(dsp_train, select = -c(AEZ, district, refY, refTreat, season_AB)), num.trees = 200) 
  
  predRFs <- rbind(predRFs, unique(data.frame(TLID = i,
                                       subset(dsp_valid, select=c(N, P, K, Y, refTreat)),
                                       #dYp0 = predict(RF0, dsp_valid),
                                       #dYp1 = predict(RF1, dsp_valid)
                                       Yp0 = predict(RF0, dsp_valid)$predictions,
                                       Yp1 = predict(RF1, dsp_valid)$predictions))
  )
  
}


predRFsl <- predRFs %>%
  tidyr::gather(variable, value, Yp0:Yp1) %>%
  mutate(variable = mapvalues(variable, from = c("Yp0", "Yp1"), to = c("with AEZ, district, refY and season_AB", "without AEZ, district, refY and season_AB")))



ggplot(predRFsl, aes(x = value, y = Y)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = predRFsl %>% 
              group_by(variable) %>% 
              dplyr::summarise(rmse = sqrt(sum((value - Y)**2)/n()),
                               value = -1,
                               Y = 53),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("\nPredicted potato tuber yield using RF model [t/ha]") +
  ylab("BLUP potato tuber yield [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))



predRFs

predRFs <- predRFs[, c("TLID","refTreat","N", "P", "K", "Y","Yp0", "Yp1")]

predRFsl2 <- predRFs %>%
  tidyr::gather(variable, value, Y:Yp1) %>%
  dplyr::group_by(TLID, variable) %>% 
  mutate(refY = ifelse(refTreat == TRUE , value, NA)) %>%
  as.data.frame()
         

predRFsl2$dum <- paste(predRFsl2$TLID, predRFsl2$variable, sep="_")
for(dums in unique(predRFsl2$dum)){
  predRFsl2[predRFsl2$dum == dums, ]$refY <- predRFsl2[predRFsl2$dum == dums & !is.na(predRFsl2$refY), ]$refY
}

predRFsl3 <- predRFsl2 %>% 
  mutate(dY = refY - value) %>%
  filter(!(refTreat == TRUE)) %>%
  dplyr::select(-c(refY, value, dum)) %>%
  tidyr::spread(variable, dY) %>%
  dplyr::rename(dY = Y, dYp0 = Yp0, dYp1 = Yp1) %>%
  tidyr::gather(variable, value, dYp0:dYp1) %>%
  mutate(variable = mapvalues(variable, from = c("dYp0", "dYp1"), to = c("with district, refY and season_AB", "without district, refY and season_AB")))




ggplot(data = predRFsl3, aes(x = value, y = dY)) +
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = predRFsl3 %>% 
              group_by(variable) %>% 
              dplyr::summarise(rmse = sqrt(sum((value - dY)**2)/n()),
                               value = -1,
                               dY = 20),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("\nRF predicted potato tuber yield effect relative to reference treatment [t/ha]") +
  ylab("BLUP tuber yield effect relative to reference treatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

























































