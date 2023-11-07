
#################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
packages_required <- c("plyr", "tidyverse", "ggplot2", "foreach","doParallel","MuMIn","ggpmisc","sf","cluster",
                       "limSolve", "lpSolve", "Rquefts", "rgdal", "randomForest","ranger","Metrics", "factoextra")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


### the previous procedures:
## 1. agwise-datacuration/dataops/datacuration/Scripts/useCases/UseCase_Rwanda_RAB/Potato/Compiling all potato fieldData Rwanda.R 
## 2. agwise-datacuration/dataops/datacuration/Scripts/useCases/UseCase_Rwanda_RAB/Potato/randomNoiseReduction_potato_RAB.R
## 3.  run get soil and topo geo data
## 4. read soil, AEZ and top and merge with yield data



### read yield data linked with soil and weather data ML_train_Data <- unique(readRDS(paste(pathIn, "modelReady_trial.RDS", sep="")))


ML_train_Data <- unique(readRDS(paste(pathIn, "modelReady_trial.RDS", sep="")))

#################################################################################################################
# 2. read data 
#################################################################################################################
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"



pathOut1 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")

if (!dir.exists(pathOut1)){
  dir.create(file.path(pathOut1), recursive = TRUE)
}

source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/agwise_ML_routine.R")

pathInFieldData <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")
pathInSoilData <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/Soil/", sep="")
pathInTopoData <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/Topography/", sep="")
pathIn2 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/", sep="")



## field data
ds<- unique(readRDS(paste(pathInFieldData, "compiled_fieldData.RDS", sep="")))
ds <- subset(ds, select=-c(yieldEffectraw, yieldEffectBlup,refY, refYBLUP))

## soil, totpo and AEZ data
soil <- unique(readRDS(paste(pathInSoilData, "Soil_PointData_trial.RDS", sep="")))
Topo <- unique(readRDS(paste(pathInTopoData, "Topography_PointData_trial.RDS", sep="")))

soil[soil$ID == "IFDC_3", ]$NAME_1 <- "Amajyaruguru"
soil[soil$ID == "IFDC_3", ]$NAME_2 <- "Burera"
Topo[Topo$ID == "IFDC_3", ]$NAME_1 <- "Amajyaruguru"
Topo[Topo$ID == "IFDC_3", ]$NAME_2 <- "Burera"

soil[soil$ID == "SATLRW475382409484", ]$NAME_1 <- "Iburengerazuba"
soil[soil$ID == "SATLRW475382409484", ]$NAME_2 <- "Rubavu"
Topo[Topo$ID == "SATLRW475382409484", ]$NAME_1 <- "Iburengerazuba"
Topo[Topo$ID == "SATLRW475382409484", ]$NAME_2 <- "Rubavu"



AEZ <- readOGR(dsn=paste(pathIn2, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
gpsPoints <- soil[, c("longitude", "latitude")]
gpsPoints$longitude <- as.numeric(gpsPoints$longitude)
gpsPoints$latitude <- as.numeric(gpsPoints$latitude)
RAW_AEZ_trial <- suppressWarnings(raster::extract(RW_aez, gpsPoints[, c("longitude", "latitude")]))

RAW_AEZ_trial <- RAW_AEZ_trial %>%
  select(c(Names_AEZs)) %>%
  cbind(soil[, c("ID", "longitude", "latitude")])
 
 
 
 AEZ_Topo <- RAW_AEZ_trial %>%
   left_join(Topo)
 

 ds_topo <- merge(ds, AEZ_Topo, by.x=c( "TLID",  "lon","lat") ,by.y=c("ID", "longitude", "latitude"))
 
  

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
 
 
 
 INS <- supply %>%
   #adding lats and lons and data source:
   left_join(ds %>% dplyr::select(TLID, lat, lon, expCode, season) %>% unique()) %>%
   mutate(lat = as.numeric(lat),
          lon = as.numeric(lon)) %>%
   #setting negative values to zero and maximal values to 750:
   mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
          #across(c(N_base_supply:K_base_supply), ~ ifelse(.x > 400, 400, .x)),
          #N_base_supply = ifelse(N_base_supply > 400, 400, N_base_supply),
          P_base_supply = ifelse(P_base_supply > 1000, 1000, P_base_supply),
          #K_base_supply = ifelse(K_base_supply > 400, 400, K_base_supply)
   ) %>%
   mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
   #remove incomplete rows:
   na.omit()
 
 
 
 #Create plot to demonstrate ranges in supply by expCode and season combinations:
 INS %>%
   gather(variable, value, N_base_supply:K_base_supply) %>%
   mutate(variable = factor(variable, levels = c("N_base_supply", "P_base_supply", "K_base_supply")),
          variable = revalue(variable, c("N_base_supply" = "N",
                                         "P_base_supply" = "P",
                                         "K_base_supply" = "K")),
          season = ifelse(grepl("A", season), "A", "B")) %>%
   ggplot(aes(x = expCode, y = value, fill = season)) + 
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
 
 ########################################################
 # 6. Predict indigenous nutrient supply from soil data #
 ########################################################
 
 #Soil information for prediction model:
 sdt <- soil %>%
   #dplyr::select(-ID) %>%
   dplyr::rename(TLID = ID,
          lon = longitude,
          lat = latitude,
          province = NAME_1,
          district = NAME_2) %>%
   #removing soil variables with unlikely predictive value:
   dplyr::select(-c(fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30))
 
 sdt <- sdt %>%
   dplyr::select(-c(lat, lon))
 
 
 
 #Productivity class:
 ds_ref <- ds %>%
   #select treatments with high nutrient rates (Increased NPK for RS-PFR-1, NPK_all for IFDC, NPK11 for SA-VAL-1):
   filter(N > 75, P > 30, K > 50) %>%
   group_by(expCode, TLID) %>%
   dplyr::summarise(refY = median(TY)) %>%
   mutate(refY = cut(refY, c(-Inf, 10, 20, 30, 40, Inf), labels = c("Very low", "Low", "Medium", "High", "Very high")))

 
 #Topography and EAZ data:
 tdt <- AEZ_Topo %>%
   dplyr::rename(TLID = ID,
          lon = longitude,
          lat = latitude,
          alt = altitude,
          AEZ = Names_AEZs,
          Province = NAME_1,
          District = NAME_2) %>%
   dplyr::select(-c(lat, lon, Province, District, slope, TPI, TRI)) %>%
   mutate(AltClass2 = cut(alt, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)),
          #AEZ = ifelse(AEZ == "Central plateau", "Congo-Nile watershed divide", AEZ)
   )
  
 
 #Add predictors to INS dataset:
 INS <- INS %>% 
   left_join(sdt) %>%
   left_join(ds_ref) %>%
   left_join(tdt) %>%
   na.omit()
 
 
 
 #create subset with predictors only:
 ins <- INS %>%
   #filter(expCode != "IFDC") %>%
   dplyr::select(-c(TLID, lon, lat, expCode, season, Ya,
                    #season_AB, #season could be considered or dropped
                    #refY #reference yield class could be considered or dropped
   ))
 
 set.seed(333)
 
 
 #fit random forest using all data:
 RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins, select = -c(P_base_supply, K_base_supply)), 
                      importance=TRUE, ntree=1000)
 
 RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins, select = -c(N_base_supply, K_base_supply)),
                      importance=TRUE, ntree=1000)
 
 RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins, select = -c(N_base_supply, P_base_supply)),
                      importance=TRUE, ntree=1000)
 
 
 #fit random forest using all data with ranger package (faster):
 RF_N_ranger <- ranger(formula = log(N_base_supply) ~ ., 
                       data = subset(ins, select = -c(P_base_supply, K_base_supply)), 
                       num.trees=1000)
 RF_P_ranger <- ranger(formula = log(P_base_supply) ~ ., 
                       data = subset(ins, select = -c(N_base_supply, K_base_supply)), 
                       num.trees=1000)
 RF_K_ranger <- ranger(formula = log(K_base_supply) ~ ., 
                       data = subset(ins, select = -c(N_base_supply, P_base_supply)), 
                       num.trees=1000)
 
 #comparing rmse
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
 
 #Leave-one-out cross-validation:
 preds <- NULL
 run <- 0
 
 for(i in unique(INS$TLID)){
   
   print(paste0(round(run/length(unique(INS$TLID))*100), "% complete"))
   
   ins_train <- subset(INS[INS$TLID != i,], select = -c(TLID, lon, lat, expCode, season, Ya))
   ins_valid <- subset(INS[INS$TLID == i,], select = -c(TLID, lon, lat, expCode, season, Ya))
   
   RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins_train, select = -c(P_base_supply, K_base_supply)), 
                        importance=TRUE, ntree=1000)
   
   RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins_train, select = -c(N_base_supply, K_base_supply)),
                        importance=TRUE, ntree=1000)
   
   RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins_train, select = -c(N_base_supply, P_base_supply)),
                        importance=TRUE, ntree=1000)
   
   preds <- rbind(preds, data.frame(TLID = i,
                                    N_pred = exp(predict(RF_N, ins_valid)),
                                    P_pred = exp(predict(RF_P, ins_valid)),
                                    K_pred = exp(predict(RF_K, ins_valid))))
   run <- run + 1
   
 }
 
 
 saveRDS(preds, paste(pathOut1, "pred_INS.RDS", sep=""))
 preds <- readRDS(paste(pathOut1, "pred_INS.RDS", sep=""))
 
 
 INS <- INS %>% left_join(preds)
 
 pINS <- INS %>%
   dplyr::select(TLID, N_base_supply, P_base_supply, K_base_supply, N_pred, P_pred, K_pred) %>%
   gather(variable, value, -TLID) %>%
   mutate(nutrient = substr(variable, 1, 1),
          nutrient = factor(nutrient, levels = c("N", "P", "K")),
          variable = ifelse(grepl("pred", variable), "RandomForest", "revQUEFTS")) %>%
   spread(variable, value) %>%
   left_join(ds %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
   mutate(season = ifelse(grepl("A", season), "A", "B")) %>%
   #mutate(RandomForest = ifelse(nutrient == "N" & RandomForest >= 120, NA, 
   #                             ifelse(nutrient == "P"& RandomForest >= 30, NA,
   #                                    ifelse(nutrient == "K" & RandomForest >= 150, NA, RandomForest)))) %>%
   na.omit()
 
 pINS$nutrient <- factor(pINS$nutrient, levels=c("N","P", "K"))
 
 #plot demonstrating reverse QUEFTS calculated N, P and K supply versus RF predictions using LOOCV:
 ggplot(pINS, aes(x = RandomForest, y = revQUEFTS)) + 
   geom_abline(intercept = 0, slope = 1) +
   geom_point(aes(fill = season, shape=expCode), size = 3) + 
   scale_shape_manual(values = 21:23) +
   scale_fill_manual(values = c("grey90", "grey50"))+
   facet_wrap(~nutrient, nrow=1, scales="free") +
   ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
   ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                         formula = y ~ x, size = 5,
                         label.y = .975) +
   geom_text(data = pINS %>% 
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
 # 7. Predict yield and compare different models #
 #################################################
 
 py <- NULL
 ni <- 0
 
 for(i in unique(INS$TLID)){
   
   print(paste0(round(ni/length(unique(INS$TLID))*100), "% complete"))
   ni <- ni+1
   
   dsi <- ds[ds$TLID == i,]
   fri <- data.frame("N" = dsi$N,
                     "P" = dsi$P,
                     "K" = dsi$K)
   
   #different supply estimates:
   sqi <- INS[INS$TLID == i, c("N_base_supply", "P_base_supply", "K_base_supply")]
   spi <- INS[INS$TLID == i, c("N_pred", "P_pred", "K_pred")]
   sni <- INS %>% dplyr::select(N_base_supply, P_base_supply, K_base_supply) %>% 
     dplyr::summarise(across(everything(), list(median)))
   
   ayi <- as.numeric(INS[INS$TLID == i,]$refY) * 10 * 1000 * 0.21
   
   #yield predicted using reverse Quefts calculated supply
   yqi <- runQUEFTS(nut_rates = fri,
                    supply = as.numeric(sqi),
                    crop = "Potato",
                    Ya = ayi,
                    SeasonLength = 120)
   
   #yield predicted using supply obtained from predictions by RF
   ypi <- runQUEFTS(nut_rates = fri,
                    supply = as.numeric(spi),
                    crop = "Potato",
                    Ya = ayi,
                    SeasonLength = 120)
   
   #yield predicted by a naive model using median values of NPK supply across all TLIDs:
   yni <- runQUEFTS(nut_rates = fri,
                    supply = as.numeric(sni),
                    crop = "Potato",
                    Ya = ayi,
                    SeasonLength = 120)
   
   py <- rbind(py, data.frame(TLID = i,
                              N = fri$N,
                              P = fri$P,
                              K = fri$K,
                              Yq = yqi / 1000 / 0.21, #yield predicted using revQUEFTS supply
                              Yp = ypi / 1000 / 0.21, #yield predicted using RF predicted supply
                              Yn = yni / 1000 / 0.21, #yield predicted using median nutrient supply
                              Yb = dsi$blup,          #yield blup
                              Yo = dsi$TY))           #yield observed
 }
 
 
 py %>% left_join(ds %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
   #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") %>%
   mutate(refY = ifelse(N > 75 & P > 30 & K > 50, "Reference treatment", "Other treatments")) %>%
   ggplot(aes(x = Yp, y = Yq)) + 
   geom_point(alpha = .33, shape = 16) + 
   geom_abline(slope= 1, intercept = 0) + 
   #stat_poly_line(se = F) +
   #stat_poly_eq(aes(label = after_stat(eq.label)), size=6) +
   #stat_poly_eq(label.y = 0.9, size = 6) +
   #facet_wrap(~refY) +
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
   gather(variable, value, Yq:Yo) %>%
   group_by(TLID, N, P, K, variable) %>%
   dplyr::summarise(value = mean(value)) %>%
   mutate(treat = ifelse(N>75 & P>30 & K>50, "ref", "other")) %>%
   group_by(TLID, variable) %>%
   mutate(refY = mean(ifelse(treat == "ref", value, NA), na.rm=TRUE),
          dY = refY - value) %>%
   filter(treat != "ref") %>%
   dplyr::select(-treat, -value, -refY) %>%
   spread(variable, dY) %>%
   gather(variable, value, c(Yq, Yp, Yo, Yn)) %>%
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
   
   dsp_train <- subset(dsp[dsp$TLID != i,], select = -c(TLID, expCode))
   dsp_valid <- subset(dsp[dsp$TLID == i,], select = -c(TLID, expCode))
   
   #RF0 <- randomForest(Y ~ ., dsp_train, ntree=500)
   #RF1 <- randomForest(Y ~ ., subset(dsp_train, select = -c(district, refY)), ntree=500)
   
   RF0 <- ranger(formula = Y ~ ., data = dsp_train, num.trees = 200) 
   RF1 <- ranger(formula = Y ~ ., data = subset(dsp_train, select = -c(district, refY, season_AB)), num.trees = 200) 
   
   predRFs <- rbind(predRFs, data.frame(TLID = i,
                                        subset(dsp_valid, select=c(N, P, K, Y)),
                                        #dYp0 = predict(RF0, dsp_valid),
                                        #dYp1 = predict(RF1, dsp_valid)
                                        Yp0 = predict(RF0, dsp_valid)$predictions,
                                        Yp1 = predict(RF1, dsp_valid)$predictions
   ))
   
 }

 
 predRFsl <- predRFs %>%
   gather(variable, value, Yp0:Yp1) %>%
   mutate(variable = mapvalues(variable, from = c("Yp0", "Yp1"), to = c("with district, refY and season_AB", "without district, refY and season_AB")))
 
 
 
 
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
 
 
 
 predRFsl2 <- predRFs %>%
   tidyr::gather(variable, value, Y:Yp1) %>%
   dplyr::group_by(TLID, variable) %>%
   mutate(refY = ifelse(N > 75 & P > 30 & K > 50, value, NA),
          refY = mean(refY, na.rm=TRUE),
          dY = refY - value) %>%
   filter(!(N > 75 & P > 30 & K > 50)) %>%
   dplyr::select(-c(refY, value)) %>%
   spread(variable, dY) %>%
   dplyr::rename(dY = Y, dYp0 = Yp0, dYp1 = Yp1) %>%
   tidyr::gather(variable, value, dYp0:dYp1) %>%
   mutate(variable = mapvalues(variable, from = c("dYp0", "dYp1"), to = c("with district, refY and season_AB", "without district, refY and season_AB")))
 
 
 ggplot(data = predRFsl2, aes(x = value, y = dY)) +
   geom_point(alpha=.33, shape=16) +
   facet_wrap(~variable) +
   geom_text(data = predRFsl2 %>% 
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
 
 
 
 
 dsp <- ds %>%
   join(sdt) %>%
   join(ds_ref) %>%
   join(tdt) %>%
   dplyr::select(-c(expCode, FDID, lat, lon, season, plantingDate, harvestDate, treat, N100:K100, TY))
 
 fr <- data.frame("N" = seq(0, 180, 1),
                  "P" = 22,
                  "K" = 42)
 
 
 
 
 res <- NULL
 run <- 0
 
 for(tlid in sample(unique(INS[grepl("RwaSIS", INS$TLID) | grepl("RS", INS$TLID),]$TLID), 12)){
   
   run <- run+1
   print(run)
   
   sp <- INS[INS$TLID == tlid, c("N_pred", "P_pred", "K_pred")]
   ay <- as.numeric(INS[INS$TLID == tlid,]$refY) * 10 * 1000 * 0.21
   yp <- runQUEFTS(nut_rates = fr,
                   supply = as.numeric(sp),
                   crop = "Potato",
                   Ya = ay,
                   SeasonLength = 120) / 1000 / 0.21
   
   dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) %>% na.omit()
   dsp_prdct <- dsp[dsp$TLID == tlid,] %>%
     dplyr::select(-c(N, P, K, blup)) %>%
     unique() %>%
     cross_join(fr)
   
   #RF0 <- randomForest(blup ~ ., dsp_train, ntree=500)
   RF0 <- ranger(formula = blup ~ ., data = dsp_train, num.trees = 200) 
   
   #ypRF <- predict(RF0, dsp_prdct)
   ypRF = predict(RF0, dsp_prdct)$predictions
   
   
   res <- rbind(res, data.frame(TLID = tlid,
                                fr,
                                yp = yp,
                                ypRF = ypRF))
   
 }

 
 #888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
 388888888888888998888888888888888888888888888888888888888888888888888888888888888888888888888
 
 
 
 
 res %>%
   ggplot()+
   geom_point(aes(x = N, y=yp))+
   geom_point(aes(x = N, y=ypRF), colour="blue")+
   geom_point(data=ds[ds$TLID %in% res$TLID & ds$P == 22 & ds$K == 42 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
              aes(x=N, y=blup), colour="red", size = 4)+
   facet_wrap(~TLID) + 
   xlab("\nN fertilizer application rate (kg N/ha)") +
   ylab("Potato tuber yield [t/ha]\n")+
   theme_gray() +
   theme(axis.title = element_text(size = 14, face="bold"),
         axis.text = element_text(size = 14),
         strip.text = element_text(size = 14, face="bold"))
 
 
 fr <- data.frame("N" = 51,
                  "P" = 0:50,
                  "K" = 42)
 
 res <- NULL
 run <- 0
 
 for(tlid in sample(unique(INS[grepl("RwaSIS", INS$TLID) | grepl("RS", INS$TLID),]$TLID), 12)){
   
   run <- run+1
   print(run)
   
   sp <- INS[INS$TLID == tlid, c("N_pred", "P_pred", "K_pred")]
   ay <- as.numeric(INS[INS$TLID == tlid,]$refY) * 10 * 1000 * 0.21
   yp <- runQUEFTS(nut_rates = fr,
                   supply = as.numeric(sp),
                   crop = "Potato",
                   Ya = ay,
                   SeasonLength = 120) / 1000 / 0.21
   
   dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) %>% na.omit()
   dsp_prdct <- dsp[dsp$TLID == tlid,] %>%
     dplyr::select(-c(N, P, K, blup)) %>%
     unique() %>%
     cross_join(fr)
   
   #RF0 <- randomForest(blup ~ ., dsp_train, ntree=500)
   RF0 <- ranger(formula = blup ~ ., data = dsp_train, num.trees = 200) 
   
   #ypRF <- predict(RF0, dsp_prdct)
   ypRF = predict(RF0, dsp_prdct)$predictions
   
   res <- rbind(res, data.frame(TLID = tlid,
                                fr,
                                yp = yp,
                                ypRF = ypRF))
   
 }
 
 res %>%
   ggplot()+
   geom_point(aes(x = P, y=yp))+
   geom_point(aes(x = P, y=ypRF), colour="blue")+
   geom_point(data=ds[ds$TLID %in% res$TLID & round(ds$N) == 51 & ds$K == 42 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
              aes(x=P, y=blup), colour="red", size = 4)+
   facet_wrap(~TLID) + 
   xlab("\nP fertilizer application rate (kg P/ha)") +
   ylab("Potato tuber yield [t/ha]\n")+
   theme_gray() +
   theme(axis.title = element_text(size = 14, face="bold"),
         axis.text = element_text(size = 14),
         strip.text = element_text(size = 14, face="bold"))
 
 
 fr <- data.frame("N" = 51,
                  "P" = 22,
                  "K" = 0:90)
 
 res <- NULL
 run <- 0
 
 for(tlid in sample(unique(INS[grepl("RwaSIS", INS$TLID) | grepl("RS", INS$TLID),]$TLID), 12)){
   
   run <- run+1
   print(run)
   
   sp <- INS[INS$TLID == tlid, c("N_pred", "P_pred", "K_pred")]
   ay <- as.numeric(INS[INS$TLID == tlid,]$refY) * 10 * 1000 * 0.21
   yp <- runQUEFTS(nut_rates = fr,
                   supply = as.numeric(sp),
                   crop = "Potato",
                   Ya = ay,
                   SeasonLength = 120) / 1000 / 0.21
   
   dsp_train <- subset(dsp[dsp$TLID != tlid,], select = -TLID) %>% na.omit()
   dsp_prdct <- dsp[dsp$TLID == tlid,] %>%
     dplyr::select(-c(N, P, K, blup)) %>%
     unique() %>%
     cross_join(fr)
   
   #RF0 <- randomForest(blup ~ ., dsp_train, ntree=500)
   RF0 <- ranger(formula = blup ~ ., data = dsp_train, num.trees = 200) 
   
   #ypRF <- predict(RF0, dsp_prdct)
   ypRF = predict(RF0, dsp_prdct)$predictions
   
   res <- rbind(res, data.frame(TLID = tlid,
                                fr,
                                yp = yp,
                                ypRF = ypRF))
   
 }
 
 res %>%
   ggplot()+
   geom_point(aes(x = K, y=yp))+
   geom_point(aes(x = K, y=ypRF), colour="blue")+
   geom_point(data=ds[ds$TLID %in% res$TLID & round(ds$N) == 51 & round(ds$P) == 22 & (grepl("RwaSIS", ds$TLID) | grepl("RS", ds$TLID)),], 
              aes(x=K, y=blup), colour="red", size = 4)+
   facet_wrap(~TLID) + 
   xlab("\nK fertilizer application rate (kg P/ha)") +
   ylab("Potato tuber yield [t/ha]\n")+
   theme_gray() +
   theme(axis.title = element_text(size = 14, face="bold"),
         axis.text = element_text(size = 14),
         strip.text = element_text(size = 14, face="bold"))
 
 
 result <- NULL
 for(sn in c("all", unique(ds$season))){
   for(ab in c(1,0)){
     for(ry in c(1,0)){
       
       INS <- supply %>%
         #adding lats and lons and data source:
         left_join(ds %>% dplyr::select(TLID, lat, lon, expCode, season) %>% unique()) %>%
         mutate(lat = as.numeric(lat),
                lon = as.numeric(lon)) %>%
         #setting negative values to zero and maximal values to 750:
         mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
                #across(c(N_base_supply:K_base_supply), ~ ifelse(.x > 400, 400, .x)),
                #N_base_supply = ifelse(N_base_supply > 400, 400, N_base_supply),
                P_base_supply = ifelse(P_base_supply > 1000, 1000, P_base_supply),
                #K_base_supply = ifelse(K_base_supply > 400, 400, K_base_supply)
         ) %>%
         mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
         #remove incomplete rows:
         na.omit()
       
       #Add predictors to INS dataset:
       INS <- INS %>% 
         left_join(sdt) %>%
         left_join(ds_ref) %>%
         left_join(tdt) %>%
         na.omit()
       
       ins <- INS[INS$season != sn,]
       if(ab == 0){ins <- subset(ins, select = -season_AB)}
       if(ry == 0){ins <- subset(ins, select = -refY)}
       
       #Leave-one-out cross-validation using ranger (much faster with minimal increase in RMSE):
       preds <- NULL
       run <- 0
       
       for(i in unique(ins$TLID)){
         
         print(paste0(round(run/length(unique(ins$TLID))*100), "% complete"))
         
         ins_train <- subset(ins[ins$TLID != i,], select = -c(TLID, lon, lat, expCode, season, Ya))
         ins_valid <- subset(ins[ins$TLID == i,], select = -c(TLID, lon, lat, expCode, season, Ya))
         
         RF_N <- ranger(formula = log(N_base_supply) ~ ., 
                        data = subset(ins_train, select = -c(P_base_supply, K_base_supply)), 
                        num.trees=500)
         
         RF_P <- ranger(formula = log(P_base_supply) ~ ., 
                        data = subset(ins_train, select = -c(N_base_supply, K_base_supply)), 
                        num.trees=500)
         
         RF_K <- ranger(formula = log(K_base_supply) ~ ., 
                        data = subset(ins_train, select = -c(N_base_supply, P_base_supply)), 
                        num.trees=500)
         
         preds <- rbind(preds, data.frame(TLID = i,
                                          N_pred = exp(predict(RF_N, ins_valid)$predictions),
                                          P_pred = exp(predict(RF_P, ins_valid)$predictions),
                                          K_pred = exp(predict(RF_K, ins_valid)$predictions)))
         run <- run + 1
         
       }
       
       INS <- INS %>% right_join(preds)
       
       pINS <- INS %>%
         dplyr::select(TLID, N_base_supply, P_base_supply, K_base_supply, N_pred, P_pred, K_pred) %>%
         gather(variable, value, -TLID) %>%
         mutate(nutrient = substr(variable, 1, 1),
                nutrient = factor(nutrient, levels = c("N", "P", "K")),
                variable = ifelse(grepl("pred", variable), "RandomForest", "revQUEFTS")) %>%
         spread(variable, value) %>%
         left_join(ds %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
         mutate(season = ifelse(grepl("A", season), "A", "B")) %>%
         #mutate(RandomForest = ifelse(nutrient == "N" & RandomForest >= 120, NA, 
         #                             ifelse(nutrient == "P"& RandomForest >= 30, NA,
         #                                    ifelse(nutrient == "K" & RandomForest >= 150, NA, RandomForest)))) %>%
         na.omit()
       
       
       py <- NULL
       ni <- 0
       
       for(i in unique(INS$TLID)){
         
         print(paste0(round(ni/length(unique(INS$TLID))*100), "% complete"))
         ni <- ni+1
         
         dsi <- ds[ds$TLID == i,]
         fri <- data.frame("N" = dsi$N,
                           "P" = dsi$P,
                           "K" = dsi$K)
         
         #different supply estimates:
         sqi <- INS[INS$TLID == i, c("N_base_supply", "P_base_supply", "K_base_supply")]
         spi <- INS[INS$TLID == i, c("N_pred", "P_pred", "K_pred")]
         sni <- INS %>% dplyr::select(N_base_supply, P_base_supply, K_base_supply) %>% 
           summarise(across(everything(), list(median)))
         
         ayi <- as.numeric(INS[INS$TLID == i,]$refY) * 10 * 1000 * 0.21
         
         #yield predicted using reverse Quefts calculated supply
         yqi <- runQUEFTS(nut_rates = fri,
                          supply = as.numeric(sqi),
                          crop = "Potato",
                          Ya = ayi,
                          SeasonLength = 120)
         
         #yield predicted using supply obtained from predictions by RF
         ypi <- runQUEFTS(nut_rates = fri,
                          supply = as.numeric(spi),
                          crop = "Potato",
                          Ya = ayi,
                          SeasonLength = 120)
         
         #yield predicted by a naive model using median values of NPK supply across all TLIDs:
         yni <- runQUEFTS(nut_rates = fri,
                          supply = as.numeric(sni),
                          crop = "Potato",
                          Ya = ayi,
                          SeasonLength = 120)
         
         py <- rbind(py, data.frame(TLID = i,
                                    N = fri$N,
                                    P = fri$P,
                                    K = fri$K,
                                    Yq = yqi / 1000 / 0.21, #yield predicted using revQUEFTS supply
                                    Yp = ypi / 1000 / 0.21, #yield predicted using RF predicted supply
                                    Yn = yni / 1000 / 0.21, #yield predicted using median nutrient supply
                                    Yb = dsi$blup,          #yield blup
                                    Yo = dsi$TY))           #yield observed
       }
       
       
       pyr <- py %>%
         gather(variable, value, Yq:Yo) %>%
         group_by(TLID, N, P, K, variable) %>%
         summarise(value = mean(value)) %>%
         mutate(treat = ifelse(N>75 & P>30 & K>50, "ref", "other")) %>%
         group_by(TLID, variable) %>%
         mutate(refY = mean(ifelse(treat == "ref", value, NA), na.rm=TRUE),
                dY = refY - value) %>%
         filter(treat != "ref") %>%
         dplyr::select(-treat, -value, -refY) %>%
         spread(variable, dY) %>%
         gather(variable, value, c(Yq, Yp, Yo, Yn)) %>%
         mutate(variable = mapvalues(variable, 
                                     from = c("Yq", "Yp", "Yn"),
                                     to = c("supply from reverse QUEFTS", "supply by RF prediction", "simple medians for supply"))) %>%
         ungroup()
       
       
       pyr <- pyr %>% 
         filter(variable != "Yo") %>% 
         group_by(variable) %>% 
         summarise(rmse = sqrt(sum((value - Yb)**2)/n()),
                   rsq = summary(lm(Yb ~ value))$r.squared,
                   slope = lm(Yb ~ value)$coef[2],
                   sn = sn,
                   ab = ab,
                   ry = ry)
       
       
       result <- rbind(result, pyr)
       print(result)
       
     }
   }
 }
 
 
 
 
 
 result %>%
   filter(variable == "supply by RF prediction") %>%
   left_join(ds %>% dplyr::select(season, expCode) %>% unique() %>% rename(sn = season)) %>%
   mutate(x = mapvalues(paste0(ry,ab), 
                        from = c("00", "01", "10", "11"),
                        to = c("without refY \n without season", "without refY \n with season", "with refY \n without season", "with refY \n with season")),
          y = ifelse(is.na(expCode), "all data", paste0("without ", expCode, " (", sn, ")"))) %>%
   ggplot(aes(x = x, y = reorder(y, rmse), fill = rmse)) +
   geom_tile(colour="white") +
   geom_text(aes(label=round(rmse*100)/100), size = 6) +
   scale_fill_gradient2(low = "grey95", high = "darkred", mid="lightgoldenrod2", midpoint = median(result[result$variable == "supply by RF prediction",]$rmse)) +
   ggtitle("RMSE on predicted yield effects [t/ha]") +
   theme_minimal() +
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   theme(axis.title = element_blank(),
         axis.text = element_text(size = 12),
         plot.title = element_text(hjust = 0, face = "bold", size = 14),
         legend.text = element_text(size = 12),
         legend.title = element_blank())
 
 result %>%
   filter(variable == "supply by RF prediction") %>%
   left_join(ds %>% dplyr::select(season, expCode) %>% unique() %>% rename(sn = season)) %>%
   mutate(x = mapvalues(paste0(ry,ab), 
                        from = c("00", "01", "10", "11"),
                        to = c("without refY \n without season", "without refY \n with season", "with refY \n without season", "with refY \n with season")),
          y = ifelse(is.na(expCode), "all data", paste0("without ", expCode, " (", sn, ")"))) %>%
   ggplot(aes(x = x, y = reorder(y, rmse), fill = rsq)) +
   geom_tile(colour="white") +
   geom_text(aes(label=round(rsq*100)/100), size = 6) +
   scale_fill_gradient2(low = "khaki1", high = "seagreen3", mid="palegreen3", midpoint = mean(result[result$variable == "supply by RF prediction",]$rsq)) +
   ggtitle("R2 BLUP versus predicted yield effect") +
   theme_minimal() +
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   theme(axis.title = element_blank(),
         axis.text = element_text(size = 12),
         plot.title = element_text(hjust = 0, face = "bold", size = 14),
         legend.text = element_text(size = 12),
         legend.title = element_blank())
 
 
 result %>%
   filter(variable == "supply by RF prediction") %>%
   left_join(ds %>% dplyr::select(season, expCode) %>% unique() %>% rename(sn = season)) %>%
   mutate(x = mapvalues(paste0(ry,ab), 
                        from = c("00", "01", "10", "11"),
                        to = c("without refY \n without season", "without refY \n with season", "with refY \n without season", "with refY \n with season")),
          y = ifelse(is.na(expCode), "all data", paste0("without ", expCode, " (", sn, ")"))) %>%
   ggplot(aes(x = x, y = reorder(y, rmse), fill = slope)) +
   geom_tile(colour="white") +
   geom_text(aes(label=round(slope*100)/100), size = 6) +
   scale_fill_gradient2(low = "grey95", high = "royalblue1", mid="turquoise", midpoint = mean(result[result$variable == "supply by RF prediction",]$slope)) +
   ggtitle("Slope BLUP versus predicted yield effect") +
   theme_minimal() +
   scale_x_discrete(expand=c(0,0))+
   scale_y_discrete(expand=c(0,0))+
   theme(axis.title = element_blank(),
         axis.text = element_text(size = 12),
         plot.title = element_text(hjust = 0, face = "bold", size = 14),
         legend.text = element_text(size = 12),
         legend.title = element_blank())
 
 
 # Graph showing attainable yields by AEZ and 
 ays <- supply %>%
   left_join(tdt %>% dplyr::select(TLID, AEZ)) %>%
   left_join(sdt %>% dplyr::select(TLID, province, district)) %>%
   filter(!is.na(district)) %>%
   #trials in Central plateau were supposed to be in Buberuka highlands...
   mutate(AEZ = ifelse(AEZ == "Central plateau", "Buberuka highlands", AEZ))
 
 ays %>% 
   mutate(refY = cut(Ya/0.21/1000, breaks = c(-Inf, 10, 20, 30, 40, Inf), labels = c("Very low", "Low", "Medium", "High", "Very high"))) %>%
   group_by(AEZ, refY) %>%
   summarise(n = n()) %>%
   mutate(freq = n / sum(n))
 
 ggplot(ays, aes(x = district, y = Ya/0.21/1000)) + 
   geom_hline(yintercept = seq(0, 50, 10), linetype=2)+
   geom_boxplot(fill="azure2") + 
   geom_hline(data = ays %>% group_by(AEZ) %>% summarise(Ya = median(Ya)), aes(yintercept = Ya/0.21/1000), linewidth=1.5, colour="blue")+
   geom_text(data = data.frame(AEZ = "Congo-Nile watershed divide",
                               district = "Karongi",
                               y = c(5, 15, 25, 35, 45),
                               label = c("I", "II", "III", "IV", "V")),
             aes(y = y, label = label),
             colour = "grey66", size = 9, fontface = "bold")+
   coord_flip()+
   ylim(0, 58)+
   ylab("Attainable potato tuber yield (t/ha)")+
   ggforce::facet_col(facets = vars(AEZ), 
                      scales = "free_y", 
                      space = "free") +
   theme_bw()+
   theme(axis.title.y = element_blank(),
         axis.title.x = element_text(size = 14, face = "bold"),
         axis.text = element_text(size = 12),
         strip.text = element_text(size = 12, face="bold", hjust = 0),
         strip.background = element_blank()
   )
 
 
 #######################################################
 # Using RF model to predict INS, IPS and IKS at scale #
 #######################################################
 
 #Decision by team: exclude season, but include reference yield:
 INS <- supply %>%
   #adding lats and lons and data source:
   left_join(ds %>% dplyr::select(TLID, lat, lon, expCode, season) %>% unique()) %>%
   mutate(lat = as.numeric(lat),
          lon = as.numeric(lon)) %>%
   #setting negative values to zero and maximal values to 750:
   mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
          #across(c(N_base_supply:K_base_supply), ~ ifelse(.x > 400, 400, .x)),
          #N_base_supply = ifelse(N_base_supply > 400, 400, N_base_supply),
          P_base_supply = ifelse(P_base_supply > 1000, 1000, P_base_supply),
          #K_base_supply = ifelse(K_base_supply > 400, 400, K_base_supply)
   ) %>%
   mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
   #remove incomplete rows:
   na.omit() %>%
   left_join(sdt) %>%
   left_join(ds_ref) %>%
   left_join(tdt) %>%
   mutate(AEZ = factor(AEZ),
          AltClass = factor(AltClass),
          province = factor(province),
          dataset = "train") %>%
   droplevels() %>%
   na.omit()
 
 #create subset with predictors only:
 ins <- INS %>%
   dplyr::select(-c(TLID, expCode, season, Ya,
                    season_AB, #season could be considered or dropped
   ))
 
 #spatial data at 5x5 km:
 rwd <- readRDS("ML_test_Data.RDS") %>%
   dplyr::select(-id.y) %>%
   mutate(AES = factor(AEZ))
 
 #spatial data at 1x1 km:
 rwd <- readRDS("Soil_PointData_AOI.RDS") %>%
   left_join(readRDS("Topography_PointData_AOI.RDS"))
 rwaez <- st_read("AEZ_DEM_Dissolve.shp")
 rwaez <- st_transform(rwaez, crs = st_crs(4326))
 coords <- st_as_sf(rwd[, c("longitude", "latitude")], coords = c("longitude", "latitude"), crs = 4326)
 coords <- st_join(coords, rwaez, join = st_intersects)
 rwd$AEZ <- factor(coords$Names_AEZs)
 
 rwd <- rwd %>%
   rename(lon = longitude,
          lat = latitude,
          alt = altitude,
          province = NAME_1,
          district = NAME_2) %>%
   mutate(AltClass = cut(alt, c(-Inf, 2000, 2200, 2400, Inf), labels = paste0("Class_", 1:4)),
          AltClass2 = cut(alt, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)),
          AltClass2 = revalue(AltClass2, c("(3e+03, Inf]" = "(2.5e+03,3e+03]")),
          AEZ = factor(AEZ),
          province = factor(province),
          district = factor(district),
          N_base_supply = NA,
          P_base_supply = NA,
          K_base_supply = NA,
          dataset = "AOI") %>%
   filter(AEZ %in% unique(ins$AEZ),
          AEZ != "Central plateau") %>%
   cross_join(data.frame(refY = unique(ins$refY))) %>%
   droplevels()
 
 rwd <- rwd %>%
   dplyr::select(-setdiff(names(rwd), names(ins))) %>%
   bind_rows(ins)
 
 
 
 set.seed(666)
 
 #fit random forest using all data:
 RF_N <- randomForest(log(N_base_supply) ~ ., subset(rwd[rwd$dataset == "train",], 
                                                     select = -c(P_base_supply, K_base_supply, dataset, province, district)), 
                      importance=TRUE, ntree=1000)
 
 RF_P <- randomForest(log(P_base_supply) ~ ., subset(rwd[rwd$dataset == "train",], 
                                                     select = -c(N_base_supply, K_base_supply, dataset, province, district)),
                      importance=TRUE, ntree=1000)
 
 RF_K <- randomForest(log(K_base_supply) ~ ., subset(rwd[rwd$dataset == "train",], 
                                                     select = -c(N_base_supply, P_base_supply, dataset, province, district)),
                      importance=TRUE, ntree=1000)
 
 
 rwd$predN <- exp(predict(RF_N, rwd))
 rwd$predP <- exp(predict(RF_P, rwd))
 rwd$predK <- exp(predict(RF_K, rwd))
 
 ggN <- ggplot()+
   geom_tile(data = rwd[rwd$dataset == "AOI" & rwd$refY %in% c("Low", "Medium", "High", "Very high"),], aes(x=lon, y=lat, fill = predN))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
   facet_wrap(~refY, nrow = 1) +
   scale_fill_gradient(low="palegoldenrod", high="darkgreen",
                       limits = quantile(rwd[rwd$dataset == "AOI" & rwd$refY %in% c("Low", "Medium", "High", "Very high"),]$predN, c(0.05, 0.95)), 
                       oob = scales::squish) +
   guides(fill=guide_legend(title="Soil N supply\n[kg N/ha]")) +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank())
 
 ggP <- ggplot()+
   geom_tile(data = rwd[rwd$dataset == "AOI" & rwd$refY %in% c("Low", "Medium", "High", "Very high"),], aes(x=lon, y=lat, fill = predP))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
   facet_wrap(~refY, nrow = 1) +
   scale_fill_gradient(low="mistyrose2", high="darkred",
                       limits = quantile(rwd[rwd$dataset == "AOI" & rwd$refY %in% c("Low", "Medium", "High", "Very high"),]$predP, c(0.05, 0.95)), 
                       oob = scales::squish) +
   guides(fill=guide_legend(title="Soil P supply\n[kg P/ha]")) +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank())
 
 ggK <- ggplot()+
   geom_tile(data = rwd[rwd$dataset == "AOI" & rwd$refY %in% c("Low", "Medium", "High", "Very high"),], aes(x=lon, y=lat, fill = predK))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
   facet_wrap(~refY, nrow = 1) +
   scale_fill_gradient(low="lightblue1", high="darkblue", 
                       limits = quantile(rwd[rwd$dataset == "AOI" & rwd$refY %in% c("Low", "Medium", "High", "Very high"),]$predK, c(0.05, 0.95)), 
                       oob = scales::squish) +
   guides(fill=guide_legend(title="Soil K supply\n[kg K/ha]")) +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank())
 
 rwd %>%
   gather(nutrient, supply, predN:predK) %>%
   filter(dataset == "AOI") %>%
   filter(refY != "Very low") %>%
   group_by(nutrient, refY, AEZ) %>%
   summarise(supply = round(median(supply))) %>%
   print(n = 36)
 
 rwd %>%
   gather(nutrient, supply, predN:predK) %>%
   filter(dataset == "AOI") %>%
   ggplot(aes(x = AEZ, y = supply)) +
   geom_boxplot() +  
   coord_flip() +
   scale_y_log10() +
   facet_grid(refY ~ nutrient, scales = "free_x")
 
 # preparing data for K-means clustering
 aoi <- droplevels(rwd[rwd$dataset == "AOI",])
 aoi$ID <- 1:nrow(aoi)
 
 summary(lm(predN ~ AEZ + district + refY, data=aoi))$r.squared
 summary(lm(predN ~ district + refY, data=aoi))$r.squared
 summary(lm(predN ~ AEZ + refY, data=aoi))$r.squared
 summary(lm(predN ~ refY, data=aoi))$r.squared
 summary(lm(predN ~ AEZ, data=aoi))$r.squared
 
 summary(lm(predP ~ AEZ + district + refY, data=aoi))$r.squared
 summary(lm(predP ~ district + refY, data=aoi))$r.squared
 summary(lm(predP ~ AEZ + refY, data=aoi))$r.squared
 summary(lm(predP ~ refY, data=aoi))$r.squared
 summary(lm(predP ~ AEZ, data=aoi))$r.squared
 
 summary(lm(predK ~ AEZ + district + refY, data=aoi))$r.squared
 summary(lm(predK ~ district + refY, data=aoi))$r.squared
 summary(lm(predK ~ AEZ + refY, data=aoi))$r.squared
 summary(lm(predK ~ refY, data=aoi))$r.squared
 summary(lm(predK ~ AEZ, data=aoi))$r.squared
 
 
 cls <- NULL
 nclus <- 100
 for(i in unique(aoi$refY)){
   for(j in unique(aoi$AEZ)){
     tmp <- subset(aoi[aoi$refY == i & aoi$AEZ == j,], select = c(ID, AEZ, refY, predN, predP, predK))
     fit <- kmeans(subset(tmp, select = -c(ID, AEZ, refY)), nclus)
     print(paste0("between_SS / total_SS = ", round(fit$betweenss/fit$totss*100),"%"))
     tmp <- data.frame(tmp, fit$cluster)
     cls <- rbind(cls, tmp)
   }
 }
 
 cls <- cls %>% left_join(aoi %>% dplyr::select(ID, district, lat, lon))
 
 clss <- cls %>%
   group_by(AEZ, refY, fit.cluster) %>%
   summarise(clusN = median(predN),
             clusP = median(predP),
             clusK = median(predK))
 
 my_ferts <- rbind(fertilizers()[c(8,12),], data.frame(group = "synthetic",
                                                       name = "NPK 17-17-17",
                                                       N = 17,
                                                       P = 17*2*31 / (2*31 + 5*16),
                                                       K = 17*2*39.1 / (2*39.1 + 16),
                                                       Ca = 0,
                                                       Mg = 0,
                                                       S = 0,
                                                       Mb = 0,
                                                       Zn = 0, 
                                                       Bo = 0,
                                                       Cu = 0))
 fert_rates_blanket <- c(0, 0, 300)
 yts <- c(0, 0.1, 0.2)
 
 pyaoi <- NULL
 frecs <- NULL
 
 for(i in 1:nrow(clss)){
   
   print(i)
   
   si <- as.numeric(clss[i, c("clusN", "clusP", "clusK")])
   yai <- as.numeric(clss[i, ]$refY) * 10 * 1000 * 0.21 * 1.2 #set to 20% above ceiling of yield class
   
   TY0 <- runQUEFTS(nut_rates = data.frame("N" = 0, "P" = 0, "K" = 0),
                    supply = si,
                    crop = "Potato",
                    Ya = yai)
   
   TYb <- runQUEFTS(nut_rates = as.data.frame(as.list(nutrientRates(my_ferts, fert_rates_blanket))),
                    supply = si,
                    crop = "Potato",
                    Ya = yai)
   
   FRS <- NULL
   for(j in yts){
     FRS <- rbind(FRS, data.frame(id = i,
                                  target = j,
                                  TYt = TYb * (1 + j) / 1000 / 0.21,
                                  name = my_ferts$name,
                                  rate = rec_targetdY(my_ferts = my_ferts,
                                                      dY = TYb * (1 + j) - TY0,
                                                      target = "absolute",
                                                      supply = si,
                                                      crop = "Potato", #crop to be defined by QUEFTS
                                                      Ya = yai)))
     
     
   }
   
   pyaoi <- rbind(pyaoi, data.frame(id = i,
                                    TY0 = TY0 / 1000 / 0.21,
                                    TYb = TYb / 1000 / 0.21))
   frecs <- rbind(frecs, FRS)
   
 }
 
 rec <- clss %>%
   bind_cols(pyaoi) %>%
   left_join(frecs) %>%
   mutate(rate = ifelse(rate<0, 0, rate)) %>%
   dplyr::select(-id) %>%
   mutate(name = mapvalues(name, 
                           from = c("Urea (U-46)", "Diammonium phosphate", "NPK 17-17-17"),
                           to = c("Urea", "DAP", "NPK171717"))) %>%
   spread(name, rate) %>%
   ungroup() %>%
   left_join(cls, relationship = "many-to-many")
 
 saveRDS(rec, "Fertilizer_recommendations_potato_rw.RDS")
 
 gg0 <- ggplot()+
   geom_tile(data = rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,], aes(x=lon, y=lat, fill = TY0))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
   facet_wrap(~refY, nrow = 1) +
   scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(10, 25, 5),
                        #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                        limits = c(5, 40),
                        oob = scales::squish) +
   guides(fill=guide_legend(title="Yield without\nfertilizer [t/ha]")) +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank())
 
 rec %>%
   filter(refY != "Very low",
          rec$target == 0) %>%
   group_by(refY, AEZ) %>%
   summarise(TY0 = median(TY0),
             TYb = median(TYb))
 
 
 ggBR <- ggplot()+
   geom_tile(data = rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target == 0,], aes(x=lon, y=lat, fill = TYb))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
   facet_wrap(~refY, nrow = 1) +
   scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(10, 40, 5),
                        #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TYb, c(0.05, 0.95)), 
                        limits = c(5, 40),
                        oob = scales::squish) +
   guides(fill=guide_legend(title="Yield with\ncurrent blanket\nrecommendation\n[t/ha]")) +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank())
 
 selrec <- rec %>% 
   filter((refY == "Medium" & AEZ == "Congo-Nile watershed divide") | (refY == "High" & AEZ != "Congo-Nile watershed divide"))
 
 ggUrea <- ggplot()+
   geom_tile(data = selrec, aes(x=lon, y=lat, fill = Urea))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
   facet_wrap(~target, nrow = 1) +
   scale_fill_viridis_c(direction = -1, option = "viridis",
                        breaks = seq(0, 300, 50),
                        limits = c(0, 300), 
                        oob = scales::squish) +
   guides(fill=guide_legend(title="Urea\nrequirement\n[kg/ha]")) +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank())
 
 ggDAP <- ggplot()+
   geom_tile(data = selrec, aes(x=lon, y=lat, fill = DAP))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
   facet_wrap(~target, nrow = 1) +
   scale_fill_viridis_c(direction = -1, option = "rocket",
                        breaks = seq(0, 250, 50),
                        limits = c(0, 250), 
                        oob = scales::squish) +
   guides(fill=guide_legend(title="DAP\nrequirement\n[kg/ha]")) +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank())
 
 
 ggNPK <- ggplot()+
   geom_tile(data = selrec, aes(x=lon, y=lat, fill = NPK171717))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   #geom_sf_text(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = ADM2_EN))+
   facet_wrap(~target, nrow = 1) +
   scale_fill_viridis_c(direction = -1, option = "mako",
                        breaks = seq(0, 400, 50),
                        limits = c(0, 400), 
                        oob = scales::squish) +
   guides(fill=guide_legend(title="NPK 17:17:17\nrequirement\n[kg/ha]")) +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank())
 
 
 rec %>%
   gather(name, rate, DAP:Urea) %>%
   filter(refY %in% c("Low", "Medium", "High"),
          target < 0.3) %>%
   ggplot(aes(y = rate, x = AEZ, fill = as.factor(refY))) +
   geom_boxplot(outlier.shape = NA) +
   coord_flip() +
   facet_grid(target ~ name, scales="free")
 
 rec %>%
   gather(name, rate, DAP:Urea) %>%
   filter(refY %in% c("Low", "Medium", "High"),
          target < 0.3) %>%
   ggplot(aes(y = rate, x = AEZ)) +
   geom_boxplot(outlier.shape = NA) +
   coord_flip() +
   facet_grid(name ~ refY + target, scales="free")
 
 rec %>%
   gather(name, rate, DAP:Urea) %>%
   filter(refY %in% c("Low", "Medium", "High", "Very high"),
          target < 0.3) %>%
   group_by(AEZ, refY, target, name) %>%
   summarise(rate = median(rate, na.rm = TRUE)) %>%
   unite(tmp, target, name) %>%
   spread(tmp, rate) %>%
   print(n = 12)
 
 
 
 
 
 
 ggUrea <- ggplot()+
   geom_tile(data = selrec[selrec$target == 0.2,], aes(x=lon, y=lat, fill = Urea))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   scale_fill_viridis_c(direction = -1, option = "viridis",
                        breaks = seq(0, 250, 50),
                        limits = c(0, 250), 
                        oob = scales::squish) +
   ggtitle("Urea [kg/ha]") +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_blank(),
         legend.text = element_text(size=18),
         plot.title = element_text(size=18, face="bold"))
 
 ggDAP <- ggplot()+
   geom_tile(data = selrec[selrec$target == 0.2,], aes(x=lon, y=lat, fill = DAP))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   scale_fill_viridis_c(direction = -1, option = "rocket",
                        breaks = seq(0, 250, 50),
                        limits = c(0, 250), 
                        oob = scales::squish) +
   ggtitle("DAP [kg/ha]") +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_blank(),
         legend.text = element_text(size=18),
         plot.title = element_text(size=18, face="bold"))
 
 
 ggNPK <- ggplot()+
   geom_tile(data = selrec[selrec$target == 0.2,], aes(x=lon, y=lat, fill = NPK171717))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   scale_fill_viridis_c(direction = -1, option = "mako",
                        breaks = seq(0, 250, 50),
                        limits = c(0, 250), 
                        oob = scales::squish) +
   ggtitle("NPK 17:17:17 [kg/ha]") +
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_blank(),
         legend.text = element_text(size=18),
         plot.title = element_text(size=18, face="bold"))
 
 ggt <- gridExtra::grid.arrange(ggDAP, ggNPK, ggUrea, nrow=1)
 
 
 summary(lm(Urea ~ district, data = selrec[selrec$target == 0.2,]))
 
 
 selrec %>% 
   filter((refY == "Medium" & AEZ == "Congo-Nile watershed divide") | (refY == "High" & AEZ != "Congo-Nile watershed divide"),
          target %in% c(0, 0.2)) %>%
   group_by(target, district) %>%
   summarise(DAP = median(DAP),
             NPK171717 = median(NPK171717),
             Urea = median(Urea)) %>%
   print(n = 34)
 
 
 ###########################################################
 # GENERATING FERTILIZER PACKAGES FOR VALIDATION EXERCISES #
 ###########################################################
 
 #subset scenario with 20% yield increase and median refY level:
 ssrec <- selrec %>% 
   filter((refY == "Medium" & AEZ == "Congo-Nile watershed divide") | (refY == "High" & AEZ != "Congo-Nile watershed divide"),
          target == 0.2)
 
 #using k-medoid clustering (more robust than k-means)
 tmp <- subset(ssrec, select=c(DAP, NPK171717, Urea))
 fviz_nbclust(tmp, pam, method = "wss")
 nclus <- 6
 pkg <- pam(tmp, nclus, stand = FALSE)
 ssrec$pkg.cluster <- pkg$cluster
 
 pkgs <- pkg$medoids %>%
   as.data.frame() %>%
   mutate(pkg.cluster = row_number(),
          pkg.col = rainbow(nclus),
          pkg.code = c("red", "yellow", "green", "cyan", "blue", "purple")) %>%
   rename(DAP.cluster = DAP,
          NPK171717.cluster = NPK171717,
          Urea.cluster = Urea)
 
 ssrec <- merge(ssrec, pkgs)
 
 #calculating variance captured by clusters:
 ssrec %>%
   mutate(ts = (Urea - mean(Urea))**2 + (DAP - mean(DAP))**2 + (NPK171717 - mean(NPK171717))**2,
          ms = (Urea - Urea.cluster)**2 + (DAP - DAP.cluster)**2 + (NPK171717 - NPK171717.cluster)**2) %>%
   summarise(var = 1 - sum(ms)/sum(ts))
 
 pkgs %>%
   gather(fertilizer, rate, DAP.cluster:Urea.cluster) %>%
   mutate(fertilizer = gsub(".cluster", "", fertilizer)) %>%
   ggplot(aes(x = fertilizer, y = as.factor(pkg.cluster), fill = rate)) + 
   geom_tile() + 
   scale_fill_viridis_c(direction = -1, option = "magma", guide = "none") +
   geom_text(aes(label = round(rate), colour = rate > 180), size = 9) +
   scale_color_manual(guide = "none", values = c("black", "white")) +
   scale_x_discrete(expand = c(0,0)) +
   scale_y_discrete(expand = c(0,0), limits = as.factor(6:1)) +
   theme_minimal() +
   theme(axis.title = element_blank(),
         axis.text = element_text(size = 20),
         legend.title = element_blank(),
         legend.text = element_text(size = 14))
 
 ggpkg <- ggplot()+
   geom_tile(data = ssrec, aes(x=lon, y=lat, fill = as.factor(pkg.cluster)))+
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
   scale_fill_manual(values = rainbow(nclus))+
   scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
   ylim(-2.9, -1.2)+
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_blank(),
         legend.text = element_text(size=18),
         plot.title = element_text(size=18, face="bold"))
 
 ssrec_odk <- ssrec %>%
   mutate(latr = round(lat * 100)/100,
          lonr = round(lon * 100)/100,
          lookup_key = paste0("E", lonr, "N", latr),
          DAP.cluster = round(DAP.cluster),
          NPK171717.cluster = round(NPK171717.cluster),
          Urea.cluster = round(Urea.cluster)) %>%
   dplyr::select(lookup_key, pkg.cluster, pkg.code, DAP.cluster, NPK171717.cluster, Urea.cluster) %>%
   rename(pkgNr = pkg.cluster,
          pkgCode = pkg.code,
          DAP = DAP.cluster,
          NPK = NPK171717.cluster,
          Urea = Urea.cluster)
 
 write.csv(ssrec_odk, "RwaSIS_fertilizer_packages.csv", row.names = FALSE)
 
 
 
 
 ## The fertilizer package
 
 fertPack <- read.csv("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Potato/result/inputDST_FRpotato.csv")
 head(fertPack)
 fertPack$lookup_key <- gsub("E", "", fertPack$lookup_key)
 fertPack$lookup_key <- gsub("N", "", fertPack$lookup_key)
 
 fertPack$season <- NA
 for(d in 1:nrow(fertPack)){
   fertPack$season[d] <- strsplit(fertPack$lookup_key[d], "s")[[1]][2]
   
 }
 
 fertPack$lookup_key <- gsub("s1", "", fertPack$lookup_key)
 fertPack$lookup_key <- gsub("s2", "", fertPack$lookup_key)
 
 fertPack$refYieldCalss <- NA
 for(d in 1:nrow(fertPack)){
   fertPack$refYieldCalss[d] <- strsplit(fertPack$lookup_key[d], "y")[[1]][2]
   
 }
 
 fertPack$lookup_key <- gsub("y1", "", fertPack$lookup_key)
 fertPack$lookup_key <- gsub("y2", "", fertPack$lookup_key)
 fertPack$lookup_key <- gsub("y3", "", fertPack$lookup_key)
 fertPack$lookup_key <- gsub("y4", "", fertPack$lookup_key)
 fertPack$lookup_key <- gsub("y5", "", fertPack$lookup_key)
 
 
 fertPack$longitude <- NA
 fertPack$latitude <- NA
 for(d in 1:nrow(fertPack)){
   fertPack$longitude[d] <- strsplit(fertPack$lookup_key[d], "-")[[1]][1]
   fertPack$latitude[d] <- strsplit(fertPack$lookup_key[d], "-")[[1]][2]
   
 }
 fertPack$latitude <- as.numeric( fertPack$latitude)*-1
 fertPack$longitude <- as.numeric( fertPack$longitude)
 
 countryShp <- geodata::gadm(country, level = 3, path='.')
 AEZ <- readOGR(dsn=paste(pathIn2, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")
 RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
 
 dd1 <- raster::extract(RW_aez, fertPack[, c("longitude", "latitude")])[, "Names_AEZs" ]
 dd2 <- raster::extract(countryShp, fertPack[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
 fertPack$Province <- dd2$NAME_1
 fertPack$District <- dd2$NAME_2
 fertPack$AEZ <- dd1
 
 
 fertPack$Season <- ifelse(fertPack$season == "1", "plantingJuly", "plantingJanuary")
 fertPack$refYieldClass <- ifelse(fertPack$refYieldCalss == "1", "above40_tha", 
                                  ifelse(fertPack$refYieldCalss == "2", "30_40_tha", 
                                  ifelse(fertPack$refYieldCalss == "3", "20_30_tha", 
                                         ifelse(fertPack$refYieldCalss == "4", "10_20_tha", "below10_tha"))))
 
 
 fertPack <- fertPack[, c("Province", "District", "AEZ", "Season", "refYieldClass", "longitude", "latitude", "rateUrea", "rateDAP", "rateNPK", "dY",  "totalCost","netRev")]
 
 names(fertPack) <- c("Province", "District", "AEZ", "Season", "refYieldClass", "longitude", "latitude", "Urea", "DAP", "NPK","expectedYieldReponse",  "totalFertilizerCost",  "netRevenue")
 fertPack <- fertPack[complete.cases(fertPack), ]
 
 write.csv(fertPack, "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Potato/result/FRpotato_APIinput.csv", row.names = FALSE)
 

 # Province, AEZ, district, GPS, Season, refYieldClass
 
 
 
 
 
 
 
 # aa <- py %>% left_join(ds_sdt %>% dplyr::select(TLID, expCode, season, refTreat) %>% unique())
 # 
 # aa[aa$TLID == "SATLRW480109938798", ]
 # 
 # 
 # diff_INS <- NULL
 # for(tids in unique(aa$TLID)){
 #   tdata <- aa[aa$TLID == tids, ]
 #   tdata$RQ_diff <- tdata[tdata$refTreat == TRUE, ]$Yq - tdata$Yq
 #   tdata$RF_diff <- tdata[tdata$refTreat == TRUE, ]$Yp - tdata$Yp
 #   tdata$medianINS_diff <- tdata[tdata$refTreat == TRUE, ]$Yn - tdata$Yn
 #   tdata$blup_diff <- tdata[tdata$refTreat == TRUE, ]$Yb - tdata$Yb
 #   diff_INS <- rbind(diff_INS, tdata)
 #   
 # }
 # 
 # diff_INS[diff_INS$TLID == "SATLRW480109938798", ]
 # 
 # ggplot(diff_INS, aes(RQ_diff, blup_diff))+
 #   geom_point(alpha = .33, shape = 16) + 
 #   geom_abline(slope= 1, intercept = 0) + 
 #   ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
 #   ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
 #                         formula = y ~ x, size = 6) +
 #   geom_text(data = diff_INS %>% 
 #               dplyr::summarise(rmse = sqrt(sum((blup_diff)**2)/nrow(diff_INS)),
 #                                value = -1,
 #                                Yb = 20),
 #             aes(label = paste0("rmse = ", round(rmse*100)/100)),
 #             size = 6, hjust = 1) +
 #   xlab("Yield difference (t/ha), soil INS from reverse QUEFTS") +
 #   ylab("BLUP yield difference to reference (t/ha)") +
 #   theme_gray() +
 #   theme(axis.title = element_text(size = 12, face="bold"),
 #         axis.text = element_text(size = 12),
 #         strip.text = element_text(size = 12, face="bold"))
 # 
 # ggplot(diff_INS, aes(RF_diff, blup_diff))+
 #   geom_point(alpha = .33, shape = 16) + 
 #   geom_abline(slope= 1, intercept = 0) + 
 #   ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
 #   ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
 #                         formula = y ~ x, size = 6) +
 #   xlab("Yield difference (t/ha), soil INS from Random forest") +
 #   ylab("BLUP yield difference to reference (t/ha)") +
 #   theme_gray()+
 #   theme(axis.title = element_text(size = 12, face="bold"),
 #         axis.text = element_text(size = 12),
 #         strip.text = element_text(size = 12, face="bold"))
 # 
 # 
 # ggplot(diff_INS, aes(medianINS_diff, blup_diff))+
 #   geom_point(alpha = .33, shape = 16) + 
 #   geom_abline(slope= 1, intercept = 0) + 
 #   ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
 #   ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
 #                         formula = y ~ x, size = 6) +
 #   xlab("Yield difference (t/ha), median soil INS from RQ") +
 #   ylab("BLUP yield difference to reference (t/ha)") +
 #   theme_gray()+
 #   theme(axis.title = element_text(size = 12, face="bold"),
 #         axis.text = element_text(size = 12),
 #         strip.text = element_text(size = 12, face="bold"))
 # 
 # 
 # 
 # py %>% left_join(ds_sdt %>% dplyr::select(TLID, expCode, season, refTreat) %>% unique()) %>%
 #   mutate(refY = ifelse(refTreat == TRUE, "Reference treatment", "Other treatments")) %>%
 #   ggplot(aes(x = Yp, y = Yb)) + 
 #   geom_point(alpha = .33, shape = 16) + 
 #   geom_abline(slope= 1, intercept = 0) + 
 #   ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
 #   ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
 #                         formula = y ~ x, size = 6) +
 #   xlab("\nPredicted tuber yield (t/ha) using RF predicted supply") +
 #   ylab("BLUP potato tuber yield (t/ha)\n") +
 #   xlim(0, 60)+
 #   ylim(0, 60)+
 #   theme_gray()+
 #   theme(axis.title = element_text(size = 12, face="bold"),
 #         axis.text = element_text(size = 12),
 #         strip.text = element_text(size = 12, face="bold"))
 # 
 # 
 # py %>% left_join(ds_sdt %>% dplyr::select(TLID, expCode, season, refTreat) %>% unique()) %>%
 #   mutate(refY = ifelse(refTreat == TRUE, "Reference treatment", "Other treatments")) %>%
 #   ggplot(aes(x = Yq, y = Yb)) + 
 #   geom_point(alpha = .33, shape = 16) + 
 #   geom_abline(slope= 1, intercept = 0) + 
 #   ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
 #   ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
 #                         formula = y ~ x, size = 6) +
 #   xlab("\nPredicted tuber yield (t/ha) using RQ predicted supply") +
 #   ylab("BLUP potato tuber yield (t/ha)\n") +
 #   xlim(0, 60)+
 #   ylim(0, 60)+
 #   theme_gray()+
 #   theme(axis.title = element_text(size = 12, face="bold"),
 #         axis.text = element_text(size = 12),
 #         strip.text = element_text(size = 12, face="bold"))
 # 
 # 
 # 
 # 
 # pyr1 <- py %>%
 #   gather(variable, value, Yq:Yb) %>%
 #   dplyr::group_by(TLID, N, P, K, variable,refTreat )
 # head(pyr1)
 # pyr1[pyr1$TLID == "SATLRW480109938798", ]
 # 
 # pyr2 <- pyr1 %>%
 #   dplyr::summarise(value = mean(value)) %>%
 #   ddply(.(TLID, variable), summarize, refY = mean(ifelse(refTreat == TRUE, value, NA), na.rm=TRUE)) %>%
 #   unique()
 # pyr2[pyr2$TLID == "SATLRW480109938798", ]
 # 
 # 
 # pyr3 <- pyr1[pyr1$refTreat == FALSE, ] 
 # pyr3 <- merge(pyr3, pyr2, by=c("TLID", "variable"))
 # pyr3$dY <- pyr3$refY - pyr3$value
 # 
 # 
 # 
 # pyr4 <- pyr3[pyr3$variable == "Yb" , ]
 # pyr4$diffBlup <- pyr4$dY
 # 
 # pyr5 <- pyr3[!pyr3$variable == "Yb", ]
 # 
 # pyr6 <- merge(pyr5[, c("TLID","variable", "dY")], pyr4[, c("TLID", "diffBlup")], by="TLID")
 # head(pyr6)
 # 
 # 
 # pyr6 <- pyr6 %>%
 #   mutate(variable = mapvalues(variable, 
 #                             from = c("Yq", "Yp", "Yn"),
 #                             to = c("supply from reverse QUEFTS", "supply by RF prediction", "simple medians for supply"))) %>%
 #   unique()
 # pyr3[pyr3$TLID == "SATLRW480109938798", ]
 # 
 #  ggplot(pyr6, aes(dY, diffBlup)) +
 #   geom_point(alpha=.33, shape=16) +
 #   facet_wrap(~variable) +
 #   # geom_text(data = pyr6 %>% 
 #   #             dplyr::group_by(variable) %>% 
 #   #             dplyr::summarise(rmse = sqrt(sum((dY - diffBlup)**2)/n()),
 #   #                              value = -1,
 #   #                              diffBlup = 20),
 #   #           aes(label = paste0("rmse = ", round(rmse*100)/100)),
 #   #           size = 6, hjust = 1) +
 #   xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
 #   ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
 #   geom_abline(intercept = 0, slope = 1) +
 #   ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
 #   ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
 #                         formula = y ~ x, size = 6, label.y = .975) +
 #   theme_gray()+
 #   theme(axis.title = element_text(size = 14, face="bold"),
 #         axis.text = element_text(size = 14),
 #         strip.text = element_text(size = 14, face="bold"))
 # 
 # 
 # 
 # 
 # 
 # pyr %>%
 #   #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") %>%
 #   mutate(variable = as.character(variable)) %>%
 #   ggplot(aes(x = value, y = Yb)) + 
 #   geom_point(alpha=.33, shape=16) +
 #   facet_wrap(~variable) +
 #   geom_text(data = pyr %>% 
 #               filter(variable != "Yo") %>% 
 #               dplyr::group_by(variable) %>% 
 #               dplyr::summarise(rmse = sqrt(sum((value - Yb)**2)/n()),
 #                         value = -1,
 #                         Yb = 20),
 #             aes(label = paste0("rmse = ", round(rmse*100)/100)),
 #             size = 6, hjust = 1) +
 #   xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
 #   ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
 #   geom_abline(intercept = 0, slope = 1) +
 #   ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
 #   ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
 #                         formula = y ~ x, size = 6, label.y = .975) +
 #   theme_gray()+
 #   theme(axis.title = element_text(size = 14, face="bold"),
 #         axis.text = element_text(size = 14),
 #         strip.text = element_text(size = 14, face="bold"))
 # 
 # 
 # 
 # 
 # ############################################################################################
 # # 8. Alternative approach: Predict yield directly with Random Forest a la Jordan & Camilla #
 # ############################################################################################
 # 
 # #Prepare dataset...
 # 
 # ds$season_AB <- ifelse(ds$season %in% c("2021A", "2022A"), "A", "B")
 # 
 # dsp <- ds %>%
 #   mutate(Y = blup) %>%
 #   group_by(TLID, season, N, P, K) %>%
 #   summarise(Y = mean(Y)) %>%
 #   ungroup() %>%
 #   mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
 #   dplyr::select(-season) %>%
 #   join(sdt) %>%
 #   join(ds_ref) %>%
 #   join(tdt) %>%
 #   na.omit()
 # 
 # 
 # 
 
 
   