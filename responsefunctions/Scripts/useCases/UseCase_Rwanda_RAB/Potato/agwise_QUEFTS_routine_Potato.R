
#################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
packages_required <- c("plyr", "tidyverse", "ggplot2", "foreach","doParallel",
                       "limSolve", "lpSolve", "Rquefts", "rgdal", "randomForest","ranger","Metrics")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))


### the previous procedures:
## 1. agwise-datacuration/dataops/datacuration/Scripts/useCases/UseCase_Rwanda_RAB/Potato/Compiling all potato fieldData Rwanda.R 
## 2. agwise-datacuration/dataops/datacuration/Scripts/useCases/UseCase_Rwanda_RAB/Potato/randomNoiseReduction_potato_RAB.R
## 3. agwise-datasourcing/dataops/datasourcing/Scripts/useCases/useCase_Rwanda_RAB/Potato/get_geoSpatialData_4ML_RAB_Potato.R



#################################################################################################################
# 2. read data and fit lmer to eliminate random error 
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

### read data with blups and effects and ref treat indication (without the geo-spatial data)
ds<- unique(readRDS(paste(pathIn, "compiled_fieldData.RDS", sep="")))
ds <- subset(ds, select=-c(N100, P100, K100))
str(ds)



### read yield data linked with soil and weather data

ML_train_Data <- unique(readRDS(paste(pathIn, "modelReady_trial.RDS", sep="")))

ML_train_Data <- ML_train_Data %>%
  mutate(province = NAME_1,
         district = NAME_2)

ML_train_Data <- ML_train_Data[!is.na(ML_train_Data$province), ]


# removing variables with unlikely predictive value:
sdt <- ML_train_Data %>%
  dplyr::select(-c(refY, N100, P100, K100,yieldEffectraw, yieldEffectBlup, FDID,plantingDate, harvestDate,
                   Names_AEZs, totalRF, nrRainyDays, Rain_month1, Rain_month2,Rain_month3, 
                   Rain_month4,Tmax_mean, Tmax_month1, Tmax_month2, Tmax_month3, Tmax_month4, Tmin_mean, Tmin_month1,
                   Tmin_month2, Tmin_month3,Tmin_month4,
                   AvRelativeHumidity, RH_month1, RH_month2, RH_month3, RH_month4, AvSolarRadiation,
                   solarRad_month1,solarRad_month2, solarRad_month3, solarRad_month4, slope, TPI,  TRI,
                   fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30,longitude , latitude, NAME_1, NAME_2)) %>%
  mutate(province = "Iburengerazuba", district = "Rubavu") %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         TLID = as.factor(TLID),
         expCode = as.factor(expCode),
         TY = Yield)%>%
  unique()
 
#select treatments with high nutrient rates (Increased NPK for RS-PFR-1, NPK_all for IFDC, NPK11 for SA-VAL-1):
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
  dplyr::select(-c(altitude, refYBLUP, Yield)) %>%
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

ds3 <- ds %>%
  select(-c(refY, refYBLUP, yieldEffectraw, yieldEffectBlup))



py %>% left_join(ds3 %>% dplyr::select(TLID, expCode, season) %>% unique()) %>%
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") %>%
  # mutate(refY = ifelse(N > 75 & P > 30 & K > 50, "Reference treatment", "Other treatments")) %>%
  mutate(refY = ifelse(refTreat == TRUE, "Reference treatment", "Other treatments")) %>%
  ggplot(aes(x = Yp, y = Yq)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
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
  summarise(value = mean(value)) %>%
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








aa <- py %>% left_join(ds_sdt %>% dplyr::select(TLID, expCode, season, refTreat) %>% unique())

aa[aa$TLID == "SATLRW480109938798", ]


diff_INS <- NULL
for(tids in unique(aa$TLID)){
  tdata <- aa[aa$TLID == tids, ]
  tdata$RQ_diff <- tdata[tdata$refTreat == TRUE, ]$Yq - tdata$Yq
  tdata$RF_diff <- tdata[tdata$refTreat == TRUE, ]$Yp - tdata$Yp
  tdata$medianINS_diff <- tdata[tdata$refTreat == TRUE, ]$Yn - tdata$Yn
  tdata$blup_diff <- tdata[tdata$refTreat == TRUE, ]$Yb - tdata$Yb
  diff_INS <- rbind(diff_INS, tdata)
  
}

diff_INS[diff_INS$TLID == "SATLRW480109938798", ]

ggplot(diff_INS, aes(RQ_diff, blup_diff))+
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  geom_text(data = diff_INS %>% 
              dplyr::summarise(rmse = sqrt(sum((blup_diff)**2)/nrow(diff_INS)),
                               value = -1,
                               Yb = 20),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 1) +
  xlab("Yield difference (t/ha), soil INS from reverse QUEFTS") +
  ylab("BLUP yield difference to reference (t/ha)") +
  theme_gray() +
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face="bold"))

ggplot(diff_INS, aes(RF_diff, blup_diff))+
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  xlab("Yield difference (t/ha), soil INS from Random forest") +
  ylab("BLUP yield difference to reference (t/ha)") +
  theme_gray()+
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face="bold"))


ggplot(diff_INS, aes(medianINS_diff, blup_diff))+
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  xlab("Yield difference (t/ha), median soil INS from RQ") +
  ylab("BLUP yield difference to reference (t/ha)") +
  theme_gray()+
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face="bold"))



py %>% left_join(ds_sdt %>% dplyr::select(TLID, expCode, season, refTreat) %>% unique()) %>%
  mutate(refY = ifelse(refTreat == TRUE, "Reference treatment", "Other treatments")) %>%
  ggplot(aes(x = Yp, y = Yb)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  xlab("\nPredicted tuber yield (t/ha) using RF predicted supply") +
  ylab("BLUP potato tuber yield (t/ha)\n") +
  xlim(0, 60)+
  ylim(0, 60)+
  theme_gray()+
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face="bold"))


py %>% left_join(ds_sdt %>% dplyr::select(TLID, expCode, season, refTreat) %>% unique()) %>%
  mutate(refY = ifelse(refTreat == TRUE, "Reference treatment", "Other treatments")) %>%
  ggplot(aes(x = Yq, y = Yb)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  xlab("\nPredicted tuber yield (t/ha) using RQ predicted supply") +
  ylab("BLUP potato tuber yield (t/ha)\n") +
  xlim(0, 60)+
  ylim(0, 60)+
  theme_gray()+
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face="bold"))




pyr1 <- py %>%
  gather(variable, value, Yq:Yb) %>%
  dplyr::group_by(TLID, N, P, K, variable,refTreat )
head(pyr1)
pyr1[pyr1$TLID == "SATLRW480109938798", ]

pyr2 <- pyr1 %>%
  dplyr::summarise(value = mean(value)) %>%
  ddply(.(TLID, variable), summarize, refY = mean(ifelse(refTreat == TRUE, value, NA), na.rm=TRUE)) %>%
  unique()
pyr2[pyr2$TLID == "SATLRW480109938798", ]


pyr3 <- pyr1[pyr1$refTreat == FALSE, ] 
pyr3 <- merge(pyr3, pyr2, by=c("TLID", "variable"))
pyr3$dY <- pyr3$refY - pyr3$value



pyr4 <- pyr3[pyr3$variable == "Yb" , ]
pyr4$diffBlup <- pyr4$dY

pyr5 <- pyr3[!pyr3$variable == "Yb", ]

pyr6 <- merge(pyr5[, c("TLID","variable", "dY")], pyr4[, c("TLID", "diffBlup")], by="TLID")
head(pyr6)


pyr6 <- pyr6 %>%
  mutate(variable = mapvalues(variable, 
                            from = c("Yq", "Yp", "Yn"),
                            to = c("supply from reverse QUEFTS", "supply by RF prediction", "simple medians for supply"))) %>%
  unique()
pyr3[pyr3$TLID == "SATLRW480109938798", ]

 ggplot(pyr6, aes(dY, diffBlup)) +
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  # geom_text(data = pyr6 %>% 
  #             dplyr::group_by(variable) %>% 
  #             dplyr::summarise(rmse = sqrt(sum((dY - diffBlup)**2)/n()),
  #                              value = -1,
  #                              diffBlup = 20),
  #           aes(label = paste0("rmse = ", round(rmse*100)/100)),
  #           size = 6, hjust = 1) +
  xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
  ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6, label.y = .975) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))





pyr %>%
  #filter(Nlim == "limiting", Plim == "non-limiting" & Klim == "non-limiting") %>%
  mutate(variable = as.character(variable)) %>%
  ggplot(aes(x = value, y = Yb)) + 
  geom_point(alpha=.33, shape=16) +
  facet_wrap(~variable) +
  geom_text(data = pyr %>% 
              filter(variable != "Yo") %>% 
              dplyr::group_by(variable) %>% 
              dplyr::summarise(rmse = sqrt(sum((value - Yb)**2)/n()),
                        value = -1,
                        Yb = 20),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 1) +
  xlab("\nPredicted potato tuber yield difference to reference treatment [t/ha]") +
  ylab("BLUP potato tuber yield difference to reference treatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6, label.y = .975) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))




############################################################################################
# 8. Alternative approach: Predict yield directly with Random Forest a la Jordan & Camilla #
############################################################################################

#Prepare dataset...

ds$season_AB <- ifelse(ds$season %in% c("2021A", "2022A"), "A", "B")

dsp <- ds %>%
  mutate(Y = blup) %>%
  group_by(TLID, season, N, P, K) %>%
  summarise(Y = mean(Y)) %>%
  ungroup() %>%
  mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
  dplyr::select(-season) %>%
  join(sdt) %>%
  join(ds_ref) %>%
  join(tdt) %>%
  na.omit()



