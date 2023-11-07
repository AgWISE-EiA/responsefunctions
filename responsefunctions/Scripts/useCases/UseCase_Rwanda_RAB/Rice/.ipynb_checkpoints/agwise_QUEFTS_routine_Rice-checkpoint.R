
#################################################################################################################
# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
packages_required <- c("plyr", "tidyverse", "ggplot2", "foreach","doParallel",
                       "limSolve", "lpSolve", "Rquefts", "rgdal", "randomForest","ranger","Metrics")



# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))



#################################################################################################################
# 2. read data and fit lmer to eliminate random error 
#################################################################################################################
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Rice"
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/agwise_ML_routine.R")


pathIn1 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/", sep="")

pathIn <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")

pathOut1 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")

if (!dir.exists(pathOut1)){
  dir.create(file.path(pathOut1), recursive = TRUE)
}


###################################################################
# Explore yield response to nutrients, observing b treatment 
###################################################################
ds<- unique(readRDS(paste(pathIn, "compiled_fieldData.RDS", sep="")))
ds <- subset(ds, select=-c(N100, P100, K100))
ds$treat2 <- paste(ds$N, ds$P, ds$K, sep=":")
# head(ds)

ds$treat <- factor(ds$treat, levels=c("Increased_NPK", "NPK","NPK_120N","NPK_100N","NPK_40K",
                                           "NPK_60P", "NPK_45P","NPK_30P","NPK_17_3","NPK_20K",             
                                           "NK", "PK","NP","NPK_60N","Control"))

ds$treat2 <- factor(ds$treat2, levels=c("120:45:40", "160:25:70","120:15:28","100:15:28","80:15:40",
                                      "80:60:28", "80:45:28","80:30:28","80:15:28","80:15:20",             
                                      "160:0:70", "0:25:70","160:25:0","60:15:28","0:0:0"))
ggA <- ggplot(ds,aes(treat, blup))+
  geom_boxplot()+
  facet_wrap(~expCode+season, scales = "free")+
  ylab("BLUP tuber yield (t/ha)") + xlab("Treatment")+
  # ggtitle("Tuber yield by treatments")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))

ggsave(paste(pathOut1, "yieldTreat.pdf", sep=""), ggA, width = 10, height = 6)


ggB <- ggplot(ds,aes(treat, yieldEffectBlup))+
  geom_boxplot()+
  facet_wrap(~expCode+season, scales = "free")+
  ylab("BLUP yield effect (t/ha)") + xlab("Treatment")+
  # ggtitle("Tuber yield by treatments")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1))

ggsave(paste(pathOut1, "yieldEffectTreat.pdf", sep=""), ggB, width = 10, height = 6)


###################################################################
# read data linked to soil and weather data
###################################################################

ML_train_Data <- unique(readRDS(paste(pathIn, "modelReady_trial.RDS", sep="")))


ML_train_Data <- ML_train_Data %>%
  mutate(province = NAME_1,
         district = NAME_2)

ML_train_Data <- ML_train_Data[!is.na(ML_train_Data$province), ]


# removing common variables and soil variables with unlikely predictive value:
sdt <- ML_train_Data %>%
  dplyr::select(-c(N100, P100, K100, refY, refYBLUP, yieldEffectraw, yieldEffectBlup, plantingDate, harvestDate,
                   Names_AEZs, totalRF, nrRainyDays, Rain_month1, Rain_month2,Rain_month3, 
                   Rain_month4,Tmax_mean, Tmax_month1, Tmax_month2, Tmax_month3, Tmax_month4, Tmin_mean, Tmin_month1,
                   Tmin_month2, Tmin_month3,Tmin_month4,location,
                   AvRelativeHumidity, RH_month1, RH_month2, RH_month3, RH_month4, AvSolarRadiation,
                   solarRad_month1,solarRad_month2, solarRad_month3, solarRad_month4, slope, TPI,  TRI,
                   fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30,longitude , latitude, NAME_1, NAME_2)) %>%
  # mutate(province = "Iburengerazuba", district = "Rubavu") %>%
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

head(ds_ref)
#Topography and EAZ data:
tdt <- sdt %>%
  dplyr::select(c(expCode, TLID, altitude)) %>%
  mutate(alt = cut(altitude, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)))%>%
  unique()


ds_sdt <- sdt %>%
  left_join(tdt) %>%
  left_join(ds_ref) %>%
  dplyr::select(-c(altitude, Yield)) %>%
  unique()

saveRDS(ds_sdt, paste(pathOut1, "ds_forSupply.RDS", sep=""))


#############################
# 1. Running reverse QUEFTS #
#############################
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")

## get soil INS
supply <- NULL
for(i in unique(ds_sdt$TLID)){
  print(i)
   #subsetting and preparing data for revQUEFTS:
  dsi <- ds_sdt[ds_sdt$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y" #Aim is to explain variation in the BLUP yields

  if(unique(dsi$expCode) == "RS-RFR-1"){
    dsi$Y <- dsi$Y * 1000 * 0.82 #converting to drymatter kg / ha assuming 18 % moisture content 
  }else{
    dsi$Y <- dsi$Y * 1000 * 0.86  #converting to drymatter kg / ha 
  }
  
  
  #attainable yield is set to 20% above yield obtained with high NPK rate:
  # yy <- dsi[dsi$N > 75 & dsi$P > 30 & dsi$K > 50,]
  yy <- dsi[dsi$refTreat == TRUE, ]

    if(nrow(yy) == 0){
    print(i)
    yy <- dsi[dsi$N == max(dsi$N), ]
    }
  
  Yai <- mean(yy$Y) * 1.2 ### attainable yield is set at 20% more than the reference yield
  
  #at least 3 rows of data are needed + attainable yield:
  if(length(unique(dsi$treat)) > 2 & !is.na(Yai)){
    
    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = "Rice")
   
    #  if(unique(dsi$expCode) == "RS-RFR-1"){
    #   yaa <- Ya/0.82/1000 
    # }else{
    #   yaa <- Ya/0.86/1000 
    # }
    
    supply <- rbind(supply, data.frame(TLID = i,
                                       Ya = yaa,
                                       N_base_supply = si[1],
                                       P_base_supply = si[2],
                                       K_base_supply = si[3]))
  }
}



saveRDS(supply, paste(pathOut1, "soilINS_revQUEFTS.RDS", sep=""))
supply <- readRDS(paste(pathOut1, "soilINS_revQUEFTS.RDS", sep=""))
head(supply)


supply_RAB <- supply[supply$TLID %in% ds_sdt[ds_sdt$expCode == "RS-RFR-1", ]$TLID, ]
supply_AR <- supply[supply$TLID %in% ds_sdt[!ds_sdt$expCode == "RS-RFR-1", ]$TLID, ]

supply_RAB$Ya <- supply_RAB$Ya/0.82/1000 
supply_AR$Ya <- supply_AR$Ya/0.86/1000 

supply <- rbind(supply_RAB, supply_AR)



supply2 <- supply

###################################################
## use the supply and estimate yield estimate to validate QUEFTS
###################################################
ds_validate <- unique(ds_sdt[, c("treat", "N",  "P","K", "blup", "TLID", "expCode", "refTreat")])
ds_validate$index <- c(1:nrow(ds_validate))

supply_Qy <- NULL
for (i in unique(ds_validate$index)){
  print(i)
  tdata1 <- ds_validate[ds_validate$index == i, ]
  tdata2 <- supply2[supply2$TLID==tdata1$TLID, ]
  
  ## attainable yield in t/ha and dry wt.
  yya <- ifelse(tdata1$expCode == "RS-RFR-1", 
                ds_validate[ds_validate$TLID == tdata1$TLID & ds_validate$refTreat == TRUE, ]$blup * 1.2 * 0.82 * 1000, 
                ds_validate[ds_validate$TLID == tdata1$TLID & ds_validate$refTreat == TRUE, ]$blup * 1.2 * 0.86 * 1000)
  

  if(nrow(tdata2) > 0){
    tdata1$yieldQUEFTS <- runQUEFTS(nut_rates = data.frame(N=tdata1$N, P=tdata1$P, K=tdata1$K),
                          supply = c(tdata2$N_base_supply, tdata2$P_base_supply, tdata2$K_base_supply),
                          crop = Crop,
                          Ya = yya,
                          SeasonLength = SeasonLength)
    
    # from dry matter to fresh weight
    tdata1$yieldQUEFTS <- ifelse(tdata1$expCode == "RS-RFR-1", (tdata1$yieldQUEFTS / 0.82)/1000, (tdata1$yieldQUEFTS / 0.86)/1000)
    
    supply_Qy <- rbind(supply_Qy, tdata1)
  }
}

###yieldQUEFTS in supply_Qy is still in fresh wt in in t/ha


ds_sdt <- ds_sdt %>%
  left_join(supply_Qy[, c("treat","TLID", "expCode", "yieldQUEFTS")]) 

ggC <- ggplot(ds_sdt[!ds_sdt$expCode == "AfricaRice", ], aes(blup, yieldQUEFTS)) +
  geom_point() +
  geom_abline() +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6,
                        label.y = .975) +
  xlab("Observed yield (t/ha)") + ylab("QUEFTS predicted yield using soil INS (t/ha)") +
  theme_bw() +
  theme(axis.text= element_text(size=12))

ggsave(paste(pathOut1, "revQUEFTS_Yield_soilINS.pdf", sep=""), ggC, width = 6, height = 6)


ggD <- ggplot(ds_sdt[ds_sdt$expCode == "AfricaRice", ], aes(blup, yieldQUEFTS)) +
  geom_point() +
  geom_abline() +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6,
                        label.y = .975) +
  xlab("BLUP yield (t/ha)") + ylab("QUEFTS predicted yield using soil INS (t/ha)") +
  theme_bw() +
  theme(axis.text= element_text(size=12))

ggsave(paste(pathOut1, "revQUEFTS_Yield_soilINS_ARice.pdf", sep=""), ggD, width = 6, height = 6)




#############################
# 2. visualizing the soil INS, the 
#############################
supply <- readRDS(paste(pathOut1, "soilINS_revQUEFTS.RDS", sep="")) ##YA is in dry matter and in kg/ha
supply <- supply %>%
  select(-c(Ya))

summary(supply)
supply$N_base_supply <- ifelse(supply$N_base_supply > 200, 200, supply$N_base_supply)
supply$P_base_supply <- ifelse(supply$P_base_supply > 300, 300, supply$P_base_supply)
supply$K_base_supply <- ifelse(supply$K_base_supply > 400, 300, supply$K_base_supply)

INS <- supply %>%
  #adding lats and lons and data source:
  left_join(ds_sdt %>% dplyr::select(TLID, lat, lon, expCode, season) %>% unique()) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  # mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x))) %>%
  mutate(season_AB = ifelse(grepl("A", season), "A", "B")) %>%
  na.omit() %>% unique()


head(INS)


## Demonstrate ranges in supply by expCode and season combinations:
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
            facet_wrap(.~variable, scales="free") +
            theme_bw() +
            ylab("Indigenous nutrient supply (kg/ha)\n") +
            theme(axis.title.y = element_text(size = 15, face="bold"),
                  axis.title.x = element_blank(),
                  legend.text = element_text(size = 14),
                  legend.title = element_text(size = 14, face = "bold"),
                  legend.position = c(0.07, 0.9),
                  axis.text = element_text(size = 14,angle=45, hjust=1),
                  strip.text = element_text(size = 14, face="bold"))

    

  ggsave(paste(pathOut1, "INS_reverseQUEFTS.pdf", sep=""), gg1, width=10, height=6)
      
      
  ###########################################################
  ## assign trials to marshlands 
  ###########################################################
  marshlands <- read.csv(paste(pathIn1, "Rice_Marshland_GPS reference.csv", sep=""))
  
  marshlands <- marshlands %>%
    mutate(Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude))
  marshlands$Sector <- ifelse(is.na(marshlands$Sector), paste(marshlands$district, marshlands$Marshland, sep="_"), marshlands$Sector)
  marshlands[order(marshlands$Sector), ]
  marshlands$Sector <- ifelse(marshlands$Sector == "Muganza", paste(marshlands$Sector, marshlands$Marshland, sep="_"), marshlands$Sector)
  
  
  
  tlid_AEX <- unique(ds_sdt[, c("TLID", "AEZ", "lon", "lat", "expCode","province", "district")])
  
  dsMarshland <- NULL
  for(tid in unique(tlid_AEX$TLID)){
    print(tid)
    tdata <- tlid_AEX[tlid_AEX$TLID == tid, ]
    marshlands$latDiff <- abs(abs(tdata$lat) - abs(marshlands$Latitude))
    marshlands$lonDiff <- abs(abs(tdata$lon) - abs(marshlands$Longitude))
    marshlands$Diff <- marshlands$latDiff + marshlands$lonDiff
    wa <- marshlands[marshlands$Diff == min(marshlands$Diff), ]
    if(tdata$district == wa$District){
      tdata$Sector <- wa$Sector
      tdata$Marshland <- wa$Marshland.name
      tdata$sameDistrict <- TRUE
      dsMarshland <- rbind(dsMarshland, tdata)
    }else{
      tdata$Sector <- wa$Sector
      tdata$Marshland <- wa$Marshland.name
      tdata$sameDistrict <- FALSE
      dsMarshland <- rbind(dsMarshland, tdata)
    }
  }
  
head(dsMarshland)


INS_marshland <- INS %>%
  left_join(dsMarshland)

  #map with trial locations:
  
  rwshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
  rwshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
  rwshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
  rwshp3 <- st_as_sf(geodata::gadm(country, level = 3, path='.'))
  rwshp4 <- st_as_sf(geodata::gadm(country, level = 4, path='.'))
  rwlake <- st_read(paste(pathIn1, "Lakes/RWA_Lakes_NISR.shp", sep=""))
  AEZ <- readOGR(dsn=paste(pathIn1, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")
  RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
  RW_aez <- RW_aez[RW_aez$Names_AEZs %in% c("Granitic ridge", "Central plateau", "Eastern savana", 
                                            "Mayaga","Eastern plateau","Kivu  Lake borders", "Imbo"),]
  rwAEZ <- st_as_sf(RW_aez)
  
  
 ggD <- ggplot()+
    geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
    geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
    geom_sf(data = rwlake, size=NA, fill="lightblue")+
    geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) +
    geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
    geom_point(data = INS_marshland, aes(x=as.numeric(lon), y=as.numeric(lat), 
                                         colour = Sector, shape = AEZ, size=3))+
    # scale_size_manual(values = c(3,3)) +
     scale_shape_manual(values = c(0,1,2,3,7,8,9,15,23)) +
    scale_fill_manual(values = c(rep("grey3", 8))) +
    theme_bw()+
    xlab("Longitude")+
    ylab("Latitude")+
    theme(axis.title = element_blank(),
          axis.text = element_text(size=14),legend.position = "right",
          strip.text = element_text(size=14, face="bold"))
  
 ggsave(paste(pathOut1, "soilINS_aggregation.pdf", sep=""), ggD, width=10, height=6)
 

 
 
  
### aggregate soil INS by marshland and and by AEZ and by district and see how much different distribution there is
  ## with and without Africa Rice data
 supply <- readRDS(paste(pathOut1, "soilINS_revQUEFTS.RDS", sep="")) ##YA is in dry matter and in kg/ha
 supply <- supply %>%
   select(-c(Ya))
 
 summary(supply)
 supply$N_base_supply <- ifelse(supply$N_base_supply > 200, 200, supply$N_base_supply)
 supply$P_base_supply <- ifelse(supply$P_base_supply > 300, 300, supply$P_base_supply)
 supply$K_base_supply <- ifelse(supply$K_base_supply > 400, 300, supply$K_base_supply)
 
 
 dssdt_marshland <- ds_sdt %>%
   left_join(dsMarshland) %>%
   select(c(TLID, treat,blup, refTreat, expCode, lon, lat, AEZ, province, district, yieldQUEFTS, Sector, Marshland)) %>%
   left_join(supply)
 
 ###ds_sdt in blup and yieldQUEFTS in supply_Qy is in fresh wt and in in t/ha
 
 dssdt_marshland <- dssdt_marshland[!is.na(dssdt_marshland$N_base_supply), ]
 dssdt_marshland_Yatt <- unique(dssdt_marshland[dssdt_marshland$refTreat == TRUE & dssdt_marshland$expCode == "RS-RFR-1", ])
 dssdt_marshland_Yblanket <- unique(dssdt_marshland[dssdt_marshland$treat == "NPK_17_3", ])

 
  ## soil INS and attainable yield aggregated at sector level
  INS_ML_AEZ <- ddply(dssdt_marshland_Yatt, .(Sector, Marshland, AEZ), summarise,
                                soil_N = median(N_base_supply),
                                soil_P = median(P_base_supply),
                                soil_K = median(K_base_supply),
                                Y_att = round(median(blup), 2))  
  
write.csv(INS_ML_AEZ, paste(pathOut1, "INS_aggregated.csv", sep=""), row.names = FALSE)
  
 
## soil INS (will be the same as above) and blanket recommendation yield from QUEFTS using soil INS aggregated
blnaketyield_sector <- ddply(dssdt_marshland_Yblanket, .(Sector, Marshland, AEZ), summarise,
                    soil_N = median(N_base_supply),
                    soil_P = median(P_base_supply),
                    soil_K = median(K_base_supply),
                    Y_BR = round(median(yieldQUEFTS), 2))

write.csv(blnaketyield_sector, paste(pathOut1, "INS_BRyield_aggregated.csv", sep=""), row.names = FALSE)


  
###########################################################
## Calculate what yield the current blanket recommendation (4 bags of NPK/ha) 200 kg NPK 17*3 and 100 kg urea attain.
###########################################################
  # data.frame(type = c("DAP", "Urea", "NPK"), Ncont =c(18, 46, 17), Pcont=c(20, 0, 7.4), Kcont=c(0,0,14), price=c(722, 640,654))
  
  my_ferts <- data.frame(group = "synthetic", name = c("DAP", "Urea", "NPK"), 
                            N = c(18, 46, 17), P = c(20, 0, 7.4), K = c(0,0,14), 
                            Ca = 0, Mg = 0, S = 0, Mb = 0, Zn = 0, Bo = 0, Cu = 0,
                            price=c(722, 640,654))


  INS_ML_AEZ ## for soil INS and attainable yield
  blnaketyield_sector ## for blanket recommendation yield
  
  
  ### fertilizer recommendation by sector 
  fertrecom_sector <- NULL
  for(j in 1:nrow(INS_ML_AEZ)){
    fertperc <- NULL
    for(perc in seq(0, 0.5, 0.1)){
      fertrecom1 <-   rec_targetdY(my_ferts=my_ferts, 
                                   dY = perc, 
                                   target = "relative", 
                                   start = rep(0, nrow(my_ferts)), 
                                   supply = c(INS_ML_AEZ$soil_N[j], INS_ML_AEZ$soil_P[j], INS_ML_AEZ$soil_K[j]),
                                   att_GY = INS_ML_AEZ$Y_att[j], 
                                   GY_br = blnaketyield_sector$Y_BR[j],
                                   crop = "Rice",
                                   SeasonLength = 120,
                                   isBlanketRef = TRUE, 
                                   df_link  = blnaketyield_sector[j, ])
      fertperc <- rbind(fertperc, fertrecom1)
    }
    fertrecom_sector <- rbind(fertrecom_sector, fertperc)
  }
  
  fertrecom_sector$fertilizerCost <- fertrecom_sector$DAP * 722 + fertrecom_sector$Urea * 640 + fertrecom_sector$NPK_17_3 * 654
  head(fertrecom_sector)
  
  dd <- unique(dssdt_marshland[, c("AEZ", "province", "district", "Marshland","Sector")])
  f_ml <- merge(dd, fertrecom_sector, by=c("AEZ", "Marshland", "Sector"))
  
  
  marshlands <- read.csv(paste(pathIn1, "Rice_Marshland_GPS reference.csv", sep=""))
  marshlands <- marshlands %>%
    mutate(Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude))
  marshlands$Sector <- ifelse(is.na(marshlands$Sector), paste(marshlands$district, marshlands$Marshland, sep="_"), marshlands$Sector)
  marshlands[order(marshlands$Sector), ]
  marshlands$Sector <- ifelse(marshlands$Sector == "Muganza", paste(marshlands$Sector, marshlands$Marshland, sep="_"), marshlands$Sector)
  
  
  f_ml <- merge(f_ml, unique(marshlands[, c("Sector", "Latitude", "Longitude")]), by=c("Sector"))
  
  write.csv(f_ml, paste(pathOut1, "fertRecom_rice_Sector.csv"), row.names = FALSE)
  
  f_ml <- read.csv(paste(pathOut1, "fertRecom_rice_Sector.csv"))
  head(f_ml)
  
 ##  ### fertilizer recommendation by marshland  
  
  INS_ML_AEZ_ML <- ddply(INS_ML_AEZ, .(AEZ, Marshland), summarise, 
                 soil_N = median(soil_N),
                 soil_P = median(soil_P),
                 soil_K = median(soil_K), 
                 Y_att = median(Y_att))
  
  blnaketyield_sector_ML <- ddply(blnaketyield_sector, .(AEZ, Marshland), summarise, 
                                  soil_N = median(soil_N),
                                  soil_P = median(soil_P),
                                  soil_K = median(soil_K), 
                                  Y_BR = median(Y_BR))
  
  fertrecom_ml <- NULL
  for(j in 1:nrow(INS_ML_AEZ_ML)){
    fertperc <- NULL
    for(perc in seq(0, 0.5, 0.1)){
      fertrecom1 <-   rec_targetdY(my_ferts=my_ferts, 
                                   dY = perc, 
                                   target = "relative", 
                                   start = rep(0, nrow(my_ferts)), 
                                   supply = c(INS_ML_AEZ_ML$soil_N[j], INS_ML_AEZ_ML$soil_P[j], INS_ML_AEZ_ML$soil_K[j]),
                                   att_GY = INS_ML_AEZ_ML$Y_att[j], 
                                   GY_br = blnaketyield_sector_ML$Y_BR[j],
                                   crop = "Rice",
                                   SeasonLength = 120,
                                   isBlanketRef = TRUE, 
                                   df_link  = blnaketyield_sector_ML[j, ])
      fertperc <- rbind(fertperc, fertrecom1)
    }
    fertrecom_ml <- rbind(fertrecom_ml, fertperc)
  }
  
  
  fertrecom_ml$fertilizerCost <- fertrecom_ml$DAP * 722 + fertrecom_ml$Urea * 640 + fertrecom_ml$NPK_17_3 * 654
  head(fertrecom_ml)
  
  dd <- unique(dssdt_marshland[, c("AEZ", "province", "district", "Marshland")])
  dd_ML <- merge(dd, fertrecom_ml, by=c("AEZ", "Marshland"))
  
  write.csv(dd_ML, paste(pathOut1, "fertRecom_rice_Marshland.csv"), row.names = FALSE)
  dd_ML <- read.csv(paste(pathOut1, "fertRecom_rice_Marshland.csv"))
  head(dd_ML)
  
  
  
  ##  ### fertilizer recommendation by AEZ
  
  agg_AEZ <- ddply(INS_ML_AEZ, .(AEZ), summarise, 
                         soil_N = median(soil_N),
                         soil_P = median(soil_P),
                         soil_K = median(soil_K), 
                         Y_att = median(Y_att))
  
  blnaketyield_sector_AEZ <- ddply(blnaketyield_sector, .(AEZ), summarise, 
                                  soil_N = median(soil_N),
                                  soil_P = median(soil_P),
                                  soil_K = median(soil_K), 
                                  Y_BR = median(Y_BR))
  
  fertrecom_aez <- NULL
  for(j in 1:nrow(agg_AEZ)){
    fertperc <- NULL
    for(perc in seq(0, 0.5, 0.1)){
      fertrecom1 <-   rec_targetdY(my_ferts=my_ferts, 
                                   dY = perc, 
                                   target = "relative", 
                                   start = rep(0, nrow(my_ferts)), 
                                   supply = c(agg_AEZ$soil_N[j], agg_AEZ$soil_P[j], agg_AEZ$soil_K[j]),
                                   att_GY = agg_AEZ$Y_att[j], 
                                   GY_br = blnaketyield_sector_AEZ$Y_BR[j],
                                   crop = "Rice",
                                   SeasonLength = 120,
                                   isBlanketRef = TRUE, 
                                   df_link  = blnaketyield_sector_AEZ[j, ])
      fertperc <- rbind(fertperc, fertrecom1)
    }
    fertrecom_aez <- rbind(fertrecom_aez, fertperc)
  }
  
  
  fertrecom_aez$fertilizerCost <- fertrecom_aez$DAP * 722 + fertrecom_aez$Urea * 640 + fertrecom_aez$NPK_17_3 * 654
  head(fertrecom_aez)
  
  dd <- unique(dssdt_marshland[, c("AEZ", "province", "district")])
  dd_AEZ <- merge(dd, fertrecom_aez, by=c("AEZ"))
  write.csv(dd_AEZ, paste(pathOut1, "fertRecom_rice_AEZ.csv"), row.names = FALSE)
  dd_AEZ <- read.csv(paste(pathOut1, "fertRecom_rice_AEZ.csv"))
  head(dd_AEZ)
  dd_AEZ$blanketCost <- (200*654) + (100*640)
  dd_AEZ <- unique(subset(dd_AEZ, select=-c(province, district)))
  dd_AEZ$Y_BR <- round(dd_AEZ$Y_BR, digits=1)
  dd_AEZ$Y_BR <- round(dd_AEZ$Y_BR, digits=1)
  
  dd_AEZ_0 <- dd_AEZ[dd_AEZ$yieldPercinc == "0 %", ]
  dd_AEZ_20 <- dd_AEZ[dd_AEZ$yieldPercinc == "20 %", ]
  
  

  
  
  ##  ### fertilizer recommendation by district
  
  dd <- unique(dssdt_marshland[, c("AEZ", "province", "district", "Sector", "Marshland" )])
  INS_ML_AEZ_D <- merge(INS_ML_AEZ, dd, by=c("AEZ","Sector", "Marshland"))
  blnaketyield_sector_D <- merge(blnaketyield_sector, dd, by=c("AEZ","Sector", "Marshland"))
  
  
  agg_disrtict <- ddply(INS_ML_AEZ_D, .(district), summarise, 
                   soil_N = median(soil_N),
                   soil_P = median(soil_P),
                   soil_K = median(soil_K), 
                   Y_att = median(Y_att))
  
  blnaketyield_sector_district <- ddply(blnaketyield_sector_D, .(district), summarise, 
                                   soil_N = median(soil_N),
                                   soil_P = median(soil_P),
                                   soil_K = median(soil_K), 
                                   Y_BR = median(Y_BR))
  
  fertrecom_dist <- NULL
  for(j in 1:nrow(agg_disrtict)){
    print(j)
    fertperc <- NULL
    for(perc in seq(0, 0.5, 0.1)){
      print(perc)
      fertrecom1 <-   rec_targetdY(my_ferts=my_ferts, 
                                   dY = perc, 
                                   target = "relative", 
                                   start = rep(0, nrow(my_ferts)), 
                                   supply = c(agg_disrtict$soil_N[j], agg_disrtict$soil_P[j], agg_disrtict$soil_K[j]),
                                   att_GY = agg_disrtict$Y_att[j], 
                                   GY_br = blnaketyield_sector_district$Y_BR[j],
                                   crop = "Rice",
                                   SeasonLength = 120,
                                   isBlanketRef = TRUE, 
                                   df_link  = blnaketyield_sector_district[j, ])
      fertperc <- rbind(fertperc, fertrecom1)
    }
    fertrecom_dist <- rbind(fertrecom_dist, fertperc)
  }
  
  
  fertrecom_dist$fertilizerCost <- fertrecom_dist$DAP * 722 + fertrecom_dist$Urea * 640 + fertrecom_dist$NPK_17_3 * 654
  head(fertrecom_dist)
  
  dd <- unique(dssdt_marshland[, c("province", "district")])
  dd_dist <- merge(dd, fertrecom_dist, by=c("district"))
  write.csv(dd_dist, paste(pathOut1, "fertRecom_rice_district.csv"), row.names = FALSE)
  
  
  ### checking yield distribution 
  
  ## for 20% yield increase, rate of fertilizers 
  
  fert_prov <- ddply(dd_dist[dd_dist$yieldPercinc == "20 %", ], .(province), summarize, 
                     DAP = median(DAP),
                     Urea = median(Urea),
                     NPK = median(NPK_17_3))
  
  fert_dist <- ddply(dd_dist[dd_dist$yieldPercinc == "20 %", ], .(district), summarize, 
                     DAP = median(DAP),
                     Urea = median(Urea),
                     NPK = median(NPK_17_3))
  
  fert_AEZ <- ddply(dd_AEZ[dd_AEZ$yieldPercinc == "20 %", ], .(AEZ), summarize, 
                    DAP = median(DAP),
                    Urea = median(Urea),
                    NPK = median(NPK_17_3))
  
  fert_ML <- ddply(dd_ML[dd_ML$yieldPercinc == "20 %", ], .(Marshland), summarize, 
                   DAP = median(DAP),
                   Urea = median(Urea),
                   NPK = median(NPK_17_3))
  
  fert_sect <- ddply(f_ml[f_ml$yieldPercinc == "20 %", ], .(Sector), summarize, 
                     DAP = median(DAP),
                     Urea = median(Urea),
                     NPK = median(NPK_17_3))
  
  names(fert_prov)[1] <- "id"
  names(fert_dist)[1]<- "id"
  names(fert_AEZ)[1] <- "id"
  names(fert_ML)[1] <- "id"
  names(fert_sect)[1] <- "id"
  
  fert_prov$agglevel <- "Province"
  fert_dist$agglevel <- "District"
  fert_AEZ$agglevel <- "AEZ"
  fert_ML$agglevel <- "Marshland"
  fert_sect$agglevel <- "Sector"
  
  fertdf <-rbind(fert_prov, fert_dist, fert_AEZ, fert_ML, fert_sect)
  
  fertdf$agglevel <- factor(fertdf$agglevel, levels = c("Sector", "Marshland", "District", "AEZ", "Province"))
  
  fertdf_long <- gather(fertdf, fertilizers, rate, DAP:NPK)
  head(fertdf_long)
  
  ggfert <- ggplot(fertdf_long, aes(id, rate)) +
    geom_col(aes(fill=factor(agglevel)))+
    facet_grid(agglevel~fertilizers, scales="free")+
    ylab("Median fertilizer rates (kg/h)")+
    xlab("") +
    coord_flip() + 
    theme(axis.text.y = element_blank(), axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14), strip.text = element_text(size=12),
          legend.position = "none")
  
  
  ggsave(paste(pathOut1, "fert_medianrate.pdf"), ggfert, width=8, height=8)
  
  
  
  
  
  
  
  
  
  
  ## att yield
  vert_prov <- ddply(dd_dist, .(province), summarize, medblup = median(Y_att))
  vert_district <- ddply(dd_dist, .(district), summarize, medblup = median(Y_att))
  vert_AEZ <- ddply(dd_AEZ, .(AEZ), summarize, medblup = median(Y_att))
  vert_Marshland <- ddply(dd_ML, .(Marshland), summarize, medblup = median(Y_att))
  vert_Sector <- ddply(f_ml, .(Sector), summarize, medblup = median(Y_att))
  
  
  names(vert_prov)[1] <- "id"
  names(vert_district)[1]<- "id"
  names(vert_AEZ)[1] <- "id"
  names(vert_Marshland)[1] <- "id"
  names(vert_Sector)[1] <- "id"
  
  vert_prov$agglevel <- "Province"
  vert_district$agglevel <- "District"
  vert_AEZ$agglevel <- "AEZ"
  vert_Marshland$agglevel <- "Marshland"
  vert_Sector$agglevel <- "Sector"
  
  vertdf <-rbind(vert_prov, vert_district, vert_AEZ, vert_Marshland, vert_Sector)
  
  vertdf$agglevel <- factor(vertdf$agglevel, levels = c("Sector", "Marshland", "District", "AEZ", "Province"))
  
  ggmed <- ggplot(vertdf, aes(id, medblup)) +
    geom_col(aes(fill=factor(agglevel)))+
    facet_grid(agglevel~., scales="free")+
    ylab("Median attainable yield (t/h)")+
    xlab("") +
    coord_flip() + 
    theme(axis.text.y = element_blank(), axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14), strip.text.y = element_text(size=12),
          legend.position = "none")
  
  
  ggsave(paste(pathOut1, "Yatt_medianDist.pdf"), ggmed, width=8, height=8)
  
  
  
  
  
  


  

















  
  
  