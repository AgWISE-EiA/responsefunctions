

#################################################################################################################
packages_required <- c("tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics", "lme4",
                       "MuMIn", "rgdal", "gridExtra", "ggspatial", "sf", "plyr")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))
#################################################################################################################


country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"

ds <- readRDS("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Potato/raw/compiled_potato_fertiliser_trial_data.RDS")
ds <- unique(ds)
str(ds)
summary(ds$TY)

#plot showing yield ranges by experiment and season:
ds %>%
  ggplot(aes(x = season, y = TY)) +
  geom_boxplot() +
  facet_wrap(~expCode, scales="free_y", ncol=1) +
  coord_flip()+
  theme_gray()+
  ylab("\nPotato tuber yield [t/ha]")+
  theme(axis.title.x = element_text(size = 15, face="bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))

#plot showing yield ranges by experiment and season:
ds %>%
  ggplot(aes(x = TY,
             colour=paste0(expCode, " (", season, ")"),
             fill=paste0(expCode, " (", season, ")")
  )) +
  geom_density(alpha=.2, linewidth=1) +
  facet_wrap(~expCode, scales="free_y", ncol=1) +
  theme_gray()+
  xlab("\nPotato tuber yield [t/ha]")+
  ylab("Density")+
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))

#plot showing variation in yield as affected by NPK rate by experiment and season:
ds %>%
  gather(nutrient, rate, N:K) %>%
  mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) %>%
  ggplot(aes(rate, TY)) + 
  geom_point(alpha=.33, shape=16) +
  facet_grid(nutrient ~ expCode+season) + 
  xlab("\nFertilizer nutrient application rate [kg/ha]") +
  ylab("Observed tuber yield [kg/ha]\n") +
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

#map with trial locations:
country <- "Rwanda"
rwshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
rwshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
rwshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
rwshp3 <- st_as_sf(geodata::gadm(country, level = 3, path='.'))
rwshp4 <- st_as_sf(geodata::gadm(country, level = 4, path='.'))
rwlake <- st_read("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/Landing/Lakes/RWA_Lakes_NISR.shp")
rwAEZ <- readOGR(dsn="~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/Landing/AEZ",  layer="AEZ_DEM_Dissolve")
rwAEZ <- rwAEZ[rwAEZ$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]
RW_aez <- spTransform(rwAEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
rwAEZ <- st_as_sf(RW_aez)
rwAEZ$Names_AEZs



ggplot()+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  #geom_sf(data = rwshp3[rwshp3$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.2, color = "white", fill=NA) + 
  geom_sf(data = rwshp2[rwshp2$ADM1_EN %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
  geom_point(data = ds, aes(x=as.numeric(lon), y=as.numeric(lat), shape = expCode, colour = expCode, size = expCode))+
  scale_shape_manual(values = c(15, 16, 18))+
  scale_size_manual(values = c(3,3,4))+
  scale_colour_manual(values = c("cornflowerblue", "blue", "blue4"))+
  scale_fill_manual(values = c("darkgoldenrod1", "darkgoldenrod", "burlywood"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=14, face="bold"))

#########################################
# 4. Fit lmer to eliminate random error #
#########################################

#create variables to deal with scale issues:
ds$N100 <- ds$N/100
ds$P100 <- ds$P/100
ds$K100 <- ds$K/100

#base model with independent parabolic response curves, fixed season effect, and random TL intercepts:
fit0 <- lmer(sqrt(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + season + (1|TLID), data=ds)
anova(fit0)
r.squaredGLMM(fit0)

#updated model allowing fixed two- and three-way interactions between N, P and K: 
fit1 <- update(fit0, . ~ . + N100:P100 + N100:K100 + P100:K100 + N100:P100:K100)
anova(fit1, fit0)
anova(fit1)
r.squaredGLMM(fit1)

#updated model adding random slopes:
fit2 <- update(fit1, . ~ . +(0 + N100|TLID) +(0 + P100|TLID) +(0 + K100|TLID))
anova(fit2, fit1)
anova(fit2)
r.squaredGLMM(fit2) 

ds$blup <- predict(fit2, ds)**2

#plot showing relationship between observations (with random error) and BLUPs (without random error)
ggplot(ds, aes(x = blup, y = TY)) + 
  geom_point(alpha=.33, shape=16) +
  geom_abline(intercept = 0, slope = 1) +
  stat_poly_line(formula = y ~ x, se = F) +
  stat_poly_eq(use_label(c("eq", "R2")),
               formula = y ~ x, size = 6)+
  xlab("\nBLUP tuber yield [t/ha]") +
  ylab("Observed tuber yield [t/ha]\n") +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14))

#plot illustrating that the elimination of random error results in more meaningful structure in yield response:
ds %>%
  gather(variable, value, c(TY, blup)) %>%
  group_by(TLID, variable) %>%
  mutate(refY = ifelse(N > 75 & P > 30 & K > 50, value, NA),
         refY = mean(refY, na.rm=TRUE),
         dY = refY - value,
         variable = factor(variable, levels=c("TY", "blup")),
         variable = mapvalues(variable,
                              from = c("TY", "blup"),
                              to = c("Raw observations", "BLUPs"))) %>%
  filter(!(N > 75 & P > 30 & K > 50)) %>%
  ggplot(aes(x = refY, y = dY)) + 
  #geom_point(alpha=.33, shape=16) + 
  geom_point(aes(shape = variable, alpha=0.5)) +
  scale_shape_manual(values = c(3, 16)) +
  facet_wrap(~variable) + 
  #facet_grid(expCode ~ variable) +
  geom_hline(yintercept = 0) +
  xlab("\nYield in reference treatment [Mg ha-1]") +
  ylab("Reference yield – treatments yield [Mg ha-1]\n") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")

ds %>%
  filter(TLID %in% sample(unique(ds$TLID), 12, replace = F)) %>%
  ggplot(aes(x = treat, y = blup)) +
  geom_point(size = 3) + 
  geom_point(aes(y = TY), shape = 3, size = 3) +
  facet_wrap(~TLID, scales = "free_x") + 
  ylab("Potato tuber yield [t/ha]\n") +
  theme_gray()+
  theme(axis.title.y = element_text(size = 14, face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))



ds2 <- NULL
for (tid in unique(ds$TLID)){
  print(tid)
  locdata <- droplevels(ds[ds$TLID == tid, ])
  locdata$refTreatment <- ifelse(locdata$N >=75 & locdata$P >=30 & locdata$K >= 50, TRUE, FALSE)
  ## for trials without the global referenceTreatment, use it own highest global NPK rate
  if(all(locdata$refTreatment == FALSE)){
    Nmax <- max(locdata$N)
    Pmax <- max(locdata$P)
    Kmax <- max(locdata$K)
    
    locdata$refTreatment <- ifelse(locdata$N == Nmax & locdata$P == Pmax & locdata$K == Kmax, TRUE, FALSE)
    ## if the trial does not have the highest NPK rate combination use higher N rate
    if(all(locdata$refTreatment == FALSE)){
      locdata$refTreatment <- ifelse(locdata$N == Nmax, TRUE, FALSE)
    }
  }
  wfd <- locdata[,c("treat", "N", "P", "K", "TY", "blup","refTreatment", "expCode", "TLID","season")]
  if(nrow(wfd[wfd$refTreatment == TRUE, ]) >1){
    print(tid)
  }
  
  refyield <- max(wfd[wfd$refTreatment == TRUE, "TY"])
  refyieldBlup <- max(wfd[wfd$refTreatment == TRUE, "blup"])
  wfd$refY <- refyield
  wfd$refYBLUP <- refyieldBlup
  wfd$yieldEffectraw <- ifelse(wfd$refTreatment == TRUE, 0,  refyield - wfd$TY )
  wfd$yieldEffectBlup <- ifelse(wfd$refTreatment == TRUE, 0,  refyieldBlup - wfd$blup )
  locdata <- merge(locdata, wfd, by=c("treat","N","P","K","TY", "blup","refTreatment", "expCode", "TLID","season"))
  ds2 <- rbind(ds2, locdata)
  
}
ds2 <- unique(ds2)


gg5 <- ggplot(ds2, aes(x = refY, y = yieldEffectraw)) + 
  geom_point(shape=3) +
  geom_hline(yintercept = 0) +
  facet_grid(expCode ~ .) + 
  xlab("\nYield in referenceTreatment [kg/ha]") +
  ylab("Yield difference relative to referenceTreatment [kg/ha]\n") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")
gg5


gg6 <- ggplot(ds2, aes(x = refY, y = yieldEffectBlup)) + 
  geom_point(shape=16) +
  geom_hline(yintercept = 0) +
  facet_grid(expCode ~ .) +  
  ylim(min(ds2$yieldEffectraw), max(ds2$yieldEffectraw))+
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")
gg6

#Remove duplicated Treatments at each trial location
ds2<- ds2 %>%
  arrange(desc(TY)) %>%
  distinct(treat, TLID, season, .keep_all = TRUE)

# #remove trial locations with less than 2 Treatments for reverseQUEFTS
ds2 <- ds2 %>%
  group_by(TLID) %>%
  filter(n() >= 2) %>%
  ungroup()

saveRDS(ds2, "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Potato/raw/compiled_fieldData.RDS")




#
# source("D:/workspace/SAnDMan/get_ONA_data.R")
# 
# 
# #######################################
# # 1. Get the potato data from SAnDMan #
# #######################################
# 
# #downloading the data
# wd <- "D:/workspace/SAnDMan"
# setwd(wd)
# creds <- scan(paste0(wd, "/pws.txt"), what = "character")
# user <- creds[1]
# pw   <- creds[2]
# 
# #get the list of all datasets of user...
# dss <- findONAdatasets(user = user, pw = pw)
# 
# #download and decompose the assign field/trial/plot data:
# id <- dss[dss$id_string == "Assign_FDTLPO",]$id
# ad <- getONAdata(user = user, pw = pw, id = id) 
# ad <- decomposeONAdata(ad)
# 
# #get the field identifiers
# af <- ad[[1]] %>%
#   filter(grepl("FD", entity)) %>%
#   dplyr::select(FDID2_new, FD_name_new, FD_owner, HHID, lat, lon) %>%
#   rename(FDID2 = FDID2_new,
#          FD_name = FD_name_new)
# 
# #get the trial identifiers
# at <- ad[[3]] %>%
#   join(ad[[1]] %>% dplyr::select(L1, entity, season, plantingDate, expCode)) %>%
#   filter(grepl("TL", entity),
#          L2 == "trial") %>%
#   dplyr::select(TLID2_new, TL_name_new, season, plantingDate, expCode) %>%
#   mutate(plantingDate = as.Date(plantingDate, format="%Y-%m-%d")) %>%
#   rename(TLID2 = TLID2_new,
#          TL_name = TL_name_new)
# 
# #download and decompose the potato plot level data:
# id <- dss[dss$id_string == "Measure_Potato_PO",]$id
# pd <- getONAdata(user = user, pw = pw, id = id) 
# pd <- decomposeONAdata(pd)
# 
# #get the potato plot yield data and merge with trial and field identifiers:
# ds1 <- pd[[3]] %>% #plot level data
#   filter(!is.na(tubersFW) | !is.na(tubersMarketableFW)) %>%
#   left_join(pd[[1]] %>% dplyr::select(L1, projectCode, FDID2, TLID2, today, start)) %>% #field level data
#   mutate(harvestDate = as.Date(today, format="%Y-%m-%d"),
#          start = as.POSIXct(gsub("\\+.*","", start), format="%Y-%m-%dT%H:%M:%S", tz="UTC")) %>%
#   dplyr::select(projectCode, FDID2, TLID2, POID2, POID2_label, start, harvestDate, plotLength, plotWidth, nrPlants, tubersFW, tubersMarketableFW) %>%
#   left_join(af) %>%
#   left_join(at)
# 
# #extracting treatment from label
# ds1 <- ds1 %>% 
#   mutate(treat = sub("_[^_]+$", "", POID2_label),
#          treat = gsub("_rep1", "", treat),
#          treat = gsub("_rep2", "", treat),
#          treat = gsub("_repA", "", treat),
#          treat = gsub("_repB", "", treat),
#          plotSize = as.numeric(plotLength) * as.numeric(plotWidth),
#          tubersFW = as.numeric(tubersFW),
#          tubersMarketableFW = as.numeric(tubersMarketableFW),
#          plantingDate = as.Date(plantingDate, format = "%Y-%m-%d")) %>%
#   filter(treat != "",
#          expCode != "RS-PLR-1") #removing lime trials without varying NPK rates
# 
# #correcting season entries
# ds1 <- ds1 %>%
#   mutate(season = ifelse(season %in% c("2222B", "B2022", "2022b", "2020B", "2022"), "2022B", season),
#          season = ifelse(season == "2020A", "2021A", season))
# 
# #correcting plotsize and calculating yield
# ds1 <- ds1 %>% 
#   mutate(plotSize = abs(plotSize),
#          plotSize = ifelse(plotSize>500, plotSize/100, plotSize),
#          plotSize = ifelse(plotSize>50, plotSize/10, plotSize)) %>%
#   group_by(TLID2) %>%
#   mutate(plotSize = median(plotSize)) %>%
#   group_by(POID2) %>%
#   filter(start == max(start)) %>% #only taking the last observation per POID
#   mutate(n = n()) %>%
#   filter(n == 1) %>% #drop all plots that have more than one yield observation
#   ungroup() %>%
#   mutate(TY = ifelse(is.na(tubersFW), tubersMarketableFW, tubersFW)/plotSize*10,
#          TY = ifelse(POID2 == "SAPORW756633027058", TY/10, TY)) %>% #correcting entry without decimal separator
#   left_join(read.csv("D:/workspace/SAnDMan/potato_trials_nutrient_rates.csv")) %>%
#   rename(FDID = FDID2,
#          TLID = TLID2) %>%
#   dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY) %>%
#   as.data.frame()
# 
# #adding and replacing plant and harvest dates from records by RAB staff for RS-PFR-1:
# phd <- read.csv("D:/workspace/RwaSIS/potato_trials_with_yield_data_2023-04-14_RwaSIS_PFR.csv") %>%
#   mutate(plantingDate_FB = as.Date(Planting.date, format="%d/%m/%Y"),
#          harvestDate_FB = as.Date(Harvest.date, format="%d/%m/%Y")) %>%
#   rename(FDID = FDID2,
#          TLID = TLID2) %>%
#   dplyr::select(FDID, TLID, plantingDate_FB, harvestDate_FB)
#   
# ds1 <- ds1 %>% left_join(phd) %>%
#   mutate(plantingDate = if_else(is.na(plantingDate_FB), plantingDate, plantingDate_FB),
#          harvestDate = if_else(is.na(harvestDate_FB), harvestDate, harvestDate_FB)) %>%
#   dplyr::select(-c(plantingDate_FB, harvestDate_FB)) %>%
#   #replace impossible harvest dates by the planting date + median duration of trials
#   mutate(harvestDate = if_else(is.na(plantingDate) | (as.numeric(harvestDate - plantingDate) < 150 & as.numeric(harvestDate - plantingDate) > 90), harvestDate, plantingDate + median(harvestDate - plantingDate)))
#   
# #########################################
# # 2. Preparing the RwaSIS season 1 data #
# #########################################
# 
# ds2 <- read.csv("D:/workspace/RwaSIS/rwasis-potato-fertiliser-all-data.csv")
# 
# ds2 <- ds2 %>%
#   filter(ds2$season == "2022A") %>% #removing the SAnDMan data which are 2022B data in ds1 for RS_PFR-1
#   rename(lon = gps_lon,
#          lat = gps_lat,
#          treat = treatment,
#          N = nfert_kgha,
#          P = pfert_kgha,
#          K = kfert_kgha,
#          TY = yield_tha,
#          FDID = farm_id) %>%
#   mutate(expCode = "RS-PFR-1",
#          FDID = paste0("RwaSIS_", FDID),
#          TLID = FDID,
#          plantingDate = as.Date(planting_date, format="%Y-%m-%d"),
#          harvestDate = as.Date(harvest_date, format="%Y-%m-%d")) %>%
#   mutate(harvestDate = if_else(is.na(plantingDate) | (as.numeric(harvestDate - plantingDate) < 150 & as.numeric(harvestDate - plantingDate) > 90), harvestDate, plantingDate + median(harvestDate - plantingDate))) %>%
#   dplyr::select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)
# 
# #####################################
# # 3. Preparing the IFDC potato data #
# #####################################
# 
# ds3 <- read.csv("D:/workspace/RwaSIS/IFDC_Rwanda potato 2014B season data subset.csv")
# ds3_nutrates <- read.csv("D:/workspace/RwaSIS/IFDC_Rwanda potato 2014B season treat nutrates.csv")
# ds3 <- ds3 %>%
#   gather(treat, TY, control:all_redK) %>%
#   mutate(season = "2014B",
#          expCode = "IFDC",
#          FDID = paste0("IFDC_", siteNr),
#          TLID = FDID, 
#          plantingDate = NA,
#          harvestDate = NA) %>%
#   join(ds3_nutrates) %>%
#   dplyr:: select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)
# 
# ds3[ds3$TLID == "IFDC_3",]$lon <- ds3[ds3$TLID == "IFDC_3",]$lon - 1 #wrong GPS entry 

#########################################
# 4. Fit lmer to eliminate random error #
#########################################

# ds <- rbind(ds1, ds2, ds3)

# write.csv(ds, "compiled_potato_fertiliser_trial_data.csv", row.names = FALSE)