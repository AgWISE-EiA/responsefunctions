#################################################################################################################
packages_required <- c("tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics", "lme4",
                       "MuMIn", "rgdal", "gridExtra", "ggspatial", "sf", "plyr","tidyr", "tools", "plotly")


# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

###############################
# 1. define path for input and output
###############################

country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"

pathIn <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Rice/raw/")
pathOut <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Potato/result/")
#get the data
ds <- readRDS(paste(pathIn, "validationData.RDS", sep=""))
nrow(ds)
ds %>% dplyr::select(intro.wrong_ENID_1, intro.wrong_ENID_1, intro.enumerator_ID, intro.wrong_ENID) %>% unique()
ds %>% dplyr::select(planting.plantingDetails.variety, planting.plantingDetails.variety_other) %>% unique()
ds %>% dplyr::select(intro.wrong_ENID_1, intro.wrong_ENID_1, intro.enumerator_ID, intro.wrong_ENID) %>% unique()
ds %>% dplyr::select(Variety3, crop) %>% unique()
names(ds)
### check differences in effect by varieties 
ds <- ds %>%
  dplyr:: filter(crop =="potatoIrish") %>%
  dplyr::rename(Date = today,
                enID = intro.wrong_ENID,
                hhID = intro.wrong_ID,
                lon = intro.longitude,
                lat = intro.latitude,
                event = intro.event,
                crop = crop,
                packageNr = packageNr,
                variety = planting.plantingDetails.variety,
                variety2 = planting.plantingDetails.variety_other,
                variety3 = Variety3,
                plantingDate = planting.plantingDetails.planting_date,
                fertOrg = fertilizer_org_used,
                plantDensity = planting.plantingDetails.plant_density,
                BR_area = plotDescription.plotLayout_BR.plot_area_control,
                IBR_area = plotDescription.plotLayout_AEZ.plot_area_aez,
                SSR_area = plotDescription.plotLayout_SSR.plot_area_ssr,
                BR_tubersMarkNr = cropManagement.Harvest.harvest.potato_harvest.tubersMark_Nr_BR,
                BR_tubersNonMarkNr = cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_Nr_BR,
                BR_tubersMarkFW = cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_BR,
                BR_tubersNonMarkFW = cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_BR,
                IBR_tubersMarkNr =  cropManagement.Harvest.harvest.potato_harvest.tubersMark_Nr_SSR1,
                IBR_tubersNonMarkNr = cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_Nr_SSR1,
                IBR_tubersMarkFW = cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR1,
                IBR_tubersNonMarkFW = cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_SSR1,
                SSR_tubersMarkNr =  cropManagement.Harvest.harvest.potato_harvest.tubersMark_Nr_SSR2,
                SSR_tubersNonMarkNr = cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_Nr_SSR2,
                SSR_tubersMarkFW = cropManagement.Harvest.harvest.potato_harvest.tubersMark_FW_SSR2,
                SSR_tubersNonMarkFW = cropManagement.Harvest.harvest.potato_harvest.tubersNonMark_SSR2) %>%
  dplyr::mutate(plantingDate = as.Date(plantingDate, format="%m/%d/%Y"),
                harvestDate = as.Date(Date, format="%m/%d/%Y"),
                Date = as.Date(Date, format="%m/%d/%Y"))%>%
  dplyr::mutate(packageNr = as.character(packageNr))%>%
  dplyr::mutate(LGP = harvestDate-plantingDate)%>%
  dplyr::select(lat,lon,enID,hhID,plantingDate,harvestDate,LGP,crop,packageNr,event,variety, variety2,variety3,BR_area,IBR_area,SSR_area,
                BR_tubersMarkNr,BR_tubersNonMarkNr,BR_tubersMarkFW,BR_tubersNonMarkFW, IBR_tubersMarkNr,IBR_tubersNonMarkNr,
                IBR_tubersMarkFW, IBR_tubersNonMarkFW, SSR_tubersMarkNr,SSR_tubersNonMarkNr, SSR_tubersMarkFW, 
                SSR_tubersNonMarkFW)%>%
  dplyr::mutate(lat = if_else(lat > 1, lat * -1, lat))
 
head(ds)
str(ds)

ds %>% dplyr::select(variety3, crop) %>% unique()
ds %>% dplyr::filter(variety3 =="n/a")%>%unique()
## info on variety:: at one hhID, the GPS readings vary by event so when the hhiD = n/a, it is not possible to link data across events, e.g variety data 
ds %>% dplyr::filter(hhID =="n/a")%>%unique()
ds_variety <- ds %>% 
  dplyr::filter(variety3 != "n/a") %>% 
  dplyr::select(c(variety3, enID,hhID))
# ds_variety$variety2 <- ifelse(ds_variety$variety != "other_variety" & ds_variety$variety2 == "n/a", ds_variety$variety, ds_variety$variety2)
ds_variety$enhhID <- paste(ds_variety$enID, ds_variety$hhID, sep="_")
ds_variety <- ds_variety %>% dplyr::select(c(enhhID, variety3)) %>% unique()
#ds_variety$variety3 <- gsub(" ", "", ds_variety$variety3)

names(ds)
## filter rows only with yield data
ds_filtr <- ds%>%
  dplyr::filter(BR_tubersMarkFW!= "n/a" | BR_tubersNonMarkFW != "n/a" | IBR_tubersMarkFW!= "n/a" | IBR_tubersNonMarkFW != "n/a" | SSR_tubersMarkFW!= "n/a" | SSR_tubersNonMarkFW != "n/a" ) %>%
  dplyr::select(-c(event, variety, variety2, variety3,plantingDate, harvestDate,LGP, crop)) %>% 
  unique()

## check how many data points by hhiD ## 
ds_filtr %>% dplyr::select(lat,lon, hhID) %>%  unique() %>% dplyr::count(hhID)

## use location to identify households instead of the hhiD, because there are some n/a in hhID
## add variety info
ds_filtr <- ds_filtr %>% 
  mutate(location = paste(lon, lat, sep="_"),
         enhhID = paste(enID, hhID, sep="_"))
ds_filtr <- merge(ds_filtr, ds_variety, by="enhhID", all.x = TRUE)

head(ds_filtr)
length(unique(ds_filtr$location)) ##136 validation trials 
unique(ds_filtr$BR_area)


## which varieties are used 
gg1 <- ggplot(ds_filtr, aes(x = variety3, fill = variety3)) +
  geom_bar(stat = "count", width = 0.5) +
  labs(title = "Total Number of trials by potato varieties",
       x = "Variety",
       y = "Count") +
    theme_minimal() +
            theme(axis.title = element_text(size = 12, face="bold"),
                  plot.title = element_text(hjust = 0.5, face = "bold"),
                  axis.text.x = element_text(angle=0, hjust=1, vjust=1, size=12),
            legend.position = "none")
gg1

ggsave(paste(pathOut,"Varieties.pdf"), gg1, width=8, height = 6)

## nr of trials per SSR package : the second package is tested in <5 trials, package 3, is most tested folowed by 1 and 4
ggpackage <- ggplot(ds_filtr, aes(x = packageNr, fill = packageNr)) +
  geom_bar(stat = "count", width = 0.5) +
  labs(title = "Total Number of trials by SSR package",
       x = "SSR package",
       y = "Count") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12, face="bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle=0, hjust=1, vjust=1, size=12),
        legend.position = "none")
ggpackage

ggsave(paste(pathOut,"SSR package trials.pdf"), ggpackage, width=8, height = 6)


head(ds_filtr)
ds_filtr <- ds_filtr %>%
  dplyr::mutate(across(c(
    BR_area, IBR_area, SSR_area,
    BR_tubersMarkNr, BR_tubersNonMarkNr, BR_tubersMarkFW, BR_tubersNonMarkFW,
    IBR_tubersMarkNr, IBR_tubersNonMarkNr, IBR_tubersMarkFW, IBR_tubersNonMarkFW,
    SSR_tubersMarkNr, SSR_tubersNonMarkNr, SSR_tubersMarkFW, SSR_tubersNonMarkFW), as.numeric)) 

names(ds_filtr) <- gsub("tubers", "potato", names(ds_filtr))
head(ds_filtr) 
ds_filtr[ds_filtr$BR_area > 100, ]

### map the validation location 
rwshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
rwshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
rwshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
rwshp3 <- st_as_sf(geodata::gadm(country, level = 3, path='.'))
rwshp4 <- st_as_sf(geodata::gadm(country, level = 4, path='.'))
rwlake <- st_read("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Rice/Landing/Lakes/RWA_Lakes_NISR.shp")
AEZ <- readOGR(dsn="~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Rice/Landing/AEZ",  layer="AEZ_DEM_Dissolve")
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
RW_aez <- RW_aez[RW_aez$Names_AEZs %in% c("Birunga","Buberuka highlands","Central plateau", "Congo-Nile watershed divide"),]
rwAEZ <- st_as_sf(RW_aez)

ggmap<- ggplot() +
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill = NA) + 
  geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
  geom_sf(data = rwlake, size = NA, fill = "lightblue") +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill = NA) +
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill = NA) + 
  geom_point(data = ds_filtr, aes(x = as.numeric(lon), y = as.numeric(lat), 
                                  shape = variety3, color = variety3), size = 3) +
  scale_shape_manual(values = c(0, 1, 2, 3, 7, 8, 9, 15, 23)) +
  scale_color_manual(values = c("red","green","blue","brown"),
                     guide = "none")  +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(fill = "Agroecological zone",   # Specify custom title for color legend
       shape = "Variety",
       color = NULL, 
       size = NULL) +
  theme(axis.text = element_text(size = 12),
        legend.position = "right",
        strip.text = element_text(size = 12, face = "bold"))

ggmap
ggsave(paste(pathOut,"Validation location map.pdf"), ggmap, width=8, height = 6)

## we shuld not have central plateau, it is congo nile watershade
### trial are located in such a clustered fashion, the spatial variation will not be captured in the validation data properly.  
  
## yield distribution by treatment 
par(mfrow=c(2,2))
hist(ds_filtr$BR_area,main="BR area")
hist(ds_filtr$IBR_area, main="IBR area")
hist(ds_filtr$SSR_area, main="SSR area")

## area data errors
##put value above 30 to 23.04, because the yield data is not any different from similar areas
ds_filtr[ds_filtr$BR_area>30, ]
ds_filtr[ds_filtr$IBR_area>30, ]

ds_filtr$BR_area <- ifelse(ds_filtr$BR_area > 30, 23.04, ds_filtr$BR_area)
ds_filtr$IBR_area <- ifelse(ds_filtr$IBR_area > 30, 23.04, ds_filtr$IBR_area)
hist(ds_filtr$BR_area,main="BR area")
hist(ds_filtr$IBR_area, main="IBR area")
hist(ds_filtr$SSR_area, main="SSR area")

### try to see if there is effect of treatment in nr of roots and FW 

par(mfrow=c(3,2))
hist(ds_filtr$BR_potatoMarkNr, main="BR marketable", xlim=c(0,2000))
hist(ds_filtr$BR_potatoNonMarkNr, main="BR not marketable", xlim=c(0,1200))
hist(ds_filtr$IBR_potatoMarkNr, main="IBR marketable", xlim=c(0,2000))
hist(ds_filtr$IBR_potatoNonMarkNr, main="IBR not marketable", xlim=c(0,1200))
hist(ds_filtr$SSR_potatoMarkNr, main="SSR marketable", xlim=c(0,2000))
hist(ds_filtr$SSR_potatoNonMarkNr, main="SSR not marketable", xlim=c(0,1200))


hist(ds_filtr$BR_potatoMarkFW, main="BR marketable FW", xlim=c(0,80))
hist(ds_filtr$BR_potatoNonMarkFW, main="BR not marketable FW", xlim=c(0,50))
hist(ds_filtr$IBR_potatoMarkFW, main="IBR marketable FW", xlim=c(0,80))
hist(ds_filtr$IBR_potatoNonMarkFW, main="IBR not marketable FW", xlim=c(0,50))
hist(ds_filtr$SSR_potatoMarkFW, main="SSR marketable FW", xlim=c(0,80))
hist(ds_filtr$SSR_potatoNonMarkFW, main="SSR not marketable FW", xlim=c(0,50))

## Are there differences in the ratio of number of marketable to not marketable roots
ds_filtr2 <- ds_filtr
ds_filtr2$MNoM_ratioBR <- ds_filtr2$BR_potatoMarkNr/ds_filtr2$BR_potatoNonMarkNr
ds_filtr2$MNoM_ratioIBR <- ds_filtr2$IBR_potatoMarkNr/ds_filtr2$IBR_potatoNonMarkNr
ds_filtr2$MNoM_ratioSSR <- ds_filtr2$SSR_potatoMarkNr/ds_filtr2$SSR_potatoNonMarkNr
summary(ds_filtr2$MNoM_ratioBR)
summary(ds_filtr2$MNoM_ratioIBR)
summary(ds_filtr2$MNoM_ratioSSR)
# in average the ratio of marketable to none marketable nr of roots is highest for SSR followed by IBR, which 
# is good because in number of marketable roots, SSR and IBR are doing better


##  Are there differences in the ratio of fresh weight of marketable to not marketable roots
ds_filtr2$MNoM_FW_ratioBR <- ds_filtr2$BR_potatoMarkFW/ds_filtr2$BR_potatoNonMarkFW
ds_filtr2$MNoM_FW_ratioIBR <- ds_filtr2$IBR_potatoMarkFW/ds_filtr2$IBR_potatoNonMarkFW
ds_filtr2$MNoM_FW_ratioSSR <- ds_filtr2$SSR_potatoMarkFW/ds_filtr2$SSR_potatoNonMarkFW
summary(ds_filtr2$MNoM_FW_ratioBR)
summary(ds_filtr2$MNoM_FW_ratioIBR)
summary(ds_filtr2$MNoM_FW_ratioSSR)
## in average, in terms of ratio of marketable to non-marketable fresh wt, SSR is the best followed by BR 
# but on the higher half of the data, IBR is better than BR with SSR leading through the whole data range
# this can be an indication for on high potential area IBR is performing better than BR but on low potential area BR is better than IBR
# SSR is working the best both for low and high potential areas


## IBR_potatoNonMarkNr seem to have some extreme value and it seems to 
summary(ds_filtr$BR_potatoNonMarkFW )
summary(ds_filtr$IBR_potatoNonMarkFW )
summary(ds_filtr$SSR_potatoNonMarkFW )

ds_filtr[ds_filtr$IBR_potatoNonMarkFW > 70, ] ## 141 is almost 3* the ax FW in the data, while all other variables are not suggesting this
ds_filtr <- ds_filtr %>% dplyr::filter(IBR_potatoNonMarkFW < 70)


## there seems to be a very high marketable FW value for IBR, check letter 
# ds_filtr[ds_filtr$enhhID == "RSENRW000013_RSHHRW000118", ]
# ds_filtr[ds_filtr$IBR_potatoMarkNr > 1200, ]

hist(ds_filtr$BR_potatoMarkNr, main="BR marketable", xlab="", xlim=c(0,1200))
hist(ds_filtr$BR_potatoNonMarkNr, main="BR not marketable", xlab="", xlim=c(0,1200))
hist(ds_filtr$IBR_potatoMarkNr, main="IBR marketable", xlab="", xlim=c(0,1200))
hist(ds_filtr$IBR_potatoNonMarkNr, main="IBR not marketable", xlab="", xlim=c(0,1200))
hist(ds_filtr$SSR_potatoMarkNr, main="SSR marketable", xlab="", xlim=c(0,1200))
hist(ds_filtr$SSR_potatoNonMarkNr, main="SSR not marketable", xlab="", xlim=c(0,1200))


hist(ds_filtr$BR_potatoMarkFW, main="BR marketable FW", xlab="", xlim=c(0,100))
hist(ds_filtr$BR_potatoNonMarkFW, main="BR not marketable FW", xlab="", xlim=c(0,100))
hist(ds_filtr$IBR_potatoMarkFW, main="IBR marketable FW", xlab="", xlim=c(0,100))
hist(ds_filtr$IBR_potatoNonMarkFW, main="IBR not marketable FW", xlab="", xlim=c(0,100))
hist(ds_filtr$SSR_potatoMarkFW, main="SSR marketable FW", xlab="", xlim=c(0,100))
hist(ds_filtr$SSR_potatoNonMarkFW, main="SSR not marketable FW", xlab="", xlim=c(0,100))


ds_filtr <- ds_filtr %>% 
  mutate(BR_FW = BR_potatoMarkFW + BR_potatoNonMarkFW,
         IBR_FW = IBR_potatoMarkFW + IBR_potatoNonMarkFW,
         SSR_FW = SSR_potatoMarkFW + SSR_potatoNonMarkFW ) %>% 
  dplyr::select(-c(enID, hhID))
head(ds_filtr)


ds_potato <- ds_filtr %>% 
  tidyr::pivot_longer(cols = starts_with(c("BR", "IBR", "SSR")),
                      names_to = c("trialCode", ".value"),
                      names_sep = "_") %>%
  dplyr::mutate(Yield = (FW / area) * 10000/1000,
                Yield_market = (potatoMarkFW / area) * 10000/1000,
                Yield_NotMarket = (potatoNonMarkFW / area) * 10000/1000) %>% 
  unique()
head(as.data.frame(ds_potato))


###### 
pathIn2 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/Landing/", sep="")
AEZ <-  suppressMessages(suppressWarnings(readOGR(dsn=paste(pathIn2, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")))
RW_aez <- suppressWarnings(spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84")))
#RW_aez <- RW_aez[RW_aez$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]
rwAEZ <- st_as_sf(RW_aez)
names(rwAEZ)[names(rwAEZ) == "Names_AEZs"] <- "AEZ"
ds_potato1<- st_as_sf(ds_potato, coords = c("lon", "lat"), crs = st_crs(RW_aez))
ds_potato <- st_intersection(ds_potato1, rwAEZ )

head(as.data.frame(ds_potato))
#plot showing yield ranges by variety 


gg1_potato <- ds_potato %>%
  unique() %>% 
  ggplot(aes(x = trialCode, y = Yield, fill=as.factor(trialCode))) +
  geom_boxplot() +
  theme_bw()+
  ggtitle("\nPotato yield by AEZ and treatment [t/ha]")+
  ylab("potatot yield [t/ha]")+
  theme(axis.title.y = element_text(size = 12, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, face = "bold", size=14),
        legend.position = "none",
        strip.text = element_text(size = 12, face="bold", hjust=0))

gg1_potato
ggsave(paste(pathOut,"Potato_total_yield_treat.pdf"), gg1_potato, width=8, height = 6)
## without looking at the difference in AEZ, package or variety, the IBR better than BR, the IBR shows improvemnt over IBR


gg1_potato_packAEZ <- ds_potato %>%
  unique() %>% 
  ggplot(aes(x = trialCode, y = Yield, fill=as.factor(trialCode))) +
  geom_boxplot() +
  facet_wrap(~ AEZ) +
  theme_bw()+
  ggtitle("\nPotato yield by AEZ and treatment [t/ha]")+
  ylab("potatot yield [t/ha]")+
  theme(axis.title.y = element_text(size = 12, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, face = "bold", size=14),
        legend.position = "none",
        strip.text = element_text(size = 12, face="bold", hjust=0))
gg1_potato_packAEZ 
ggsave(paste(pathOut,"Potato_total_yield.pdf"), gg1_potato_packAEZ, width=8, height = 6)

## IBR seems to work well except in Buberuka where it will perform less than BR at some locations but still we need to look at the cost benefit analysis to see if IBR is < BR
## SSR effect cane be expected at Birunga and congo-Nile watershde



gg1_potato_Market <- ds_potato %>%
  unique() %>% 
  ggplot(aes(x = trialCode, y = Yield_market, fill=as.factor(trialCode))) +
  geom_boxplot() +
  facet_wrap(~ AEZ) +
  theme_bw()+
  ggtitle("\nPotato yield by AEZ and treatment [t/ha marketable root]")+
  ylab("potatot yield [t/ha]")+
  theme(axis.title.y = element_text(size = 12, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, face = "bold", size=14),
        legend.position = "none",
        strip.text = element_text(size = 12, face="bold", hjust=0))
gg1_potato_Market 
ggsave(paste(pathOut,"Potato_Marketable_yield.pdf"), gg1_potato_Market, width=8, height = 6)


gg1_potato_notMarket <- ds_potato %>%
  unique() %>% 
  ggplot(aes(x = trialCode, y = Yield_NotMarket, fill=as.factor(trialCode))) +
  geom_boxplot() +
  facet_wrap(~ AEZ) +
  theme_bw()+
  ggtitle("\nPotato yield by AEZ and treatment [t/ha not marketable root]")+
  ylab("potatot yield [t/ha]")+
  theme(axis.title.y = element_text(size = 12, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, face = "bold", size=14),
        legend.position = "none",
        strip.text = element_text(size = 12, face="bold", hjust=0))

gg1_potato_notMarket 
ggsave(paste(pathOut,"Potato_notMarketable_yield.pdf"), gg1_potato_notMarket, width=8, height = 6)


dev.off()

as.data.frame(ds_potato[ds_potato$enhhID ==  "RSENRW000013_RSHHRW000118", ]) ## this HH has a huge proportion of not marketable roots

# gg1 <- grid.arrange(gg1_potato, gg2_potato, ncol = 2)
# pdf("my_grid_output.pdf", width = 10, height = 8)
# grid.draw(gg1)
# dev.off() 


ggVarietyYield <- ds_potato %>%
  unique() %>% 
  ggplot(aes(x = trialCode, y = Yield, fill=as.factor(variety3))) +
  geom_boxplot() + 
  labs(fill = "Variety")+
  theme_bw()+
  ggtitle("\nPotato yield [t/ha]")+
  ylab("potatot yield [t/ha]")+
  theme(axis.title.y = element_text(size = 15, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right",
        strip.text = element_text(size = 14, face="bold", hjust=0))
ggVarietyYield
ggsave(paste(pathOut,"Potato_yield per variety.pdf"), ggVarietyYield, width=8, height = 6)
## what was the variety for the training data set? with Cruza, IBR is at least as good as BR and SSR is better than the other two
## THE difference between varieties can also be difference between AEZ, the two are not tested at the same location, see below 


ggVarietyYield_AEZ <- ds_potato %>%
  unique() %>% 
  ggplot(aes(x = trialCode, y = Yield, fill=as.factor(variety3))) +
  geom_boxplot() + 
  labs(fill = "Variety")+
  facet_wrap(~AEZ) +
  theme_bw()+
  ggtitle("\nPotato yield [t/ha]")+
  ylab("potatot yield [t/ha]")+
  theme(axis.title.y = element_text(size = 12, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right",
        strip.text = element_text(size = 12, face="bold", hjust=0))
ggVarietyYield_AEZ

ggsave(paste(pathOut,"Potato_yield per variety.pdf"), ggVarietyYield, width=8, height = 6)



########################################
## Compare yield by treat: BR and IBR are supposed to be similar, while SSR is expected to have more yield
## density plot to see the global distribution 
gg_density <- ds_potato %>%
  ggplot(aes(x = Yield,
             colour=paste0(trialCode, " (", trialCode, ")"),
             fill=paste0(trialCode, " (", trialCode, ")"))) +
  geom_density(alpha=.2, linewidth=1) +
  # facet_wrap(~AEZ) + 
  ggtitle("Potato yield density distribution") + 
  xlab("\npotato yield [t/ha]")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank())
gg_density
ggsave(paste(pathOut,"Density.pdf"), gg_density, width=8, height = 6)

## the BR and IBR are showing major overlap which is a good indication that in most cases they perform comparable
## SSR is shifted to the left indicting a higher performance for SSR over BR and IBR

# Bar Chart (mean yield)
gg3_potato<- ggplot(ds_potato, aes(x = trialCode, y = Yield)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("blue","red", "yellow"),width = 0.5) +
  ggtitle("Potato mean yield distribution") + 
  xlab("trialCode")+
  ylab("Average yield [t/ha]")+
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))
gg3_potato

ggsave(paste(pathOut,"Average_yield_barplot.pdf"), gg3_potato, width=8, height = 6)



ds_potato <- ds_potato %>% 
  mutate(variety3 = as.factor(variety3),
         trialCode = as.factor(trialCode),
         AEZ = as.factor(AEZ))

mod <- aov(Yield ~ trialCode * variety3 * AEZ, data = ds_potato)
anova(mod)
TukeyHSD(mod, conf.level=.95)$trialCode

## less treatment effect but larger variety and AEZ variations ?
## The effect of treatment varies by variety and by AEZ, this shows the level of tailoring made at AEZ level is not sufficient
## this can be improved by developing more site specific advisory.

ds_potato_summary <-ds_potato %>%
  group_by(trialCode) %>%
  summarize(mean_yield = mean(Yield, na.rm = TRUE),  # Calculate mean yield
    median_yield = median(Yield, na.rm = TRUE),  # Calculate median yield
    total_yield = sum(Yield, na.rm = TRUE))  # Calculate total yield


##################################################################
## look at the effect by AEZ, variety, ... 

## 1. by AEZ
gg_AEZ <- ds_potato %>%
  ggplot(aes(x = trialCode, y = Yield, fill=trialCode)) +
  geom_boxplot() +
  labs(fill = "Fertilizer\nrecommendation")+
  facet_wrap(~AEZ) + 
  ggtitle("Potato yield by AEZ") + 
  theme_bw()+
  ylab("\n Yield [t/ha]")+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12, face="bold"),
        axis.title.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text.x = element_text(size = 12))

gg_AEZ

ggsave(paste(pathOut,"Yield_AEZ.pdf"), gg_AEZ, width=10, height = 10)



## yield effect by trial

them2 <- theme(panel.background = element_rect(fill = "white"), # bg of the panel
               plot.background = element_rect(fill = "white", color = NA), # bg of the plot
               panel.grid.major = element_blank(),
               plot.title =element_text(color = "#CD7F32",size=13),
               strip.text.x = element_text(size = 10, color = "black", face = "bold"),
               axis.text=element_text(color = "black",face = "bold",size=10),
               axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
               axis.title=element_text(color = "black",size=12),
               legend.title = element_blank(),
               legend.text = element_text(size = 12),
               legend.background = element_rect(fill = "white"),
               panel.border = element_blank() ,
               axis.line.x = element_blank(),
               axis.line.y = element_blank())

ds_potato_wide <- ds_potato %>% 
  unique() %>% 
  dplyr::select(enhhID, location, variety3, packageNr, trialCode, Yield, AEZ) %>% 
  tidyr::spread(key = trialCode, value = Yield) %>% 
  mutate(IBR_BR = IBR - BR,
         SSR_BR = SSR - BR,
         SSR_BR20 = SSR - (BR + 0.2*BR)) %>% 
  as.data.frame()

head(ds_potato_wide)

## cumulative distribution plot of yield effects
dev.off()
plot(ecdf(ds_potato_wide$IBR_BR))
plot(ecdf(ds_potato_wide$SSR_BR))
plot(ecdf(ds_potato_wide$SSR_BR20))

ds_potato_wide <- ds_potato_wide %>% 
  mutate(AEZV = paste(AEZ, variety3, sep="-"))

ds_potato_wide <- ds_potato_wide %>% 
  mutate(AEZP = paste(AEZ, packageNr, sep="-"))

################# IBR versus BR


ggYieldEffectIBR<-ds_potato_wide %>% 
  dplyr::filter(IBR_BR > -10 & IBR_BR < 10) %>% 
  ggplot( aes(IBR_BR, colour = AEZ)) +
  geom_vline(xintercept = 0, linetype="dashed", col="grey2")+
  stat_ecdf(linewidth=1) +
  facet_wrap(~ AEZV,  ncol=4)+
  xlab("Yield difference [t/ha]")+ ylab("Percentage benefit(%)")+
  ggtitle("Potato - yield effect of (IBR - BR) by AEZ varieties")+
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))
ggYieldEffectIBR

ggsave(paste(pathOut,"Yield_effect IBR+AEZ.pdf"), ggYieldEffectIBR, width=10, height = 10)

### for IBR - BR: 
# farmers who get increase yield benefited twice from lower cost of fertilizer and yield increase: 75% in Birunga, and central plateaue
# from those who get lower yield with IBR, for some, the yield loss is compensated by fertilizer cost reduction and for others not
# yield effect and fertilizer cost should be translated to money to identify clearly where IBR is not working
# effect of fertilizer is dependent on varieties used  
## Sufficient data is available from Birunga and Buberuka with Kirundo varietiy 
## and Central plateau and Congo Nile with Cruza
## using Cruza, 70 - 60% of the farmers benefited twice
## using Kirundo, 75 - 50% farmers benefits twice


################ SSR versus BR 

ds_potato_wide %>% 
  dplyr::filter(IBR_BR > -10 & IBR_BR < 10) %>% 
  ggplot( aes(SSR_BR, colour = AEZ)) +
  geom_vline(xintercept = 0, linetype="dashed", col="grey2")+
  stat_ecdf(linewidth=1)+
  facet_wrap(~ AEZV,  ncol=4)+
  xlab("Yield difference [t/ha]")+ ylab("Percentage benefit(%)")+
  ggtitle("Yield effect of (SSR - BR) by AEZ")+
  theme_bw()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

## in Congo Nile delta, ~90% farmers get yield increase
## Birunga ~75%, Buberuka and central plateau ~60% get yield increase


################ SSR versus BR by SSR package 

ds_potato_wide %>% 
  dplyr::filter(IBR_BR > -10 & IBR_BR < 10) %>% 
  ggplot( aes(SSR_BR, colour = packageNr)) +
  geom_vline(xintercept = 0, linetype="dashed", col="grey2")+
  stat_ecdf(linewidth=1)+
  facet_wrap(~ AEZP,  ncol=4)+
  xlab("Yield difference [t/ha]")+ ylab("Percentage benefit(%)")+
  ggtitle("Fertilizer recommendation package yield effect (SSR - BR) by AEZ")+
  theme_bw()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))

## for Birunga you would go with package 4 and 6, for Buberuka use pacakge 3, and for 
## congo nile watershade use pacakge 3, 4 or 5



###############
##################################### when the SSR is compared with strictly 20% + BR  

ds_potato_wide %>% 
  dplyr::filter(IBR_BR > -10 & IBR_BR < 10) %>% 
  ggplot( aes(SSR_BR20, colour = variety3)) +
  geom_vline(xintercept = 0, linetype="dashed", col="grey2")+
  stat_ecdf(linewidth=1)+
  facet_wrap(~AEZV,  ncol=4) +
  xlab("Yield difference [t/ha]")+ ylab("Percentage benefit(%)")+
  ggtitle("Yield effect of (SSR - BR+20%) by AEZ - varieties")+
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))

## in best case 50% of the farmers realized a 20% yield increase in Buberuka 
## Birunga,


### get cost benfit
### cost 
potatoprice <- 480000 
fert1kg <- 675



## fertilizer cost: blanket  = 300 kg /ha of NPK 17:17:17 and 
## IBR has 50 kg DAP, 50 kf NPK17:17:17 and 100 kg Urea or 100 DAP, 100 NPK171717 and 150 Urea
## the prices used at simulations were as follows:
my_ferts <- data.frame(group = "synthetic", name = c("DAP", "Urea", "NPK"), 
                       N = c(18, 46, 17), P = c(20, 0, 7.4), K = c(0,0,14), 
                       Ca = 0, Mg = 0, S = 0, Mb = 0, Zn = 0, Bo = 0, Cu = 0,
                       price=c(722, 640,654))



ds_potato_wide_pac <- NULL
for (pack in unique(ds_potato_wide$packageNr)){
  packData <- ds_potato_wide[ds_potato_wide$packageNr == pack, ]
  if(pack == "1"){
    packData$fertcostSSR = (109*722) + (77 * 654) + (159 * 640)
  }else if(pack == "2"){
    packData$fertcostSSR = (74*722) + (98 * 654) + (141 * 640)
  }else if(pack == "3"){
    packData$fertcostSSR = (131*722) + (93 * 654) + (212 * 640)
  }else if(pack == "4"){
    packData$fertcostSSR = (131*722) + (236 * 654) + (175 * 640)
  }else if(pack == "5"){
    packData$fertcostSSR = (100*722) + (159 * 654) + (166 * 640)
  }else if(pack == "6"){
    packData$fertcostSSR = (134*722) + (197 * 654) + (336 * 640)
  }
  
  packData$fertcostBR <- 300 * 657
  packData$fertcostIBR <- (100*722) + (100 * 654) + (150 * 640)
  
  
  
  ds_potato_wide_pac <- rbind(ds_potato_wide_pac, packData)
  
}

ds_potato_wide_pac$fertcostIBR_BR <- ds_potato_wide_pac$fertcostIBR - ds_potato_wide_pac$fertcostBR
ds_potato_wide_pac$fertcostSSR_BR <- ds_potato_wide_pac$fertcostSSR - ds_potato_wide_pac$fertcostBR
ds_potato_wide_pac$fertcostSSR_IBR <- ds_potato_wide_pac$fertcostSSR - ds_potato_wide_pac$fertcostIBR

head(ds_potato_wide_pac)


## both IBR and SSR require more fertilizer use than BR
## Profit  =  can be computed with fertilizer cost reduced from the profit 
## for SSR _BR, add the cost of additional 50 kg fertilizer 
## TODO since the SSR has different rates by regions, the cost benefit can not be done easily. it packages location 
ds_potato_wide_pac <- ds_potato_wide_pac %>% 
  mutate(IBR_BR_potato = IBR_BR * potatoprice,
         SSR_BR_potato = SSR_BR * potatoprice,
         SSR_IBR_potato = SSR_BR20 * potatoprice,
         IBR_BR_profit = IBR_BR_potato - fertcostIBR_BR,
         SSR_BR_profit = SSR_BR_potato - fertcostSSR_BR,
         SSR_IBR_profit = SSR_IBR_potato - fertcostSSR_IBR)


## IBR_BR: 
nrow(ds_potato_wide_pac[ds_potato_wide_pac$IBR_BR >= 0,]) ## 88/135 =65% have yield increase 
nrow(ds_potato_wide_pac[ds_potato_wide_pac$IBR_BR_profit > 0,]) ## 87/135 = 64% made profit

dd1 <- ds_potato_wide_pac[ds_potato_wide_pac$IBR_BR >= 0,]
quantile(dd1$IBR_BR, probs=seq(0,1,0.1)) ## yield increase in percentage

dd2 <- ds_potato_wide_pac[ds_potato_wide_pac$IBR_BR_profit > 0,]
quantile(dd2$IBR_BR_profit, probs=seq(0,1,0.1)) ## yield increase in percentage

## SSR - BR
nrow(ds_potato_wide_pac[ds_potato_wide_pac$SSR_BR >= 0, ]) ## 103/135 = 76% hhId with min 20% yield increase
nrow(ds_potato_wide_pac[ds_potato_wide_pac$SSR_BR_profit > 0,]) ## 102/135 = 76% made profit

dd3 <- ds_potato_wide_pac[ds_potato_wide_pac$SSR_BR >= 0,]
quantile(dd3$SSR_BR, probs=seq(0,1,0.1)) ## yield increase in percentage

dd4 <- ds_potato_wide_pac[ds_potato_wide_pac$SSR_BR_profit > 0,]
quantile(dd4$SSR_BR_profit, probs=seq(0,1,0.1)) ## yield increase in percentage



## SSR - BR20
nrow(ds_potato_wide_pac[ds_potato_wide_pac$SSR_BR20 >= 0, ]) ## 53/135 = 0.39% hhId with min 20% yield increase with SSR as compared to BR
nrow(ds_potato_wide_pac[ds_potato_wide_pac$SSR_BR_profit > 0,]) ## 102/135 = 76% made profit even if the yield increase is not 20%



ds_potato_wide_pac %>% dplyr::filter(SSR_BR20 < 0) %>% 
  unique() %>% 
  dplyr::filter(SSR_BR > 0 & SSR_BR20 < 0) %>%  
  dplyr::filter( SSR_BR_profit >= 0) %>% 
  nrow()
## 49 hhID (37%) have yield increase < 20% but still made profit by using SSR

(53 + 49)/135 ## 76% hhiD benefiting from using SSR



## Does IBR give at least the same yield as BR
x <- ds_potato_wide$IBR_BR
xi <- x[x<0]
xj <- x[x>=0]
pos <- (length(xj)/length(x))*100
neg <- (length(xi)/length(x))*100
ds_IBRBR <- data.frame(labels = c("Yield Difference <br> (IBR yield - BR yield)", "Positive change", "Negative change"),
                       values = c(NA, pos, neg))

plot_ly(data = ds_IBRBR,
        labels = ~labels,
        values = ~values,
        parents = c("", "Yield Difference <br> (IBR yield - BR yield)", "Yield Difference <br> (IBR yield - BR yield)"),  # Adjusted parents based on the hierarchy
        type = "sunburst",
        branchvalues = 'total',
        textinfo = "label+percent entry") %>% 
  layout(title = 'Effects on potato yield of IBR vs BR')

## wrt profit
pos <- 65
neg <- 35
ds_IBRBR_profit <- data.frame(labels = c("Yield Difference <br> (IBR profit - BR profit)", "Profitable", "Not profitable"),
                       values = c(NA, pos, neg))

plot_ly(data = ds_IBRBR_profit,
        labels = ~labels,
        values = ~values,
        parents = c("", "Profit based <br> (IBR profit - BR profit)", "Profit based <br> (IBR profit - BR profit)"),  # Adjusted parents based on the hierarchy
        type = "sunburst",
        branchvalues = 'total',
        textinfo = "label+percent entry") %>% 
  layout(title = 'Profitability of using IBR vs BR')


## Does SSR gave higher yield than BR?

a <- ds_potato_wide$SSR_BR
ai <- a[a<0]
aj <- a[a>0]
pos <- (length(aj)/length(a))*100
neg <- (length(ai)/length(a))*100
ds_SSRBR <- data.frame(labels = c("Yield Difference <br> (SSR yield - BR yield)", "Positive change", "Negative change"),
                       values = c(NA, pos, neg))

plot_ly(data = ds_SSRBR,
        labels = ~labels,
        values = ~values,
        parents = c("", "Yield Difference <br> (SSR yield - BR yield)", "Yield Difference <br> (SSR yield - BR yield)"),  # Adjusted parents based on the hierarchy
        type = "sunburst",
        branchvalues = 'total',
        textinfo = "label+percent entry") %>% 
  layout(title = 'Effects on potato yield of SSR vs BR')


## if the SSR yield increase 20% + the BR yield?
Q <- ds_potato_wide$SSR_BR20
qi <- Q[Q<0]
qj <- Q[Q>0]
pos <- (length(qj)/length(Q))*100
neg <- (length(qi)/length(Q))*100
ds_SSRBR <- data.frame(labels = c("Yield Difference <br> (SSR yield - BR yield)", "Positive change", "Negative change"),
                       values = c(NA, pos, neg))

plot_ly(data = ds_SSRBR,
        labels = ~labels,
        values = ~values,
        parents = c("", "Yield Difference <br> (SSR - BR + 20Perc)", "Yield Difference <br> (SSR - BR + 20Perc)"),  # Adjusted parents based on the hierarchy
        type = "sunburst",
        branchvalues = 'total',
        textinfo = "label+percent entry") %>% 
  layout(title = 'Effects on potato yield of SSR vs BR')

### interms of profitability 
pos <- 76
neg <- 24
ds_SSRBR_profit <- data.frame(labels = c("Yield Difference <br> (SSR profit - BR profit)", "Profitable", "Not profitable"),
                       values = c(NA, pos, neg))

plot_ly(data = ds_SSRBR_profit,
        labels = ~labels,
        values = ~values,
        parents = c("", "Profit based <br> (SSR profit - BR profit)", "Profit based <br> (SSR profit - BR profit)"),  # Adjusted parents based on the hierarchy
        type = "sunburst",
        branchvalues = 'total',
        textinfo = "label+percent entry") %>% 
  layout(title = 'Effects on potato yield of SSR vs BR')





# #Calculate the actual economic benefit of each package
# #Add the package fertilizer rates for package 1-6
# ds_economic <- data.frame(packageNr = c("1", "2", "3", "4", "5", "6"),
#                           DAP_rate = c(109, 75, 131, 131, 100, 134),
#                           NPK_17_17_17_rate = c(77, 98, 93, 236, 159, 197),
#                           urea_rate = c(159, 141, 212, 175, 166, 336))
# 
# #Fertilizer cost in Rwanda per Kg
# fert1kg <- 675
# #Potato selling price/tonne
# potatoprice <- 480000
# #Obtain the actual cost of each fertilizer 
# ds_economic <- ds_economic %>%
# dplyr::mutate(DAP_cost = DAP_rate * fert1kg,
#               NPK_cost = NPK_17_17_17_rate * fert1kg,
#               Urea_cost = urea_rate * fert1kg,
#               SSR_cost = DAP_cost + NPK_cost + Urea_cost)
# 
# #Combine the filtered dataframes and append the calculated cost per package
# ds_economics <- merge(ds_potato_wide, ds_economic, by = "packageNr", all.x = TRUE)
# 
# 
# #Calculate the cost for the blanket and improved blanket recommendations
# BR_NPK_rate<-300
# BR_cost<-BR_NPK_rate*fert1kg
# IBR_NPK_rate<-350
# IBR_cost<-IBR_NPK_rate*fert1kg
# 
# ds_economics$BR_cost<-BR_cost
# ds_economics$IBR_cost<-IBR_cost
# 
# 
# # Exclude NA values and calculate mean packageFertCost by packageNr
# ds_yield_cost <- ds_economics %>%
#   filter(!is.na(SSR_cost)) %>%
#   group_by(AEZP) %>%
#   summarise(SSRcost = mean(SSR_cost),
#             BRcost = mean(BR_cost),
#             IBRcost = mean(IBR_cost),
#             yield_BR = mean(BR),
#             yield_IBR = mean(IBR),
#             yield_SSR = mean(SSR))%>%
# dplyr::mutate(YiGain_SSR = yield_SSR - yield_BR,
#               YiGain_IBR = yield_IBR - yield_BR,
#               grossReturn_SSR = YiGain_SSR * potatoprice,
#               grossReturn_IBR = YiGain_IBR * potatoprice,
#               increaseCost_SSR = SSRcost - BRcost,
#               increaseCost_IBR = IBRcost - BRcost,
#               ROI_SSR = (grossReturn_SSR - SSRcost)/increaseCost_SSR,
#               ROI_IBR = (grossReturn_IBR - IBRcost)/increaseCost_IBR)
# 
# view(ds_yield_cost)
# 
# saveRDS(ds_yield_cost,paste(pathOut,"EconomicReturnOnInvestment.RDS"))
# 
# ds_yield_cost_long <- ds_yield_cost %>%
#   pivot_longer(
#     cols = starts_with("ROI_"),
#     names_to = "trialCode",
#     values_to = "ROI"
#   )
# 
# # Create a box plot to compare the gain/loss from the IBR and SSR over the current blanket
# gg_ROI <- ds_yield_cost_long %>%
#   #dplyr::filter(ROI > -25 & ROI < 25) %>%
#   unique() %>% 
# ggplot(aes(x = trialCode, y = ROI)) +
#   geom_boxplot() +
#   facet_wrap(~AEZP,  ncol=5) +
#   labs(x = "Return on investment from IBR and SSR", y = "Return on investment gain/Loss(RWF)") +
#   ggtitle("Comparison of Return on investment gain/Loss from recommended fertilizer")
# 
# gg_ROI
# ggsave(paste(pathOut,"Return on investment AEZ & Package.pdf"), gg_ROI, width=10, height = 10)
# 
# gg1_ROI <- ds_yield_cost_long %>%
#   dplyr::filter(ROI > -25 & ROI < 25) %>%
#   unique() %>% 
#   ggplot(aes(x = trialCode, y = ROI, fill= trialCode)) +
#   geom_boxplot() +
#   labs(fill = "Fertilizer recommendation")+
#   theme_bw()+
#   ggtitle("Comparison of Return on investment gain/Loss from recommended fertilizer")+
#   ylab("Return on investment gain/Loss(RWF)")+
#   xlab("Return on investment from IBR and SSR")+
#   theme(axis.title.y = element_text(size = 15, face="bold"),
#         axis.title.x = element_text(size = 15, face="bold"),
#         #axis.title.x = element_blank(),
#         axis.text = element_text(size = 14),
#         plot.title = element_text(hjust = 0.5, face = "bold"),
#         legend.position = "right",
#         strip.text = element_text(size = 14, face="bold", hjust=0))
# gg1_ROI 
# dev.off()
# 
# ggsave(paste(pathOut,"Return on investment SSR vs IBR.pdf"), gg1_ROI, width=10, height = 10)
