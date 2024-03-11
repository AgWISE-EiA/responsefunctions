#################################################################################################################
# Sourcing required packages 
#################################################################################################################

#################################################################################################################
packages_required <- c("tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics", "lme4",
                       "MuMIn", "rgdal", "gridExtra", "ggspatial", "sf", "plyr")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Maize"


###############################
# 1. define path for input and output
###############################
pathIn <- paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")
pathIn2 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/", sep="")
pathOut1 <- paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_", useCaseName, "/", Crop, "/result/", sep="")
pathOut2 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")

# if (!dir.exists(pathIn)){
#   dir.create(file.path(pathIn), recursive = TRUE)
# }
# 
# if (!dir.exists(pathOut2)){
#   dir.create(file.path(pathOut2), recursive = TRUE)
# }
# 
# if (!dir.exists(pathIn2)){
#   dir.create(file.path(pathIn2), recursive = TRUE)
# }
# 
# if (!dir.exists(pathOut1)){
#   dir.create(file.path(pathOut1), recursive = TRUE)
# }
###############################
# 2. read all data fieldData #
###############################
###############################
# 1. Curate maize fieldData #
###############################
#get the data
ds <- readRDS(paste(pathIn, "aggregated_fieldData.rds", sep=""))# read the data assembled from Carob
OFRA<-readRDS(paste(pathIn, "OFRA_Rwanda.RDS", sep="")) # read the data from OFRA experiment
OFRA<-as.data.frame(OFRA)

OFRA <- OFRA %>%
  dplyr::mutate(Sector = ifelse(Sector %in% c("Busongo", "Bu"), "Busengo", Sector))# Correct for mismatching and misspelt sector names
colnames(OFRA)[6] <- "Farmer_name"
# create a unique ID for each trial by combining season, farmer name, sector, replicate and variety
OFRA <- OFRA %>%
  dplyr::mutate(Season = gsub(" ", "", Season)) %>%
  dplyr::mutate(TLID = paste(Season,Farmer_name,Sector,Rep,V, sep = "_")) %>%
  dplyr::mutate(Treatment = gsub("[^A-Za-z]", "",Treatment))%>%
  dplyr::filter(!(Treatment %in% c("Diagnostic")))

# Create centroid coordinates of the different sectors
library(terra)
library(sf)
#read the Rwanda level 3 shapefile 
sector <- readRDS("~/gadm/gadm41_RWA_3_pk.rds")
sector <- st_as_sf(sector)

# Sector names are in a column called "NAME_3"
centroids_df <- sector %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  setNames(c("longitude", "latitude")) %>%
  cbind(sector$NAME_2)

colnames(centroids_df) <- c("longitude", "latitude", "Sector")
selected_sectors <- c("Sake","Katabagemu", "Gashanda","Cyuve","Mareba","Rusarabuye","Busengo","Mbazi")
selected_df <- centroids_df %>%
  filter(Sector %in% selected_sectors)
selected_df<-selected_df[-5,]

# Print the resulting data frame
print(selected_df)

# Left join OFRA with selected sector location based on 'sector_names'
OFRA <- OFRA %>%
  left_join(selected_df %>% select(Sector, latitude, longitude), by = 'Sector')

# Rename the  columns to match with the data from Carob

OFRA <- OFRA %>%
  dplyr::rename(lon = longitude,
                lat = latitude,
                var = Variety,
                N = N,
                P = P,
                K = K,
                TY = GrainYld,
                TLID = TLID,
                Treatment=Treatment) %>%
  dplyr::select(TLID,lat,lon,Season,N,P,K,TY,Treatment,var)%>%
  dplyr::mutate(lat = if_else(lat > 1, lat * -1, lat))%>%
  dplyr::mutate(expCode = "OFRA",TY = TY * 1000)%>% # convert to kg/ha
  dplyr::mutate(K = as.numeric(K))%>%
  dplyr::mutate(Season = ifelse(Season %in% c("2015B", "2014B"), "SeasonB", Season),
                Season = ifelse(Season == "2015A", "SeasonA", Season))%>%
  dplyr::mutate(var = ifelse(var %in% c("Hybrid(SC513)"), "Hybrid SC513", var),
                var = ifelse(var %in% c("Hybrid(SC13)"), "Hybrid SC13", var),
                var = ifelse(var %in% c("ZM607"), "ZM 607", var),
                var = ifelse(var %in% c("Hybrid(SC513)"), "Hybrid SC513", var))

# rename and standardise the column names to match the data from OFRA
ds <- ds %>%
  dplyr::rename(lon = longitude,
                lat = latitude,
                var = variety,
                N = N_fertilizer,
                P = P_fertilizer,
                K = K_fertilizer,
                TY = yield,
                dsID = dataset_id,
                year = year,
                TLID = ID,
                plantingDate = planting_date,
                harvestDate = harvest_date, 
                Treatment= nut_response_eval) %>%
  dplyr::mutate(plantingDate = as.Date(plantingDate, format="%d/%m/%Y"),
                harvestDate = as.Date(harvestDate, format="%d/%m/%Y"))%>%
  dplyr::mutate(LGP = harvestDate-plantingDate)%>%
  dplyr::select(dsID,TLID,lat,lon,plantingDate,harvestDate,LGP,N,P,K,TY,Treatment,var)%>%
  dplyr::mutate(lat = if_else(lat > 1, lat * -1, lat))%>%
  dplyr::mutate(Season = factor(ifelse(format(plantingDate, "%m") %in% c("09", "10", "11", "12"), "SeasonA",
                                       ifelse(format(plantingDate, "%m") %in% c("01", "02", "03", "04", "05", "06"), "SeasonB", "SeasonC"))),
                Treatment = as.factor(Treatment),
                Season = as.factor(Season),
                var = as.factor(var),
                expCode = as.factor(ifelse(dsID == "doi_10.5061_dryad.fg15tg2", "OFRA1", 
                                           ifelse(dsID == "doi_10.7910_DVN_UNLRGC-fao", "FAO", 
                                                  ifelse(dsID == "doi_10.18167_DVN1_DLTQWR", "CIRAD", "RwaSIS")))),
                Maturity = factor(ifelse(LGP < 90, "Early", ifelse(LGP >= 90 & LGP <= 110, "Medium", "Late"))))%>%
  dplyr::select(TLID,lat,lon,Season,N,P,K,TY,Treatment,var,expCode)
# the other columns not selected can be used as reference for other research
#Verify if the names match in both datasets
names(ds)
names(OFRA)
ds<-rbind(ds,OFRA)

#Reconstruct the treatments
ds <- ds %>%
  dplyr::mutate(Treatment = case_when(
    N != 0 | P != 0 | K != 0 ~ paste0("N", N, ",P", P, ",K", K),
    TRUE ~ "control"
  ))

ds$Treatment <- as.factor(ds$Treatment)
unique(ds$Treatment)

#retain only the RWASIS and OFRA data that has unique IDs and precise location data
ds <- ds %>%
  filter(!(expCode %in% c("FAO", "CIRAD", "OFRA1")))
unique(ds$expCode)

# Summarize to view the number of trials per treatment in each experiment
df2 <- ds %>% 
  dplyr::group_by(TLID,Treatment,expCode) %>% 
  dplyr::summarise(total_count=n(),.groups = 'drop') %>% 
  dplyr::arrange(desc(total_count)) %>%
  as.data.frame()

dplyr::count(df2, Treatment)

# Summarize and explore data to view the number of trials per treatment in each experiment
df3 <- ds %>% 
  dplyr::group_by(Treatment,expCode) %>% 
  dplyr::summarise(total_count=n(),.groups = 'drop') %>% 
  dplyr::arrange(desc(total_count)) %>%
  as.data.frame()

df3

df3_2 <- ds %>% 
  dplyr::group_by(Treatment, Season) %>% 
  dplyr::summarise(total_count = n(), mean_TY = round(median(TY)), .groups = 'drop') %>% 
  dplyr::arrange(desc(mean_TY)) %>%
  as.data.frame()
df3_2

df4 <- ds %>%
  dplyr::group_by(expCode, N, P, K,TLID) %>%
  dplyr::summarize(combination_count = n())%>%
  as.data.frame()
df4

df4_1 <- ds %>%
  dplyr::filter(!(expCode %in% c("OFRA1", "RwaSIS"))) %>%
  dplyr::group_by(expCode, Treatment,TLID) %>%
  dplyr::summarize(combination_count = n())%>%
  as.data.frame()
df4_1

#plot showing yield ranges by variety and different data sources:
bplotyield <- ds %>%
  ggplot(aes(x = var, y = TY)) +
  geom_boxplot() +
  facet_wrap(~expCode, scales="free_y", ncol=1) +
  coord_flip()+
  theme_bw()+
  #theme_gray()+
  ylab("\nMaize yield [kg/ha]")+
  theme(axis.title.x = element_text(size = 15, face="bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))
bplotyield
ggsave(paste(pathIn, "Maize variety.pdf", sep=""), bplotyield, width=8, height=8)

#plot showing yield ranges byTreatment and different data sources:
bplotyield1 <- ds %>%
  ggplot(aes(x = Treatment, y = TY)) +
  geom_boxplot() +
  facet_wrap(~expCode, scales="free_y", ncol=1) +
  coord_flip()+
  theme_bw()+
  #theme_gray()+
  ylab("\nMaize yield [kg/ha]")+
  theme(axis.title.x = element_text(size = 15, face="bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))
bplotyield1
ggsave(paste(pathIn, "MaizeTreatment (grouped NPK).pdf", sep=""), bplotyield1, width=8, height=8)

#density plot showing yield ranges by experiment and season:

densityplot <- ds %>%
  ggplot(aes(x = TY,
             colour=paste0(expCode, " (", Season, ")"),
             fill=paste0(expCode, " (", Season, ")")
  )) +
  geom_density(alpha=.2, linewidth=1) +
  facet_wrap(~expCode, scales="free_y", ncol=1) +
  theme_gray()+
  xlab("\nMaize yield [kg/ha]")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))
densityplot
ggsave(paste(pathIn, "Maize yield density distribution.pdf", sep=""), densityplot, width=8, height=8)

#density plot showing yield ranges by experiment:
densityplot2 <- ds %>%
  ggplot(aes(x = TY,
             colour=paste0(expCode),
             fill=paste0(expCode)
  )) +
  geom_density(alpha=.2, linewidth=1) +
  facet_wrap(~expCode, scales="free_y", ncol=1) +
  theme_gray()+
  xlab("\nMaize yield [kg/ha]")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))
densityplot2
ggsave(paste(pathIn, "Maize yield density distribution2.pdf", sep=""), densityplot2, width=8, height=8)

#plot showing yield ranges by experiment and season:
gg1 <- ds %>%
  ggplot(aes(x = Season, y = TY)) +
  geom_boxplot() +
  facet_wrap(~expCode, scales="free_y", ncol=1) +
  coord_flip()+
  theme_gray()+
  ylab("\nMaize yield [kg/ha]")+
  theme(axis.title.x = element_text(size = 15, face="bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold", hjust=0))

gg1
ggsave(paste(pathIn, "Maize yield range(season).pdf", sep=""), gg1, width=8, height=8)


#plot showing variation in yield as affected by NPK rate by experiment and season:
gg2 <- ds %>%
  gather(nutrient, rate, N:K) %>%
  mutate(nutrient = factor(nutrient, levels=c("N", "P", "K"))) %>%
  ggplot(aes(rate, TY)) + 
  geom_point(alpha=.33, shape=16) +
  facet_grid(nutrient ~ expCode+Season) + 
  ggtitle("Yield distribution by expCode & Season")+
  xlab("\nFertilizer nutrient application rate [kg/ha]") +
  ylab("Observed Maize yield [kg/ha]\n") +
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"),
        plot.title = element_text(hjust = 0.5, size=16))
gg2
ggsave(paste(pathIn, "Maize yieldDist_season.pdf", sep=""), gg2, width=25, height=8)


#map with trial locations:

rwshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
rwshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
rwshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
rwshp3 <- st_as_sf(geodata::gadm(country, level = 3, path='.'))
rwshp4 <- st_as_sf(geodata::gadm(country, level = 4, path='.'))
rwlake <- st_read(paste(pathIn2, "Lakes/RWA_Lakes_NISR.shp", sep=""))
AEZ <- readOGR(dsn=paste(pathIn2, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
#RW_aez <- RW_aez[RW_aez$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]
rwAEZ <- st_as_sf(RW_aez)
rwAEZ$Names_AEZs
plot (AEZ)
colors <- c("#FF4500", "#8A2BE2", "#3CB371", "#FFD700", "#00FFFF", "#BA55D3", "#F08080", "#20B2AA", "#778899", "#FF69B4", "#00FF7F", "#4682B4", "#8B008B")

gg3 <- ggplot()+
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = rwAEZ, aes(fill = Names_AEZs)) +
  geom_sf(data = rwlake, size=NA, fill="darkblue")+
  geom_sf(data = rwshp3[rwshp3$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.2, color = "white", fill=NA) + 
  geom_sf(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.8, color = "black", fill=NA) + 
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
  geom_point(data = ds, aes(x=as.numeric(lon), y=as.numeric(lat), shape = expCode, colour = expCode, size = expCode))+
  scale_shape_manual(values = c(12, 12, 12,12))+
  scale_size_manual(values = c(1,1,1,1))+
  scale_colour_manual(values = c("green","red", "blue", "black"))+
  scale_fill_manual(values = colors)+#c("darkgoldenrod1", "darkgoldenrod", "burlywood","blue","cornflowerblue", "lightblue", "burlywood","blue","green", "red",
  #"orange","grey","red"))+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=14, face="bold"))

gg3
ggsave(paste(pathIn, "Maize trial_locations.pdf", sep=""), gg3, width=8, height=8)

###############################
# 4. fit linear mixed effects model
###############################

#create variables to deal with scale issues:
ds$N100 <- ds$N/100
ds$P100 <- ds$P/100
ds$K100 <- ds$K/100


#base model with independent parabolic response curves, fixed season effect, and random TL intercepts:
fit0 <- lmer(sqrt(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + Season + (1|TLID), data=ds)
anova(fit0)
r.squaredGLMM(fit0)
plot(fit0)

#updated model allowing fixed two- and three-way interactions between N, P and K: 
fit1 <- update(fit0, . ~ . + N100:P100 + N100:K100 + P100:K100 + N100:P100:K100)
anova(fit1, fit0)
anova(fit1)
r.squaredGLMM(fit1)
plot(fit1)

#updated model adding random slopes:
fit2 <- update(fit1, . ~ . +(0 + N100|TLID) +(0 + P100|TLID) +(0 + K100|TLID))
anova(fit2, fit1)
anova(fit2)
r.squaredGLMM(fit2) 
plot(fit2)

ds$blup <- predict(fit2, ds)**2


###############################
# 4. evaluate the effect of using linear mixed effects model
###############################

#plot showing relationship between observations (with random error) and BLUPs (without random error)
gg4 <- ggplot(ds, aes(x = blup, y = TY)) + 
  geom_point(alpha=.33, shape=16) +
  geom_abline(intercept = 0, slope = 1) +
  stat_poly_line(formula = y ~ x, se = F) +
  stat_poly_eq(use_label(c("eq", "R2")),
               formula = y ~ x, size = 6)+
  xlab("\nBLUP Maize yield [kg/ha]") +
  ylab("Observed Maize yield [kg/ha]\n") +
  ggtitle("Performance of linear mixed effect model")+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust = 0.5, size=16),
        strip.text = element_text(size=14, face="bold"))
gg4
ggsave(paste(pathIn, "LME_performance.pdf", sep=""), gg4, width=8, height=8)


#plot illustrating that the elimination of random error results in more meaningful structure in yield response:
ds %>%
  gather(variable, value, c(TY, blup)) %>%
  group_by(TLID, variable) %>%
  mutate(refY = ifelse(N >= 70 & P >= 15 & K >= 18, value, NA),
         refY = mean(refY, na.rm=TRUE),
         dY = refY - value,
         variable = factor(variable, levels=c("TY", "blup")),
         variable = mapvalues(variable,
                              from = c("TY", "blup"),
                              to = c("Raw observations", "BLUPs"))) %>%
  filter(!(N >= 70 & P >= 15 & K >= 18))

ds2 <- NULL
for (tid in unique(ds$TLID)){
  print(tid)
  locdata <- droplevels(ds[ds$TLID == tid, ])
  locdata$refTreatment <- ifelse(locdata$N >=70 & locdata$P >=15 & locdata$K >= 18, TRUE, FALSE)
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
  wfd <- locdata[,c("Treatment", "N", "P", "K", "TY", "blup","refTreatment", "expCode", "TLID","Season")]
  if(nrow(wfd[wfd$refTreatment == TRUE, ]) >1){
    print(tid)
  }
  
  refyield <- max(wfd[wfd$refTreatment == TRUE, "TY"])
  refyieldBlup <- max(wfd[wfd$refTreatment == TRUE, "blup"])
  wfd$refY <- refyield
  wfd$refYBLUP <- refyieldBlup
  wfd$yieldEffectraw <- ifelse(wfd$refTreatment == TRUE, 0,  refyield - wfd$TY )
  wfd$yieldEffectBlup <- ifelse(wfd$refTreatment == TRUE, 0,  refyieldBlup - wfd$blup )
  locdata <- merge(locdata, wfd, by=c("Treatment","N","P","K","TY", "blup","refTreatment", "expCode", "TLID","Season"))
  ds2 <- rbind(ds2, locdata)
  
}
ds2 <- unique(ds2)

str(ds2)

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
ggsave(paste(pathIn, "BLUP_reducingRandomError.pdf", sep=""), grid.arrange(gg5, gg6, ncol=2), width=12, height=8)

#Remove duplicated Treatments at each trial location
ds2<- ds2 %>%
  arrange(desc(TY)) %>%
  distinct(Treatment, TLID, Season, .keep_all = TRUE)

# #remove trial locations with less than 3 Treatments for reverseQUEFTS
ds2 <- ds2 %>%
  group_by(TLID) %>%
  filter(n() >= 3) %>%
  ungroup()

gg7<-ds2 %>%
  filter(TLID %in% sample(unique(ds2$TLID), 9, replace = F)) %>%
  ggplot(aes(x =Treatment, y = blup)) +
  geom_point(size = 3) + 
  geom_point(aes(y = TY), shape = 3, size = 3) +
  facet_wrap(~TLID, scales = "free_x") + 
  ylab("Maize yield [kg/ha]\n") +
  theme_gray()+
  theme(axis.title.y = element_text(size = 14, face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))

gg7

ggsave(paste(pathIn, "BLUP_performance_sample trials.pdf", sep=""), gg7, width=20, height=12)
###############################
# 5. write out results
###############################

saveRDS(ds2, paste(pathOut1, "compiled_fieldData.RDS", sep=""))
saveRDS(ds2, paste(pathOut2, "compiled_fieldData.RDS", sep=""))


#############################
# 5. Running reverse QUEFTS #
#############################

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
## set path and input country crop and use case 
#################################################################################################################
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Maize"
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/agwise_ML_routine.R")

pathIn <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")
pathOut1 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")
pathIn2 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/", sep="")
pathIn3<-paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")

if (!dir.exists(pathOut1)){
  dir.create(file.path(pathOut1), recursive = TRUE)
}

#################################################################################################################
## read the yield, soil, weather and AEZ data and merge
#################################################################################################################
library(raster)
## yield data
ds<- unique(readRDS(paste(pathIn, "compiled_fieldData.RDS", sep="")))
gpsPoints <- unique(ds[, c("lon", "lat")])
gpsPoints$x <- as.numeric(gpsPoints$lon)
gpsPoints$y <- as.numeric(gpsPoints$lat)
gpsPoints <- gpsPoints[, c("x", "y")]

AEZ <- readOGR(dsn='~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/Landing/AEZ/',  layer="AEZ_DEM_Dissolve")
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
datasoil <- as.data.frame(raster::extract(RW_aez, gpsPoints))
datasoil$lat <- gpsPoints$y
datasoil$lon <- gpsPoints$x

ds <- merge(ds, datasoil, by=c("lat", "lon"))
ds$AEZ <- ds$Names_AEZs
ds <- subset(ds, select=-c(id.y, Names_AEZs,AEZs_no))

ds$AEZ <- ifelse(is.na(ds$AEZ), "Mayaga", ds$AEZ)
unique(ds$AEZ)
#Alternative: run in parallel on multiple CPUs
#create a function to run revQUEFTS optimisation:
calculate_supply <- function(TLID){
  
  print(TLID)
  
  #subsetting and preparing data for revQUEFTS:
  dsi <- ds[ds$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y"
  dsi$Y <- dsi$Y* 0.845 #converting to kg DM/ha, assuming 84.5% moisture content
  
  #attainable yield is set to 20% above yield obtained with high NPK rate:
  #Yai <- max(dsi[dsi$N >= 70 & dsi$P >= 15 & dsi$K >= 18,]$Y) * 1.2
  Yai<-unique(max(dsi$Y))* 1.2
  #at least 3 rows of data are needed + attainable yield:
  if(length(unique(dsi$Treatment)) > 2 & !is.na(Yai)){
    
    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = "Maize")
    print(si)
    
    supply <- data.frame(TLID = i,
                         Ya = Yai,
                         N_base_supply = si[1],
                         P_base_supply = si[2],
                         K_base_supply = si[3])
  }
  
  return(supply)
  
}

## the supply Ya is still in dry matter and scaled to 20%

cls <- parallel::makePSOCKcluster(8)
doParallel::registerDoParallel(cls)
supply <- foreach(i = unique(ds$TLID)) %do% {calculate_supply(TLID = i)}
stopCluster(cls)
supply <- do.call(rbind, supply)

saveRDS(supply,   "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Nigeria_Maize/Maize/raw/compiled_Maize_fertiliser_trial_calculated_supply_afterlmer_sqrttf.RDS")
supply <- readRDS("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Nigeria_Maize/Maize/raw/compiled_Maize_fertiliser_trial_calculated_supply_afterlmer_sqrttf.RDS")
head(supply)


###################################################
## use the supply and estimate yield estimate to validate QUEFTS
###################################################
ds_validate <- unique(ds[, c("Treatment", "N",  "P","K", "blup", "TLID", "expCode", "refTreatment")])
ds_validate$index <- c(1:nrow(ds_validate))

supply_Qy <- NULL
for (i in unique(ds_validate$index)){
  print(i)
  tdata1 <- ds_validate[ds_validate$index == i, ]
  tdata2 <- supply[supply$TLID==tdata1$TLID, ]
  
  ## attainable yield in t/ha and dry wt.
  yya <- tdata2$Ya
  
  
  if(nrow(tdata2) > 0){
    yieldQUEFTS <- runQUEFTS(nut_rates = data.frame(N=tdata1$N, P=tdata1$P, K=tdata1$K),
                                    supply = c(tdata2$N_base_supply, tdata2$P_base_supply, tdata2$K_base_supply),
                                    crop = Crop,
                                    Ya = yya,
                                    SeasonLength = SeasonLength)
    
    # from dry matter to fresh weight
    tdata1$yieldQUEFTS <- yieldQUEFTS /0.845
    
    supply_Qy <- rbind(supply_Qy, tdata1)
  }
}

saveRDS(supply_Qy, "supply_Qy.RDS")

supply_Qy <- readRDS("supply_Qy.RDS")

###yieldQUEFTS in supply_Qy is still in fresh wt in in t/ha


ds_sdt <- ds %>%
  left_join(supply_Qy[, c("Treatment","TLID", "expCode", "yieldQUEFTS")]) 

ggC <- ggplot(ds_sdt, aes(blup, yieldQUEFTS)) +
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

######################################################################


INS <- supply %>%
  left_join(ds_sdt %>% dplyr::select(TLID, lat, lon, expCode, Season, AEZ) %>% unique()) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
         P_base_supply = ifelse(P_base_supply > 120, 120, P_base_supply),
         K_base_supply = ifelse(K_base_supply > 200, 200, K_base_supply)
  ) %>%
  mutate(season_AB = ifelse(grepl("SeasonA", Season), "SeasonA", "SeasonB")) %>%
  na.omit()

#Create plot to demonstrate ranges in supply by expCode and season combinations:
INS %>%
  gather(variable, value, N_base_supply:K_base_supply) %>%
  mutate(variable = factor(variable, levels = c("N_base_supply", "P_base_supply", "K_base_supply")),
         variable = revalue(variable, c("N_base_supply" = "N",
                                        "P_base_supply" = "P",
                                        "K_base_supply" = "K")),
         season = ifelse(grepl("SeasonA", Season), "SeasonA", "SeasonB")) %>%
  ggplot(aes(x = expCode, y = value, fill = Season)) + 
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
# removing soil variables with unlikely predictive value:
########################################################
#Soil information for prediction model:
sdt <- readRDS(paste(pathIn,"Soil/Soil_PointData_trial.RDS",sep="")) %>%
  dplyr::select(-ID) %>%
  dplyr::rename(TLID = TLID,
                lon = longitude,
                lat = latitude,
                Province = NAME_1,
                District = NAME_2) %>%
  dplyr::select(-c(fe_top, fe_bottom, B_0_30, Cu_0_30, Mn_0_30))



#Productivity class: ## by a median across all treatments??
ds_ref <- ds_sdt %>%
  #select treatments with high nutrient rates
  filter(N >= 70, P >= 15 , K >= 18) %>%
  group_by(TLID) %>%
  dplyr::summarise(refY = median(blup)) %>%
  mutate(refY = cut(refY, c(-Inf, 3000, 5000, 7000, Inf), labels = c("Low", "Medium", "High", "Very High")))
summary(ds_ref$refY)


#Topography and EAZ data:
tdt <- readRDS(paste(pathIn,"Topography/Topography_PointData_trial.RDS",sep="")) %>%
  dplyr::rename(lon = longitude,
                lat = latitude,
                alt = altitude,
                Province = NAME_1,
                District = NAME_2) %>%
  dplyr::select(-c(lat, lon, Province, District)) %>%
  mutate(AltClass2 = cut(alt, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)),
         #AEZ = ifelse(AEZ == "Central plateau", "Congo-Nile watershed divide", AEZ)
  )

#Add predictors to INS dataset:
INS <- INS %>% 
  left_join(sdt) %>%
  left_join(ds_ref) %>%
  left_join(tdt) %>%
  na.omit()
summary(INS$refY)



#create subset with predictors only: season is excluded (with season in the RF model has higher R square but logistically RAB doe not need advice per season)
## refY is not contributing a lot in RF model, the R square going from 68 to 58, which is not large. the treat structure does not seem to allow to capture a real difference in yield at the higher end
## TODO Check if yield response to N is sufficiently different among the treatments used as reference. 
ins <- INS %>%
  # dplyr::filter(expCode != "OFRA") %>% ### OFRA data is left in because using it increase R square from 49 ot 68 with some increase in rmse from 2.85 to 3.3
  dplyr::select(-c(TLID, Ya, lon, lat, expCode, Season, season_AB, Ya))


library(randomForest)
set.seed(65578)##
RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins, select = -c(P_base_supply, K_base_supply)), 
                     importance=TRUE, ntree=1000)

RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins, select = -c(N_base_supply, K_base_supply)),
                     importance=TRUE, ntree=1000)

RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins, select = -c(N_base_supply, P_base_supply)),
                     importance=TRUE, ntree=1000)


rmse(ins$N_base_supply, exp(predict(RF_N, ins)))
rmse(ins$P_base_supply, exp(predict(RF_P, ins)))
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


#Leave-one-out cross-validation:
preds <- NULL
run <- 0

for(i in unique(INS$TLID)){
  
  print(paste0(round(run/length(unique(INS$TLID))*100), "% complete"))
  
  ins_train <- subset(INS[INS$TLID != i,], select = -c(TLID, lon, lat, expCode, Season, Ya))
  ins_valid <- subset(INS[INS$TLID == i,], select = -c(TLID, lon, lat, expCode, Season, Ya))
  
  RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins_train, select = -c(P_base_supply, K_base_supply)), 
                       importance=TRUE, ntree=500, mtry = 15)
  
  RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins_train, select = -c(N_base_supply, K_base_supply)),
                       importance=TRUE, ntree=500, mtry = 15)
  
  RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins_train, select = -c(N_base_supply, P_base_supply)),
                       importance=TRUE, ntree=500, mtry = 15)
  
  preds <- rbind(preds, data.frame(TLID = i,
                                   N_pred = exp(predict(RF_N, ins_valid)),
                                   P_pred = exp(predict(RF_P, ins_valid)),
                                   K_pred = exp(predict(RF_K, ins_valid))))
  run <- run + 1
  
}


setwd("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform")
saveRDS(preds,"LOOCV_predictions_NPK_base_supply_afterlmer2.RDS")
preds <- readRDS("LOOCV_predictions_NPK_base_supply_afterlmer2.RDS")

INS <- INS %>% left_join(preds) ### INS is at trial level it has NPK from QUEFTS and from RF model by trialID

INS <- droplevels(INS[!INS$TLID == "29.34361111--1.572222222", ])

pINS <- INS %>%
  dplyr::select(TLID, N_base_supply, P_base_supply, K_base_supply, N_pred, P_pred, K_pred) %>%
  gather(variable, value, -TLID) %>%
  mutate(nutrient = substr(variable, 1, 1),
         nutrient = factor(nutrient, levels = c("N", "P", "K")),
         variable = ifelse(grepl("pred", variable), "RandomForest", "revQUEFTS")) %>%
  # unique()%>%
  tidyr::spread(variable, value) %>%
  left_join(ds %>% dplyr::select(TLID, expCode, Season) %>% unique()) %>%
  mutate(Season = ifelse(grepl("SeasonA", Season), "SeasonA", "SeasonB"))
# %>% 
#   mutate(RandomForest = ifelse(nutrient == "N" & RandomForest >= 120, NA, 
#                                ifelse(nutrient == "P"& RandomForest >= 30, NA,
#                                       ifelse(nutrient == "K" & RandomForest >= 150, NA, RandomForest)))) 
# na.omit()

#plot demonstrating reverse QUEFTS calculated N, P and K supply versus RF predictions using LOOCV:
gg8<-ggplot(pINS, aes(x = RandomForest, y = revQUEFTS)) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(fill = Season, shape=expCode), size = 3) + 
  scale_shape_manual(values = 21:23) +
  scale_fill_manual(values = c("grey90", "grey50"))+
  facet_wrap(~nutrient, nrow=1, scales="free") +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6,
                        label.y = .975) +
  geom_text(data = pINS %>% 
              dplyr::group_by(nutrient) %>% 
              dplyr::summarise(rmse = sqrt(sum((revQUEFTS - RandomForest)**2)/n()),
                               RandomForest = 0) %>%
              mutate(revQUEFTS = c(200, 200, 200)*0.95),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlim(0, NA)+
  ylim(0, 200)+
  xlab("Random Forest predicted soil nutrient supply [kg/ha]")+
  ylab("Reverse QUEFTS calculated supply [kg/ha]") +
  theme_gray() +
  theme(strip.text = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        legend.position = "none",
        axis.title = element_text(size = 15, face="bold"))
gg8

ggsave(paste(pathIn, "ReverseQuefts vs Random forest supply.pdf", sep=""), gg8, width=20, height=12)
##
#################################################
# 7. Predict yield from reverse QUEFTS NPK and from RF NPK and compare 
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
  
  ayi <- as.numeric(INS[INS$TLID == i,]$Ya)## given Ya is coming from the supply and it is dry matter scaled to 1.2 we can use it as is

  #yield predicted using reverse Quefts calculated supply
  yqi <- runQUEFTS(nut_rates = fri,
                   supply = as.numeric(sqi),
                   crop = "Maize",
                   Ya = ayi,
                   SeasonLength = 120)
  
  #yield predicted using supply obtained from predictions by RF
  ypi <- runQUEFTS(nut_rates = fri,
                   supply = as.numeric(spi),
                   crop = "Maize",
                   Ya = ayi,
                   SeasonLength = 120)
  
  #yield predicted by a naive model using median values of NPK supply across all TLIDs:
  yni <- runQUEFTS(nut_rates = fri,
                   supply = as.numeric(sni),
                   crop = "Maize",
                   Ya = ayi,
                   SeasonLength = 120)
  
  py <- rbind(py, data.frame(TLID = i,
                             N = fri$N,
                             P = fri$P,
                             K = fri$K,
                             Yq = yqi / 0.845, #yield predicted using revQUEFTS supply
                             Yp = ypi / 0.845, #yield predicted using RF predicted supply
                             Yn = yni / 0.845, #yield predicted using median nutrient supply
                             Yb = dsi$blup,          #yield blup
                             Yo = dsi$TY))           #yield observed
}

saveRDS(py, "py.RDS")

py <- readRDS("py.RDS")

py %>% left_join(ds_sdt %>% dplyr::select(TLID, expCode, Season, refTreatment) %>% unique()) %>%
  ggplot(aes(x = Yp, y = Yb)) + 
  geom_point(alpha = .33, shape = 16) + 
  geom_abline(slope= 1, intercept = 0) + 
  #stat_poly_line(se = F) +
  #stat_poly_eq(aes(label = after_stat(eq.label)), size=6) +
  #stat_poly_eq(label.y = 0.9, size = 6) +
  # facet_wrap(~refTreatment) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6) +
  
  #facet_wrap(~expCode+season, ncol=3) + 
  xlab("\nPredicted  yield (t/ha) using RF predicted supply")+
  ylab("BLUP Maize  yield (t/ha)\n")+
  # xlim(0, 62.5)+
  # ylim(0, 62.5)+
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


pyr <- py %>%
  gather(variable, value, Yq:Yo) %>%
  group_by(TLID, N, P, K, variable) %>%
  dplyr::summarise(value = mean(value)) %>%
  mutate(Treatment = ifelse(N>=70 & P>=15 & K>=18, "ref", "other")) %>%
  group_by(TLID, variable) %>%
  mutate(refY = mean(ifelse(Treatment == "ref", value, NA), na.rm=TRUE),
         dY = refY - value) %>%
  filter(Treatment != "ref") %>%
  dplyr::select(-Treatment, -value, -refY) %>%
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
                               Yb = 20),
            aes(label = paste0("rmse = ", round(rmse*100)/100)),
            size = 6, hjust = 0) +
  xlab("\nPredicted Maize  yield difference to referenceTreatment [t/ha]") +
  ylab("BLUP Maize  yield difference to referenceTreatment [t/ha]\n") +
  geom_abline(intercept = 0, slope = 1) +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq")),
                        formula = y ~ x, size = 6) +
  theme_gray()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"))


#######################################################
# Using RF model to predict INS, IPS and IKS at scale #
#######################################################
INS <- supply %>%
  #adding lats and lons and data source:
  left_join(ds_sdt %>% dplyr::select(TLID, lat, lon, expCode, Season, AEZ) %>% unique()) %>%
  mutate(lat = as.numeric(lat) ,
         lon = as.numeric(lon)) %>%
  #setting negative values to zero and maximal values to 750:
  mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x))) %>%
  mutate(season_AB = ifelse(grepl("A", Season), "A", "B")) %>%
  #remove incomplete rows:
  na.omit() %>%
  left_join(sdt) %>%
  left_join(ds_ref) %>%
  left_join(tdt) %>%
  mutate(AEZ = factor(AEZ),
         AltClass = factor(AltClass2),
         Province = factor(Province),
         dataset = "train") %>%
  droplevels() %>%
  na.omit()


#create subset with predictors only:
packages_required <- c("tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics", "lme4",
                       "MuMIn", "rgdal", "gridExtra", "ggspatial", "sf", "plyr")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Maize"

# setwd("~/Bester Space")
# saveRDS(INS, "test_INS.RDS")
# INS <- readRDS("test_INS.RDS")

saveRDS(INS, "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/INS2.RDS")
INS <- readRDS("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/INS2.RDS")


ins <- INS %>%
  dplyr::select(-c(TLID, expCode, Season, Ya,dataset, season_AB))

# #spatial data at 5x5 km:
# rwd <- readRDS("ML_test_Data.RDS") %>%
#   dplyr::select(-id.y) %>%
#   mutate(AES = factor(AEZ))

#spatial data at 1x1 km:

# rwd <- readRDS("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/result/geo_4cropModel/SoilDEM_PointData_AOI.RDS")
# rwd$longitude <- rwd$lon
# rwd$latitude <- rwd$lat
# rwd <- subset(rwd, select=-c(country, KS_top, KS_bottom, altitude, slope, TPI, TRI, lon, lat))

rwd <- readRDS(paste(pathIn,"Soil/Soil_PointData_AOI.RDS",sep=""))  %>%
  left_join(readRDS(paste(pathIn,"Topography/Topography_PointData_AOI.RDS",sep="")))

# saveRDS(rwd, "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/rwd.RDS")
# rwd <- raeadDS("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/rwd.RDS")


# rwaez <- AEZ
rwaez <- st_read("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/Landing/AEZ/AEZ_DEM_Dissolve.shp")
rwaez <- st_transform(rwaez, crs = st_crs(4326))
coords <- st_as_sf(rwd[, c("longitude", "latitude")], coords = c("longitude", "latitude"), crs = 4326)
coords <- st_join(coords, rwaez, join = st_intersects)
rwd$AEZ <- factor(coords$Names_AEZs)
#Remove NA in AEZ names
rwd<- rwd %>%
  mutate(AEZ = if_else(is.na(AEZ), "Mayaga", AEZ))

rwd <- rwd %>%
  dplyr::rename(lon = longitude,
                lat = latitude,
                alt = altitude,
                Province = NAME_1,
                District = NAME_2) %>%
  mutate(AltClass = cut(alt, c(-Inf, 2000, 2200, 2400, Inf), labels = paste0("Class_", 1:4)),
         AltClass2 = cut(alt, breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, Inf)),
         AltClass2 = revalue(AltClass2, c("(3e+03, Inf]" = "(2.5e+03,3e+03]")),
         AEZ = factor(AEZ),
         Province = factor(Province),
         District = factor(District),
         N_base_supply = NA,
         P_base_supply = NA,
         K_base_supply = NA,
         dataset = "AOI") %>%
  droplevels()

## the reference yield should be set to low, medium, high and very high for the whole data
## and one level at a time so that we will have 4 soil N maps, 4 soil P maps and 4 soil K maps


rwd <- rwd %>%
  dplyr::select(-setdiff(names(rwd), names(ins))) %>%
  bind_rows(ins)

ins$District <- as.factor(ins$District)
rwd$District <- as.factor(rwd$District)


#fit random forest using all data:
# rwd <- droplevels(rwd[rwd$AEZ %in% ins$AEZ, ])

refYlevel <- c("Low", "Medium", "High", "Very high")

## to loop through refYlevel


rwd_npk <- NULL
for(ryc in refYlevel){
  rwd2 <- rwd
  rwd2$refY <- as.factor(ryc)
  ins$dsort <- "train"
  rwd2$dsort <- "valid"
  
  aoi_rf <- rbind(ins, rwd2)
  rwd2 <- aoi_rf[aoi_rf$dsort == "valid", ]
  ins <- aoi_rf[aoi_rf$dsort == "train", ]
  
  library(randomForest)
  
  set.seed(777)
  
  RF_N <- randomForest(log(N_base_supply) ~ ., subset(ins, select = -c(P_base_supply, K_base_supply, dsort, lon, lat)), 
                       importance=TRUE, ntree=500, mtry = 15)
  
  RF_P <- randomForest(log(P_base_supply) ~ ., subset(ins, select = -c(N_base_supply, K_base_supply, dsort, lon, lat)),
                       importance=TRUE, ntree=1000)
  
  RF_K <- randomForest(log(K_base_supply) ~ ., subset(ins, select = -c(P_base_supply, N_base_supply, dsort, lon, lat)), 
                       importance=TRUE, ntree=1000)
  
  
  rwd2$predN <- exp(predict(RF_N, rwd2))
  rwd2$predP <- exp(predict(RF_P, rwd2))
  rwd2$predK <- exp(predict(RF_K, rwd2))
  rwd_npk <- rbind(rwd_npk, rwd2)
}

saveRDS(rwd_npk, "rwd_npk.RDS")

rwd_npk <- readRDS("rwd_npk.RDS")


hist(rwd_npk$predN)
length(rwd_npk$predN)
# saveRDS(rwd_npk, "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/rwd_npk2.RDS")
# rwd_npk <- readRDS("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/rwd_npk2.RDS")

ddply(rwd_npk, .(refY), summarize, avN = mean(predN), avP = mean(predP), avK = mean(predK)) ## N is not diff in diff ref yield class

rwshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
rwshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
rwshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
rwshp3 <- st_as_sf(geodata::gadm(country, level = 3, path='.'))
rwshp4 <- st_as_sf(geodata::gadm(country, level = 4, path='.'))
rwlake <- st_read(paste(pathIn2, "Lakes/RWA_Lakes_NISR.shp", sep=""))
RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
rwAEZ <- st_as_sf(RW_aez)
rwAEZ$Names_AEZs


### There is an issue at the norther tip of the soil dat and it is not giving prediction 
#1:19585
# 19797:39380
#39593:59177
#59389:78973


dev.off()
ggN <- ggplot()+
  geom_tile(data = rwd_npk[c(1:19585, 19797:39380,39593:59177, 59389:78973, 59389:78973), ], aes(x=lon, y=lat, fill = predN)) +
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  # geom_sf(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
  geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
  geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
  facet_wrap(~refY, nrow = 1) +
  scale_fill_gradient(low="palegoldenrod", high="darkgreen", 
                      limits = quantile(rwd_npk[rwd_npk$refY %in% c("Low","Medium", "High", "Very high"),]$predN, c(0.05, 0.95),na.rm = TRUE),
                      oob = scales::squish) +
  guides(fill=guide_legend(title="Soil N supply\n[kg N/ha]")) +
  # scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  # ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())
ggN



ggP <- ggplot()+
  geom_tile(data = rwd_npk[c(1:19585, 19797:39380,39593:59177, 59389:78973, 59389:78973), ], aes(x=lon, y=lat, fill = predP)) +
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
  geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
  geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
  facet_wrap(~refY, nrow = 1) +
  scale_fill_gradient(low="mistyrose2", high="darkred",
                      limits = quantile(rwd[rwd$dataset == "AOI" & rwd$refY %in% c("Low", "Medium", "High", "Very high"),]$predP, c(0.05, 0.95),na.rm = TRUE), 
                      oob = scales::squish) +
  guides(fill=guide_legend(title="Soil P supply\n[kg P/ha]")) +
  # scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  # ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())
ggP


ggK <- ggplot() +
  geom_tile(data = rwd_npk[c(1:19585, 19797:39380,39593:59177, 59389:78973, 59389:78973), ], aes(x=lon, y=lat, fill = predK)) +
  geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
  geom_sf(data = rwAEZ, fill = NA, linewidth = 1) +
  geom_sf(data = rwlake, size=NA, fill="lightblue")+
  geom_sf(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], linewidth = 0.6, color = "grey", fill=NA) +
  geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
  geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
  geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
  facet_wrap(~refY, nrow = 1) +
  scale_fill_gradient(low="lightblue1", high="darkblue", 
                      limits = quantile(rwd[rwd$dataset == "AOI" & rwd$refY %in% c("Low", "Medium", "High", "Very high"),]$predK, c(0.05, 0.95),na.rm = TRUE), 
                      oob = scales::squish) +
  guides(fill=guide_legend(title="Soil K supply\n[kg K/ha]")) +
  # scale_x_continuous(breaks = c(29, 29.5, 30), limits = c(29, 30.3)) +
  # ylim(-2.9, -1.2)+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=18, face="bold"),
        strip.background = element_blank())
ggK


ggsave("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/Rwanda_soilN.pdf", ggN, width = 14, height = 8)
ggsave("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/Rwanda_soilP.pdf", ggP, width = 14, height = 8)
ggsave("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform/Rwanda_soilK.pdf", ggK, width = 14, height = 8)


rwd_summaryAEZ <- rwd_npk[, c( "refY","AEZ","predN", "predP", "predK")]

mediansupply_AEZ <- rwd_summaryAEZ %>%
  gather(nutrient, supply, predN:predK) %>%
  group_by(nutrient, refY, AEZ)
median(mediansupply$supply)


npksuply <- mediansupply %>%
        ggplot(aes(x = AEZ, y = supply)) +
        geom_boxplot() +  
        coord_flip() +
        scale_y_log10() +
        facet_grid(refY ~ nutrient, scales = "free_x")

ggsave("NPK_AEZ_refY.pdf", npksuply, width=8, height = 8)
    
    # preparing data for K-means clustering
# aoi <- droplevels(rwd[rwd$dataset == "AOI",])
aoi <- rwd_npk
aoi$ID <- 1:nrow(aoi)

summary(lm(predN ~ AEZ + District + refY, data=aoi))$r.squared
summary(lm(predN ~ AEZ + District, data=aoi))$r.squared ## when refY is dropped
summary(lm(predN ~ District + refY, data=aoi))$r.squared ## when AEZ is dropped
summary(lm(predN ~ AEZ + refY, data=aoi))$r.squared ##when District is out
summary(lm(predN ~ refY, data=aoi))$r.squared ## withonly ref Y
summary(lm(predN ~ AEZ, data=aoi))$r.squared ## with only AEZ
summary(lm(predN ~ District, data=aoi))$r.squared ## with only AEZ
## this result clearly shows District is the best predictor much better than AEZ, we should develop the advisory by distruct
## Given teh ref yield is not adding much, we can just assume teh median refy class and develop teh advisory accordingly. 

summary(lm(predP ~ AEZ + District + refY, data=aoi))$r.squared
summary(lm(predP ~ District + refY, data=aoi))$r.squared
summary(lm(predP ~ AEZ + refY, data=aoi))$r.squared
summary(lm(predP ~ refY, data=aoi))$r.squared
summary(lm(predP ~ AEZ, data=aoi))$r.squared

summary(lm(predK ~ AEZ + District + refY, data=aoi))$r.squared
summary(lm(predK ~ District + refY, data=aoi))$r.squared
summary(lm(predK ~ AEZ + refY, data=aoi))$r.squared
summary(lm(predK ~ refY, data=aoi))$r.squared
summary(lm(predK ~ AEZ, data=aoi))$r.squared


#############################################################################
## get advisory at AEZ level after aggregating the soil NPK at AEZ level
#############################################################################
source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")
## the current blanket recommendation is ...
fert_rates_blanket <- c(100, 50, 0) ## when 100 kg Urea and 50 kg DAP is used


##3. predict the zero fertilizer, and the blanket recommendation yield 
## given the AEZ aggregated Ya and soil NPK still at a point level 
my_ferts <- data.frame(group = "synthetic", name = c("DAP", "Urea", "NPK"), 
                       N = c(18, 46, 17), P = c(20, 0, 7.4), K = c(0,0,14), 
                       Ca = 0, Mg = 0, S = 0, Mb = 0, Zn = 0, Bo = 0, Cu = 0,
                       price=c(722, 640,654))

rec_targetdY <- function(my_ferts,
                         dY,
                         target = c("relative", "absolute"), #is dY relative or absolute?
                         start = rep(50, nrow(my_ferts)), #starting values for the optimisation algorithm.
                         supply, #indigenous nutrient supply
                         crop = crop, #crop to be defined by QUEFTS
                         att_GY, #attainable yield must be n dry wt
                         GY_br, #blnaket recom yield must be in dry wt
                         SeasonLength = 120,
                         isBlanketRef = FALSE,
                         df_link){ #parameter important for the attainable yield estimation
  
  
  df_link$yieldPercinc <- paste(dY*100, " %", sep="")
  
  #calculate the control yield $for Rice. this is the yield estimated by QUEFTS for soil INS + blanket recommendation
  if(isBlanketRef == TRUE){
    GY0  <- GY_br
  }else{
    GY0 <- runQUEFTS(nut_rates = data.frame("N" = 0, "P" = 0, "K" = 0),
                     supply = supply,
                     crop = crop,
                     Ya = att_GY,
                     SeasonLength = SeasonLength)
  }
  
  
  #calculate the target yield
  if(dY == 0){
    GYt <- GY0
    df_link$targetYield <- df_link$yieldBlanket
  }else{
    GYt <- ifelse(target == "absolute", GY0 + dY, GY0 * (1 + dY))
    df_link$targetYield <- round(GY_br + (GY_br * dY), 2)
  }
  
  
  if(GYt > att_GY){
    
    # print("No solution available: yield target exceeds attainable yield.")
    fert_rates <- rep(NA, nrow(my_ferts))
    
  }else{
    
    #build the function to optimise
    qfo <- function(fert_rates,
                    my_ferts = my_ferts,
                    GYt = GYt,
                    supply = supply,
                    crop = crop,
                    Ya = att_GY,
                    SeasonLength=SeasonLength){
      
      GYq <- runQUEFTS(nut_rates = as.data.frame(as.list(nutrientRates(my_ferts, fert_rates))),
                       supply = supply,
                       crop = crop,
                       Ya = att_GY,
                       SeasonLength = SeasonLength)
      
      #set a value that minimises the yield difference with the control, and in addition...
      #minimises the total amount of fertiliser to apply (to ensure more stable results).
      return((GYq - GYt)**2 + sqrt(sum(fert_rates**2)))
      
    }
    
    #now performing the optimisation that minimises the difference to the target yield:
    result <- optim(start,
                    qfo,
                    my_ferts = my_ferts,
                    GYt = GYt,
                    supply = supply,
                    crop = crop,
                    Ya = att_GY,
                    SeasonLength = SeasonLength)
    
    #return the vector with the fertiliser rates to achieve the target:
    fert_rates <- result$par
  }
  
  df_link$DAP <- round(fert_rates[1],0)
  df_link$Urea <- round(fert_rates[2], 0)
  df_link$NPK_17_3 <- round(fert_rates[3],0)
  
  return(df_link)
  
}




#############################################################################
## get advisory at point level with the attainable yield aggregated either at AEZ level or at District level
## sol NPK is still at point level, the fertilizer recommendation can be aggregated at the desired level after the point estimation. 
## given there is hardly any predictive value we see for the reference yield, we will use the median class
#############################################################################

rwd_npk_medium <- rwd_npk[rwd_npk$refY == "Medium", c("lat", "lon", "AEZ", "District", "Province", "predN", "predP", "predK")]

## yield by AEZ : we do not have field data in Imbo, Impala and Lake Kivu AEZ to assign Ya and for 7 Districts in these AEZ
## we use for now median across all data, 
Ya_AEZ <- ddply(INS, .(AEZ), summarize, Ya = median(Ya))
Ya_AEZ <- rbind(Ya_AEZ, data.frame(AEZ = c("Imbo","Impala", "Lake kivu"),  Ya = c(rep(5736,3))))

## yield by District : we do not have field data in Gasabo, Kayonza, Kicukiro,Nyamasheke, Nyarugenge,Ruhango , Rusizi   to assign Ya and for 7 Districts in these AEZ
## we use for now median across all data, 
Ya_District <- ddply(INS, .(District), summarize, Ya = median(Ya))
Ya_District <- rbind(Ya_District, data.frame(District = c("Gasabo", "Kayonza", "Kicukiro","Nyamasheke", "Nyarugenge","Ruhango" , "Rusizi"),
                                                Ya = c(rep(5736,7))))


Yield_zero_Blanket_AEZ <- NULL
Yield_zero_Blanket_District <- NULL
for(i in 1:nrow(rwd_npk_medium)){
  print(i)
  ## NPK at a point
  NPK_i <- rwd_npk_medium[i,]
  NPKsupply <- data.frame("N" = NPK_i$predN,
                          "P" = NPK_i$predP,
                          "K" = NPK_i$predK)
  
  zeroBlanket_AEZ <- NPK_i
  zeroBlanket_District <- NPK_i
  
  ## AEZ level
  Ya_AEZ_i <- Ya_AEZ[Ya_AEZ$AEZ %in% NPK_i$AEZ, ]
  if(nrow(Ya_AEZ_i)>0){
    zeroBlanket_AEZ$Ya_AEZ <- Ya_AEZ_i$Ya
    
    zeroBlanket_AEZ$yieldZeroAEZ <- runQUEFTS(nut_rates = data.frame(N=0, P=0, K=0),
                                     supply = c(NPKsupply$N, NPKsupply$P, NPKsupply$K),
                                     crop = "Maize",
                                     Ya = zeroBlanket_AEZ$Ya_AEZ ,
                                     SeasonLength = 120)
    
    zeroBlanket_AEZ$yieldBlanketAEZ <- runQUEFTS(nut_rates = data.frame(N=55, P=10, K=0), ##55:10:0
                                        supply = c(NPKsupply$N, NPKsupply$P, NPKsupply$K),
                                        crop = Crop,
                                        Ya = zeroBlanket_AEZ$Ya_AEZ ,
                                        SeasonLength = 120)
    
    Yield_zero_Blanket_AEZ <- rbind(Yield_zero_Blanket_AEZ, zeroBlanket_AEZ)
  }

  ## District level
  Ya_District_i <- Ya_District[Ya_District$District == NPK_i$District,]
  if(nrow(Ya_District_i) > 0){
    zeroBlanket_District$Ya_D <- Ya_District_i$Ya
   
    
    zeroBlanket_District$yieldZeroDis <- runQUEFTS(nut_rates = data.frame(N=0, P=0, K=0),
                                     supply = c(NPKsupply$N, NPKsupply$P, NPKsupply$K),
                                     crop = "Maize",
                                     Ya =  zeroBlanket_District$Ya_D,
                                     SeasonLength = 120)
    
    zeroBlanket_District$yieldBlanketDis <- runQUEFTS(nut_rates = data.frame(N=55, P=10, K=0), ##55:10:0
                                        supply = c(NPKsupply$N, NPKsupply$P, NPKsupply$K),
                                        crop = Crop,
                                        Ya =  zeroBlanket_District$Ya_D,
                                        SeasonLength = 120)
    
    Yield_zero_Blanket_District <- rbind(Yield_zero_Blanket_District, zeroBlanket_District)
  }
  
}

 saveRDS(Yield_zero_Blanket_District, "Yield_zero_Blanket_District.RDS")
 saveRDS(Yield_zero_Blanket_AEZ, "Yield_zero_Blanket_AEZ.RDS")

 Yield_zero_Blanket_District <- readRDS("Yield_zero_Blanket_District.RDS")
 Yield_zero_Blanket_AEZ <- readRDS("Yield_zero_Blanket_AEZ.RDS")

 
 ggyieldBlanket_District <- ggplot() +
                    geom_tile(data = Yield_zero_Blanket_District[1:19585, ], aes(x=lon, y=lat, fill = yieldBlanketDis/0.845)) +
                    geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
                    geom_sf(data = rwAEZ, fill = NA, linewidth = 1, color = "grey") +
                    geom_sf(data = rwlake, size=NA, fill="lightblue")+
                    geom_sf(data = rwshp2, linewidth = 0.6, color = "black", fill=NA) +
                    geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
                    geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
                    geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
                     scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(2500, 5000, 500),
                                          #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                                          limits = c(2000, 5500),
                                          oob = scales::squish) +
                     guides(fill=guide_legend(title="Yield with\ncurrent blanket\nrecommendation\n[t/ha]")) +
                     ggtitle("Attainable yield aggregated at District level") +
                     theme_bw()+
                     xlab("Longitude")+
                     ylab("Latitude")+
                     theme(axis.title = element_blank(),
                           axis.text = element_text(size=14),
                           legend.title = element_text(size=18, face="bold"),
                           legend.text = element_text(size=18),
                           strip.text = element_text(size=18, face="bold"),
                           strip.background = element_blank(), plot.title = element_text(hjust = 0.5))
      

 ggyieldzero_District <- ggplot() +
   geom_tile(data = Yield_zero_Blanket_District[1:19585, ], aes(x=lon, y=lat, fill = yieldZeroDis/0.845)) +
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1, color = "grey") +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
   geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
   scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(1000, 3500, 500),
                        #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                        limits = c(500, 4000),
                        oob = scales::squish) +
   guides(fill=guide_legend(title="Yield without\nfertilizer [t/ha]")) +
   ggtitle("Attainable yield aggregated at District level") +
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank(), plot.title = element_text(hjust = 0.5))
 


 ggsave("yieldBlanket_District.pdf", ggyieldBlanket_District, width = 8, height = 8)
 ggsave("yieldzero_District.pdf", ggyieldzero_District, width = 8, height = 8)
 
 
 
 
 ggyieldBlanket_AEZ <- ggplot() +
   geom_tile(data = Yield_zero_Blanket_AEZ[1:19585, ], aes(x=lon, y=lat, fill = yieldBlanketAEZ/0.845)) +
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1, color = "grey") +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
   geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
   scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(2000, 5000, 500),
                        #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                        limits = c(2000, 5500),
                        oob = scales::squish) +
   guides(fill=guide_legend(title="Yield with\ncurrent blanket\nrecommendation\n[t/ha]")) +
   ggtitle("Attainable yield aggregated at AEZ level") +
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank(), plot.title = element_text(hjust = 0.5))
 
 ggyieldBlanket_AEZ
 
 ggyieldzero_AEZ <- ggplot() +
   geom_tile(data = Yield_zero_Blanket_AEZ[1:19585, ], aes(x=lon, y=lat, fill = yieldZeroAEZ/0.845)) +
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1, color = "grey") +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
   geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2))+
   scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(1000, 3500, 500),
                        #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                        limits = c(1000, 4000),
                        oob = scales::squish) +
   guides(fill=guide_legend(title="Yield without\nfertilizer [t/ha]")) +
   ggtitle("Attainable yield aggregated at AEZ level") +
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=18, face="bold"),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank(), plot.title = element_text(hjust = 0.5))
 
 ggyieldzero_AEZ
 
 ggsave("yieldBlanket_AEZ.pdf", ggyieldBlanket_AEZ, width = 8, height = 8)
 ggsave("yieldzero_AEZ.pdf", ggyieldzero_AEZ, width = 8, height = 8)
 
 
 
 
 
 
 ### fertilizer recommendation by AEZ, considering DAP, Urea and NPK 17:17:17 and 
 ## setting the target yield at yield obtained with blanket recommendation or increasing that yield by 10,20 or 30 percent
 
 fertrecom_AEZ <- NULL
 for(j in 263:nrow(Yield_zero_Blanket_AEZ)){
   print(j)
   fertperc <- NULL
   for(perc in c(0, 0.1, 0.2, 0.3)){
     print(perc)
     fertrecomAEZ <-   rec_targetdY(my_ferts=my_ferts, 
                                  dY = perc, 
                                  target = "relative", 
                                  start = rep(0, nrow(my_ferts)), 
                                  supply = c(Yield_zero_Blanket_AEZ$predN[j], Yield_zero_Blanket_AEZ$predP[j], 1000), ## put K so high that the optimization will ignore K need
                                  att_GY = Yield_zero_Blanket_AEZ$Ya_AEZ[j] , 
                                  GY_br = Yield_zero_Blanket_AEZ$yieldBlanketAEZ[j],
                                  crop = "Maize",
                                  SeasonLength = 120,
                                  isBlanketRef = TRUE, 
                                  df_link  = Yield_zero_Blanket_AEZ[j, ])
     fertperc <- rbind(fertperc, fertrecomAEZ)
   }
   
   fertrecom_AEZ <- rbind(fertrecom_AEZ, fertperc)
   saveRDS(fertrecom_AEZ, "fertrecom_AEZ.RDS")
 }
 
 
 fertrecom_AEZ$YaFW <- fertrecom_AEZ$Ya / 0.845 ## convert it back to fresh yield
 fertrecom_AEZ$yieldZeroFW <- fertrecom_AEZ$yieldZero / 0.845
 fertrecom_AEZ$yieldBlanketFW <- fertrecom_AEZ$yieldBlanket / 0.845
 fertrecom_AEZ$targetYieldFW <- fertrecom_AEZ$targetYield / 0.845

 
 fertrecom_AEZ[fertrecom_AEZ$AEZ== "Birunga",]
 
 saveRDS(fertrecom_AEZ, "AEZ_fertilizer_Recommendation.RDS")
 write.csv(fertrecom_AEZ, "AEZ_fertilizer_Recommendation.csv", row.names = FALSE)
 
 
 
 ### plotting can be done using the following: e.g. AEZ level fertilizer recommendation 20% yield increase
 
 ggfertrecom_AEZ_DAP <- ggplot() +
   geom_tile(data = fertrecom_AEZ[fertrecom_AEZ$yieldPercinc == "20 %", ][1:19585, ], aes(x=lon, y=lat, fill = DAP)) +
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1,color = "grey") +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
   geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2)) +
   scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(10, 100, 10),
                        #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                        limits = c(15, 60),
                        oob = scales::squish) +
   guides(fill=guide_legend(title="DAP [kg/ha] \n for 20% yield increase")) +
   ggtitle("Targeted yield aggregated at AEZ level") +
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=14),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank(), plot.title = element_text(hjust = 0.5))
 
 ggfertrecom_AEZ_DAP
 
 ggsave("yield20Perc_YaAEZ_DAP.pdf", ggfertrecom_AEZ_DAP, width = 8, height = 8)
 
 
 ggfertrecom_AEZ_Urea <- ggplot() +
   geom_tile(data = fertrecom_AEZ[fertrecom_AEZ$yieldPercinc == "20 %", ][1:19585, ], aes(x=lon, y=lat, fill = Urea)) +
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1, color = "grey") +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
   geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2)) +
   scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(0, 225, 25),
                        #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                        limits = c(0, 225),
                        oob = scales::squish) +
   guides(fill=guide_legend(title="Urea [kg/ha] \n for 20% yield increase")) +
   ggtitle("Targeted yield aggregated at AEZ level") +
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=14),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank(), plot.title = element_text(hjust = 0.5))
 
 ggfertrecom_AEZ_Urea
 
 ggsave("yield20Perc_YaAEZ_Urea.pdf", ggfertrecom_AEZ_Urea, width = 8, height = 8)
 
 
 
 
 ggfertrecom_AEZ_NPK <- ggplot() +
   geom_tile(data = fertrecom_AEZ[fertrecom_AEZ$yieldPercinc == "20 %", ][1:19585, ], aes(x=lon, y=lat, fill = NPK_17_3)) +
   geom_sf(data = rwshp0, linewidth = 1.2, color = "black", fill=NA) +
   geom_sf(data = rwAEZ, fill = NA, linewidth = 1, color = "grey") +
   geom_sf(data = rwlake, size=NA, fill="lightblue")+
   geom_sf(data = rwshp2, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp1, linewidth = 0.6, color = "black", fill=NA) +
   geom_sf(data = rwshp0, linewidth = 1, color = "black", fill=NA) +
   geom_sf_text(data = rwshp2[rwshp2$NAME_1 %in% c("Northern Province", "Western Province", "Southern Province"),], aes(label = NAME_2)) +
   scale_fill_viridis_c(direction = -1, option="turbo", breaks = seq(0, 225, 25),
                        #limits = quantile(rec[rec$refY %in% c("Low", "Medium", "High", "Very high") & rec$target==0,]$TY0, c(0.05, 0.95)), 
                        limits = c(0, 125),
                        oob = scales::squish) +
   guides(fill=guide_legend(title="NPK 17:17:17 [kg/ha] \n for 20% yield increase")) +
   ggtitle("Targeted yield aggregated at AEZ level") +
   theme_bw()+
   xlab("Longitude")+
   ylab("Latitude")+
   theme(axis.title = element_blank(),
         axis.text = element_text(size=14),
         legend.title = element_text(size=14),
         legend.text = element_text(size=18),
         strip.text = element_text(size=18, face="bold"),
         strip.background = element_blank(), plot.title = element_text(hjust = 0.5))
 
 ggfertrecom_AEZ_NPK
 
 ggsave("yield20Perc_YaAEZ_NPK.pdf", ggfertrecom_AEZ_NPK, width = 8, height = 8)
 
 
  ### fertilizer recommendation by District
 fertrecom_District_predK <- NULL
 for(j in 1:nrow(Yield_zero_Blanket_District)){
   print(j)
   fertpercD <- NULL
   for(perc in c(0, 0.1, 0.2, 0.3)){
     print(perc)
     fertrecomDistrict <-   rec_targetdY(my_ferts=my_ferts, 
                                    dY = perc, 
                                    target = "relative", 
                                    start = rep(0, nrow(my_ferts)), 
                                    supply = c(Yield_zero_Blanket_District$predN[j], Yield_zero_Blanket_District$predP[j], 1000), ## put K so high that the optimization will ignore K need
                                    att_GY = Yield_zero_Blanket_District$Ya_D[j] , 
                                    GY_br = Yield_zero_Blanket_District$yieldBlanketDis[j],
                                    crop = "Maize",
                                    SeasonLength = 120,
                                    isBlanketRef = TRUE, 
                                    df_link  = Yield_zero_Blanket_District[j, ])
     fertpercD <- rbind(fertpercD, fertrecomDistrict)
   }
   
   fertrecom_District_predK <- rbind(fertrecom_District_predK, fertpercD)
   saveRDS(fertrecom_District_predK, "fertrecom_District_predK.RDS")
 }
 

 fertrecom_District_predK$YaFW <- fertrecom_District_predK$Ya / 0.845 ## convert it back to fresh yield
 fertrecom_District_predK$yieldZeroFW <- fertrecom_District_predK$yieldZero / 0.845
 fertrecom_District_predK$yieldBlanketFW <- fertrecom_District_predK$yieldBlanket / 0.845
 fertrecom_District_predK$targetYieldFW <- fertrecom_District_predK$targetYield / 0.845
 
 fertrecom_District_predK[fertrecom_District_predK$District== "Nyaruguru",]
 
 saveRDS(fertrecom_District, "District_fertilizer_Recommendation.RDS")
 write.csv(fertrecom_District, "District_fertilizer_Recommendation.csv", row.names = FALSE)
 
 saveRDS(fertrecom_District_predK , "District_fertilizer_Recommendation_soilK.RDS")
 write.csv(fertrecom_District_predK , "District_fertilizer_Recommendation_soilK.csv", row.names = FALSE)
 

 
 setwd("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Maize/transform")
 fertrecom_District <- readRDS("District_fertilizer_Recommendation.RDS")
 fertrecom_District_predK <- readRDS("District_fertilizer_Recommendation_soilK.RDS")

#############################################################################
## aggregate the advisory at District or AEZ level
#############################################################################
 
 fertilizrAdvice_AEZ_aggregated <- ddply(fertrecom_AEZ, .(AEZ,  yieldPercinc), summarise, 
                             predN = median(predN),
                             predP = median(predP), 
                             predK = median(predK),
                             Ya_att = median(YaFW),
                             yieldZero = median(yieldZeroFW),
                             yieldBlnaket = median(yieldBlanketFW),
                             targetYield = median(targetYieldFW), 
                             Urea_kg = median(Urea),
                             DAP_kg = median(DAP),
                             NPK_171717_kg = median(NPK_17_3))

 fertilizrAdvice_AEZ_aggregated$Urea_25kgbag <- round(fertilizrAdvice_AEZ_aggregated$Urea_kg/25)* 25
 fertilizrAdvice_AEZ_aggregated$DAP_25kgbag <- round(fertilizrAdvice_AEZ_aggregated$DAP_kg/25)* 25
 fertilizrAdvice_AEZ_aggregated$NPK171717_25kgbag <- round(fertilizrAdvice_AEZ_aggregated$NPK_171717_kg/25)* 25
 
 FertilizerAdive_AEZ_0Perc <- fertilizrAdvice_AEZ_aggregated[fertilizrAdvice_AEZ_aggregated$yieldPercinc == "0 %", ]
 FertilizerAdive_AEZ_10Perc <- fertilizrAdvice_AEZ_aggregated[fertilizrAdvice_AEZ_aggregated$yieldPercinc == "10 %", ]
 FertilizerAdive_AEZ_20Perc <- fertilizrAdvice_AEZ_aggregated[fertilizrAdvice_AEZ_aggregated$yieldPercinc == "20 %", ]
 FertilizerAdive_AEZ_30Perc <- fertilizrAdvice_AEZ_aggregated[fertilizrAdvice_AEZ_aggregated$yieldPercinc == "30 %", ]
 
 saveRDS(fertilizrAdvice_AEZ_aggregated, "fertilizrAdvice_AEZ_aggregated.RDS")
 
 
 
 fertilizrAdvice_District_aggregated <- ddply(fertrecom_District, .(District,  yieldPercinc), summarise, 
                                         predN = median(predN),
                                         predP = median(predP), 
                                         predK = median(predK),
                                         Ya_att = median(YaFW),
                                         yieldZero = median(yieldZeroFW),
                                         yieldBlnaket = median(yieldBlanketFW),
                                         targetYield = median(targetYieldFW), 
                                         Urea_kg = median(Urea),
                                         DAP_kg = median(DAP),
                                         NPK_171717_kg = median(NPK_17_3))
 
 
 
 fertilizrAdvice_District_aggregated$Urea_25kgbag <- round(fertilizrAdvice_District_aggregated$Urea_kg/25)* 25
 fertilizrAdvice_District_aggregated$DAP_25kgbag <- round(fertilizrAdvice_District_aggregated$DAP_kg/25)* 25
 fertilizrAdvice_District_aggregated$NPK171717_25kgbag <- round(fertilizrAdvice_District_aggregated$NPK_171717_kg/25)* 25
 
 FertilizerAdive_Dis_0Perc <- fertilizrAdvice_District_aggregated[fertilizrAdvice_District_aggregated$yieldPercinc == "0 %", ]
 FertilizerAdive_Dis_10Perc <- fertilizrAdvice_District_aggregated[fertilizrAdvice_District_aggregated$yieldPercinc == "10 %", ]
 FertilizerAdive_Dis_20Perc <- fertilizrAdvice_District_aggregated[fertilizrAdvice_District_aggregated$yieldPercinc == "20 %", ]
 FertilizerAdive_Dis_30Perc <- fertilizrAdvice_District_aggregated[fertilizrAdvice_District_aggregated$yieldPercinc == "30 %", ]
 
 saveRDS(fertilizrAdvice_District_aggregated, "fertilizrAdvice_District_aggregated.RDS")
 
 fertilizrAdvice_District_aggregated <- readRDS("fertilizrAdvice_District_aggregated.RDS")
 
 
 ### with the estimated soil INS for K being used for the fertilizer rate predcition 
 fertilizrAdvice_District_K_aggregated <- ddply(fertrecom_District_predK, .(District,  yieldPercinc), summarise, 
                                              predN = median(predN),
                                              predP = median(predP), 
                                              predK = median(predK),
                                              Ya_att = median(YaFW),
                                              yieldZero = median(yieldZeroFW),
                                              yieldBlnaket = median(yieldBlanketFW),
                                              targetYield = median(targetYieldFW), 
                                              Urea_kg = median(Urea),
                                              DAP_kg = median(DAP),
                                              NPK_171717_kg = median(NPK_17_3))
 
 
 
 fertilizrAdvice_District_K_aggregated$Urea_25kgbag <- round(fertilizrAdvice_District_K_aggregated$Urea_kg/25)* 25
 fertilizrAdvice_District_K_aggregated$DAP_25kgbag <- round(fertilizrAdvice_District_K_aggregated$DAP_kg/25)* 25
 fertilizrAdvice_District_K_aggregated$NPK171717_25kgbag <- round(fertilizrAdvice_District_K_aggregated$NPK_171717_kg/25)* 25
 
 FertilizerAdive_soilK_Dis_0Perc <- fertilizrAdvice_District_K_aggregated[fertilizrAdvice_District_K_aggregated$yieldPercinc == "0 %", ]
 FertilizerAdive_soilK_Dis_10Perc <- fertilizrAdvice_District_K_aggregated[fertilizrAdvice_District_K_aggregated$yieldPercinc == "10 %", ]
 FertilizerAdive_soilK_Dis_20Perc <- fertilizrAdvice_District_K_aggregated[fertilizrAdvice_District_K_aggregated$yieldPercinc == "20 %", ]
 FertilizerAdive_soilK_Dis_30Perc <- fertilizrAdvice_District_K_aggregated[fertilizrAdvice_District_K_aggregated$yieldPercinc == "30 %", ]
 
 saveRDS(fertilizrAdvice_District_K_aggregated, "fertilizrAdvice_District_soilK_aggregated.RDS")
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 