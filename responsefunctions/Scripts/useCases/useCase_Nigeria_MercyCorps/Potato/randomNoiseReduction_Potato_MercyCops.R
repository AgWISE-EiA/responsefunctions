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

country <- "Nigeria"
useCaseName <- "MercyCorps"
Crop <- "Potato"


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
# 1. Curate Potato fieldData #
###############################
#get the data and change P2O5 to P and K2O to K
ds <- readRDS(paste(pathIn, "aggregated_fieldData.rds", sep=""))# read the data assembled from Carob
ds <- ds %>% 
  dplyr::mutate(P = round(P_fertilizer * 0.44, digits=0),
                K = round(K_fertilizer * 0.83, digits=0))
head(ds)

## the P and K rate is given as P2O5 and K2O, change that to elemental P and K (1 P2O5 has 0.44 P ; 1K2O has 0.83 K)

## saving data or crop type mapping using remote sensing application
ds_RS <- ds %>%
  dplyr::mutate(country = "Nigeria") %>% 
  dplyr::rename(lon = longitude ,
                lat = latitude  ) %>% 
  dplyr::select(c(country, lon, lat, planting_date, harvest_date)) %>% 
  unique()
ds_RS$crop <- "Potato"

saveRDS(ds_RS , "~/agwise-datacuration/dataops/datacuration/Data/useCase_Nigeria_MercyCorps/Potato/raw/data4RS.RDS")


# rename and standardize the column names to match the data from OFRA
ds <- ds %>%
  dplyr::rename(district = site.District,
                cluster = Cluster,
                lon = longitude,
                lat = latitude,
                var = variety,
                N = N_fertilizer,
                TY = yield,
                plantingDate = planting_date,
                harvestDate = harvest_date, 
                Treatment= nut_response_eval) %>%
  dplyr::mutate(plantingDate = as.Date(plantingDate, format="%d/%m/%Y"),
                harvestDate = as.Date(harvestDate, format="%d/%m/%Y"))%>%
  dplyr::mutate(LGP = harvestDate-plantingDate)%>%
  dplyr::select(district,cluster,EA,LGA,lat,lon,plantingDate,harvestDate,Season,LGP,N,P,K,TY,Treatment,var)%>%
  # dplyr::mutate(lat = if_else(lat > 1, lat * 1, lat))%>%
  dplyr::mutate(Treatment = as.factor(Treatment),
                Season = as.factor(Season),
                var = as.factor(var))%>%
  dplyr::mutate(Treatment = fct_recode(Treatment,
                                       "Control" = "Control",
                                       "PK" = "PK",
                                       "NK" = "NK",
                                       "NP" = "NP",
                                       "NPK" = "NPK",
                                       "Blanket" = "NPK blanket"))%>%
  dplyr::select(district,cluster,EA,LGA,lat,lon,Season,N,P,K,TY,Treatment,var)%>%
dplyr::mutate(TLID = paste(lon, lat, sep="_"),
              plotID = paste(lon, lat,Treatment,sep="_"))

# the other columns not selected can be used as reference for other research

head(ds)
ds$index <- 1:nrow(ds)
summary(ds[ds$Treatment == "Blanket", ])
ds[ds$TLID == "8.7029267_9.5959467", ]
unique(ds[, c("N","P","K","Treatment")])
ds <- ds[ds$TY < 40000, ]

ggplot(ds, aes(Treatment, TY, fill=cluster))+
  geom_boxplot()+
  facet_wrap(~LGA)+
  ylab("Yield (kg/ha)")

## identify extreme values and remove those in case it improves the model result

Bassa <- ds[ds$LGA == "Bassa", ]
Bassa[Bassa$TY > 10000, ]
ds <- ds[!ds$TLID == "8.7322652_9.8825828", ]
JS <- ds[ds$LGA == "Jos South", ]
JS[JS$TY > 25000, ]
JS[JS$TLID == "8.9074251_9.7762642", ]
ds[ds$Treatment== "PK" & ds$TY > 25000, ]
ds <- ds[!ds$index == 62, ]

ds[ds$Treatment== "NK" & ds$TY > 25000, ]
ds <- ds[!ds$index == 93, ]

ds[ds$Treatment== "NP" & ds$TY > 25000, ]
ds <- ds[!ds$index == 40, ]



# Summarize to view the number of trials per treatment in each experiment
df2 <- ds %>% 
  dplyr::group_by(TLID,Treatment,plotID) %>% 
  dplyr::summarise(total_count=n(),.groups = 'drop') %>% 
  dplyr::arrange(desc(total_count)) %>%
  as.data.frame()

dplyr::count(df2, TLID)
dplyr::count(df2, Treatment) ## 48 trials

# Summarize and explore data to view the number of trials per treatment in each experiment

df3 <- ds %>% 
  dplyr::group_by(Treatment,var) %>% 
  dplyr::summarise(total_count = n(), mean_TY = round(median(TY)), .groups = 'drop') %>% 
  dplyr::arrange(desc(mean_TY)) %>%
  as.data.frame()
df3

# did we use the same variety within a trial
varrtlid <- unique(ds[, c("TLID", "var")])
vt <- as.data.frame(table(varrtlid$var))


ds[ds$var == "Cip 398308.29", ] ## this one treatment has higher yield than the average in the LGA for all treatments
ds %>% dplyr::filter(LGA == "Riyom") %>%
  group_by(Treatment) %>% summarise(m = mean(TY))
  

ggplot(ds, aes(Treatment, TY, fill=var))+
  geom_boxplot()+
  facet_wrap(~cluster)+
  ylab("Yield (kg/ha)")


hist(ds$TY,main="Yield")


## see where there is negative yield effect between NPK-Blanket an NPK-Control
ds_wide <- ds %>% 
  dplyr::select(c("TY","Treatment","TLID")) %>% 
  tidyr::spread(Treatment, TY) %>% 
  dplyr::mutate(NPK_BR = NPK - Blanket,
                NPK_control = NPK - Control) 



ds$Ydiff <- ifelse(ds$TLID %in% unique(ds_wide[!is.na(ds_wide$NPK_BR) & ds_wide$NPK_BR < 0, "TLID"]), "NegEff", "PosEff")

ds$YdiffC <- ifelse(ds$TLID %in% unique(ds_wide[!is.na(ds_wide$NPK_control) & ds_wide$NPK_control < 0, "TLID"]), "NegEff", "PosEff")


#plot showing yield ranges by variety and different data sources:
gg2 <- ds %>%
  unique() %>% 
  ggplot(aes(x = Treatment, y = TY, fill=as.factor(Treatment))) +
  geom_boxplot() +
  theme_bw()+
  ylab("\nPotato yield [kg/ha]")+
  facet_wrap(~Ydiff) +
  ggtitle("Potato yield per treatment faceted by NPK-Blanket diff") +
  theme(axis.title.y = element_text(size = 15, face="bold"),
        #axis.title.x = element_blank("Potato yield per treatment facetd by NPK-Blanket diff"),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(size = 14, face="bold", hjust=0))
gg2

ggsave(paste(pathOut1,"Potato yield per treatment faceted by NPK-Blanket diff.pdf"), gg2, width=10, height = 6)

quantile(ds[ds$Treatment == "Blanket", "TY"], probs=seq(0, 1, 0.2))


#plot showing yield ranges by variety and different data sources:
gg2B <- ds %>%
  unique() %>% 
  ggplot(aes(x = Treatment, y = TY, fill=as.factor(Treatment))) +
  geom_boxplot() +
  theme_bw()+
  ylab("\nPotato yield [kg/ha]")+
  facet_wrap(~YdiffC) +
  ggtitle("Potato yield per treatment faceted by NPK-Control diff") +
  theme(axis.title.y = element_text(size = 15, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(size = 14, face="bold", hjust=0))
gg2B

ggsave(paste(pathOut1,"Potato yield per treatment faceted by NPK-Control diff.pdf"), gg2B, width=10, height = 6)

gg3<- ds %>%
  unique() %>% 
  ggplot(aes(x = Treatment, y = TY, fill=as.factor(cluster))) +
  geom_boxplot() +
  theme_bw()+
  ylab("\nPotato yield [kg/ha]")+
  theme(axis.title.y = element_text(size = 15, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 14, face="bold", hjust=0))
gg3

ggsave(paste(pathOut1,"Potato yield treatment per cluster.pdf"), gg3, width=8, height = 6)

#density plot showing yield ranges by experiment and season:

gg_density <- ds %>%
  ggplot(aes(x = TY,
             colour=paste0(Treatment, " (", Treatment, ")"),
             fill=paste0(Treatment, " (", Treatment, ")"))) +
  geom_density(alpha=.2, linewidth=1) +
   #facet_wrap(~cluster) + 
  ggtitle("Potato, yield density distribution") + 
  xlab("\nPotato yield [kg/ha]")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank())
gg_density
ggsave(paste(pathOut1,"Potato yield density.pdf"), gg_density, width=8, height = 6)

#density plot showing yield ranges by cluster:
gg_density_clust <- ds %>%
  ggplot(aes(x = TY,
             colour=paste0(Treatment, " (", Treatment, ")"),
             fill=paste0(Treatment, " (", Treatment, ")"))) +
  geom_density(alpha=.2, linewidth=1) +
  facet_wrap(~cluster) + 
  ggtitle("Potato, yield density distribution") + 
  xlab("\nPotato yield [kg/ha]")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank())
gg_density_clust
ggsave(paste(pathOut1,"Potato yield density per cluster.pdf"), gg_density_clust, width=8, height = 6)

#map with trial locations:
NGAshp0 <- st_as_sf(geodata::gadm(country, level = 0, path='.'))
NGAshp1 <- st_as_sf(geodata::gadm(country, level = 1, path='.'))
NGAshp2 <- st_as_sf(geodata::gadm(country, level = 2, path='.'))
AEZ <- st_read(paste(pathIn2, "Nigeria_AEZ.shp", sep=""))
target_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
NGA_aez <- st_transform(AEZ, target_crs)
NGA_dist <- NGAshp1 [NGAshp1$NAME_1 == "Plateau", ]
AEZ <- AEZ[4]
#colors <- c("#FF4500", "#8A2BE2", "#3CB371", "#FFD700", "#00FFFF", "#BA55D3", "#F08080", "#20B2AA", "#778899", "#FF69B4", "#00FF7F", "#4682B4", "#8B008B")

ggmap <- ggplot()+
  geom_sf(data = NGAshp0, linewidth = 1.2, color = "black", fill=NA) + 
  geom_sf(data = AEZ, linewidth = 0, aes(fill = AEZ$AEZ2)) +
  geom_sf(data = NGA_dist, linewidth = 0.5, color = "black", fill=NA) +
  geom_sf_text(data = NGA_dist, aes(label = NAME_1),
               size = 4,  # Adjust the text size as needed
               color = "black",  # Adjust the text color as needed
               nudge_x = 1,  # Optional horizontal nudging
               nudge_y = 0.7)+  
  geom_sf_text(data = NGAshp0, aes(label = COUNTRY),
               size = 6,  # Adjust the text size as needed
               color = "black",  # Adjust the text color as needed
               nudge_x = 1,  # Optional horizontal nudging
               nudge_y = 5)+# Optional vertical nudging
  geom_point(data = ds, aes(x=as.numeric(lon), y=as.numeric(lat), colour = cluster, size = cluster))+
  labs(fill = "Agroecological zone",# Specify custom title for color legend
       color = "Cluster", 
       size = "Cluster") +
  scale_shape_manual(values = c(12, 12, 12,12,12,12))+
  scale_size_manual(values = c(1,1,1,1,1,1))+
  #scale_colour_manual(values = c("green","red", "blue", "black","yellow","purple"))+
  scale_fill_manual(values = c("darkgoldenrod1", "darkgoldenrod", "burlywood","saddlebrown", "peru", "blue"))+
  theme(axis.title = element_blank(),
        axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        strip.text = element_text(size=14, face="bold"))

ggmap
ggsave(paste(pathIn, "Potato trial_locations.pdf", sep=""), ggmap, width=8, height=8)

 ###############################
# 4. fit linear mixed effects model
###############################
str(ds)
unique(ds$district) ## only one so no use
unique(ds$Season) ## just one season
unique(ds$cluster)  
unique(ds$LGA)
unique(ds$var) ## what is the effect of these varieties?
hist(ds$TY)
ds <- droplevels(ds[ds$TY <= 30000, ])

ggplot(ds, aes(var, TY, fill=Treatment)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=45, hjust=1))
  

ds$TLID <- as.factor(ds$TLID)
ds$LGA <- as.factor(ds$LGA)
ds$cluster <- as.factor(ds$cluster)


#################################################################################
#################################################################################
## LME 

#create variables to deal with scale issues:
ds$N100 <- ds$N/100
ds$P100 <- ds$P/100
ds$K100 <- ds$K/100


#base model with independent parabolic response curves, fixed season effect, and random TL intercepts:
fit0 <- lmer(log(TY) ~ N + P + K + (1|TLID), data=ds)
plot(fit0)

## remove data with large residual error

ds$resError <- resid(fit0)
hist(ds$resError)
ds <- ds[ds$resError > -1.5, ]
hist(ds$TY)

fit0 <- lmer(log(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + (1|TLID), data=ds)
fit1 <- lmer(log(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + cluster + (1|TLID), data=ds)
fit2 <- lmer(log(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + var + cluster + (1|TLID), data=ds)
fit3 <- lmer(log(TY) ~ N + P + K + I(N100**2) + I(P100**2) + I(K100**2) + var + (1|TLID), data=ds)

anova(fit0, fit3)
anova(fit0, fit1)
anova(fit0, fit2)
anova(fit1, fit2)

## not variety but cluster is important for the model 

#updated model allowing fixed two- and three-way interactions between N, P and K: 
fit3 <- update(fit1, . ~ . + N100:P100 + N100:K100 + P100:K100 + N100:P100:K100)
anova(fit3, fit1) ## no improvement of model, keep fit1


#updated model adding random slopes:
fit4 <- update(fit1, . ~ . +(0 + N100|TLID) +(0 + P100|TLID) +(0 + K100|TLID))
anova(fit4, fit1) ## keep fit 4


ds$blup <- exp(predict(fit4, ds))
summary(ds)


###############################
# 4. evaluate the effect of using linear mixed effects model
###############################

#plot showing relationship between observations (with random error) and BLUPs (without random error)
gg5 <- ggplot(ds, aes(x = blup, y = TY)) + 
  geom_point(alpha=.33, shape=16) +
  geom_abline(intercept = 0, slope = 1) +
  stat_poly_line(formula = y ~ x, se = F) +
  stat_poly_eq(use_label(c("eq", "R2")),
               formula = y ~ x, size = 5)+
  xlab("\nBLUP Potato yield [kg/ha]") +
  ylab("Observed Potato yield [kg/ha]\n") +
  ggtitle("Performance of linear mixed effect model")+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust = 0.5, size=16),
        strip.text = element_text(size=14, face="bold"))
gg5
ggsave(paste(pathIn, "LME_performance.pdf", sep=""), gg5, width=8, height=8)




#plot illustrating that the elimination of random error results in more meaningful structure in yield response:
## take the NPK treatment as reference 120:60:90
ds %>%
  gather(variable, value, c(TY, blup)) %>%
  group_by(TLID, variable) %>%
  mutate(refY = ifelse(Treatment == "NPK", value, NA),
         refY = mean(refY, na.rm=TRUE),
         dY = refY - value,
         variable = factor(variable, levels=c("TY", "blup")),
         variable = mapvalues(variable,
                              from = c("TY", "blup"),
                              to = c("Raw observations", "BLUPs"))) %>%
  filter(!(N >= 90 & P >= 40 & K >= 75))

ds2 <- NULL
for (tid in unique(ds$TLID)){
  print(tid)
  locdata <- droplevels(ds[ds$TLID == tid, ])
  locdata$refTreatment <- ifelse(locdata$Treatment == "NPK", TRUE, FALSE)
  ## for trials without the global referenceTreatment, use it own highest global NPK rate
  if(all(locdata$refTreatment == FALSE)){
    locdata$refTreatment <- ifelse(locdata$Treatment == "NP", TRUE, FALSE)
    if(all(locdata$refTreatment == FALSE)){
      locdata$refTreatment <- ifelse(locdata$Treatment == "NK", TRUE, FALSE)
    }
  }
  wfd <- locdata[,c("Treatment", "N", "P", "K", "TY", "blup","refTreatment","TLID","plotID")]
  if(nrow(wfd[wfd$refTreatment == TRUE, ]) >1){
    print(TLID)
  }
  
  refyield <- max(wfd[wfd$refTreatment == TRUE, "TY"])
  refyieldBlup <- max(wfd[wfd$refTreatment == TRUE, "blup"])
  wfd$refY <- refyield
  wfd$refYBLUP <- refyieldBlup
  wfd$yieldEffectraw <- ifelse(wfd$refTreatment == TRUE, 0,  refyield - wfd$TY )
  wfd$yieldEffectBlup <- ifelse(wfd$refTreatment == TRUE, 0,  refyieldBlup - wfd$blup )
  locdata <- merge(locdata, wfd, by=c("Treatment","N","P","K","TY", "blup","refTreatment", "TLID","plotID"))
  ds2 <- rbind(ds2, locdata)
  
}
ds2 <- unique(ds2)

str(ds2)

gg6 <- ggplot(ds2, aes(x = refY, y = yieldEffectraw)) + 
  geom_point(shape=3) +
  geom_hline(yintercept = 0) +
  #facet_grid(cluster ~ .) + 
  xlab("\nYield in referenceTreatment [kg/ha]") +
  ylab("Yield difference relative to referenceTreatment [kg/ha]\n") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")
gg6

gg7 <- ggplot(ds2, aes(x = refY, y = yieldEffectBlup)) + 
  geom_point(shape=16) +
  geom_hline(yintercept = 0) +
  #facet_grid(cluster ~ .) +  
  ylim(min(ds2$yieldEffectraw), max(ds2$yieldEffectraw))+
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(axis.title = element_text(size = 14, face="bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face="bold"),
        legend.position = "none")
gg7
ggsave(paste(pathIn, "BLUP_reducingRandomError.pdf", sep=""), grid.arrange(gg6, gg7, ncol=2), width=12, height=8)


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
country <- "Nigeria"
useCaseName <- "MercyCorps"
Crop <- "Potato"

source("~/agwise-responsefunctions/dataops/responsefunctions/Scripts/generic/QUEFTS_functions.R")

pathIn <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")
pathOut1 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/transform/", sep="")
pathIn2 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName, "/", Crop, "/Landing/", sep="")
pathIn3<-paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")

# if (!dir.exists(pathOut1)){
#   dir.create(file.path(pathOut1), recursive = TRUE)
# }

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

AEZ <- readOGR(dsn='~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Nigeria_MercyCorps/Potato/Landing/',  layer="Nigeria_AEZ")
nga_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
datasoil <- as.data.frame(raster::extract(nga_aez, gpsPoints))
datasoil$lat <- gpsPoints$y
datasoil$lon <- gpsPoints$x

ds <- merge(ds, datasoil, by=c("lat", "lon"))
ds$AEZ <- ds$AEZ2
ds <- subset(ds, select=-c(id.y, GRIDCODE,Area,AEZ2))

head(ds)

#############################################################
## estimate the soil INS using reverse QUEFTS procedure 
# 1. estimate soil INS without Blanket", "Control"
# 2. validate the soil INS using Blanket", "Control" yield
#############################################################

calculate_supply <- function(TLID){
  print(TLID)
  dsi <- ds[ds$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y"
  dsi$Y <- dsi$Y * 0.20 #converting to kg DM/ha, assuming 75% moisture content
  yy <- dsi[dsi$Y == max(dsi$Y), ]
  Yai <- mean(yy$Y) * 1.5 ## given for potato the yield increase is still in the linear space of the response curve with the fert rate we are using, higher refYield is set  
  dsi <- dsi[!dsi$Treatment %in% c("Blanket", "Control"), ]
  if(length(unique(dsi$Treatment)) > 2 & !is.na(Yai)){  #at least 3 rows of data are needed + attainable yield:
    
    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = "Potato")
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

cls <- parallel::makePSOCKcluster(10)
doParallel::registerDoParallel(cls)
supply <- foreach(i = unique(ds$TLID)) %do% {calculate_supply(TLID = i)}
stopCluster(cls)
supply <- do.call(rbind, supply)

saveRDS(supply, "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Nigeria_MercyCorps/Potato/raw/soilINS_DW.RDS")
supply <- readRDS("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Nigeria_MercyCorps/Potato/raw/soilINS_DW.RDS")

head(supply) ### Note Ya is ~ WLY for a site and it is still in DW
summary(supply)
supply <- supply %>%
  mutate(across(N_base_supply:K_base_supply, ~ ifelse(.x < 0, 0.1, .x)),
          K_base_supply = ifelse(K_base_supply > 125, 125, K_base_supply),
         # N_base_supply = ifelse(N_base_supply > 80, 80, N_base_supply),
          P_base_supply = ifelse(P_base_supply < 0, 0, P_base_supply))


###################################################
## use the supply and estimate yield estimate to validate QUEFTS
## this takes the soil INS from reverse QUEFTS, add the NPK from teh fertilizer and estimate the yield for every treatment
###################################################
ds_validate <- unique(ds[, c("Treatment", "cluster","LGA","N","P","K","blup", "TLID", "plotID", "refTreatment")])
ds_validate$index <- c(1:nrow(ds_validate))

supply_Qy <- NULL
for (i in unique(ds_validate$index)){
  print(i)
  tdata1 <- ds_validate[ds_validate$index == i, ]
  tdata2 <- supply[supply$TLID==tdata1$TLID, ]
  
  ## attainable yield in t/ha and dry wt.
  yya <- tdata2$Ya
  
  if(nrow(tdata2) > 0){
    tdata1$yieldQUEFTS <- runQUEFTS(nut_rates = data.frame(N=tdata1$N, P=tdata1$P, K=tdata1$K),
                                    supply = c(tdata2$N_base_supply, tdata2$P_base_supply, tdata2$K_base_supply),
                                    crop = Crop,
                                    Ya = yya,
                                    SeasonLength = SeasonLength)
    
    # from dry matter to fresh weight
    tdata1$yieldQUEFTS <- tdata1$yieldQUEFTS / 0.20
    
    supply_Qy <- rbind(supply_Qy, tdata1)
  }
}


saveRDS(supply_Qy, "supply_Qy.RDS")
supply_Qy <- readRDS("supply_Qy.RDS")


ggC <- ggplot(supply_Qy[supply_Qy$Treatment %in% c("Control", "Blanket"), ], aes(blup, yieldQUEFTS)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~Treatment, scales="free") +
  ggpmisc::stat_poly_line(formula = y ~ x, se = F) +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2")),
                        formula = y ~ x, size = 6,
                        label.y = .975) +
  xlim(2500, 25000) + ylim(2500, 25000)+
  xlab("Observed yield (t/ha)") + ylab("QUEFTS predicted yield using soil INS (t/ha)") +
  theme_bw() +
  theme(axis.text= element_text(size=12))
ggC

ggsave(paste(pathOut1, "revQUEFTS_Yield_soilINS.pdf", sep=""), ggC, width = 12, height = 10)

######################################################################

INS <- supply %>%
  left_join(ds %>% dplyr::select(TLID, lat, lon, plotID, Treatment,cluster,LGA, AEZ, blup) %>% unique()) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  na.omit()

#Create plot to demonstrate ranges in supply by expCode and season combinations:
ggINS <-INS %>%
  gather(variable, value, N_base_supply:K_base_supply) %>%
  mutate(variable = factor(variable, levels = c("N_base_supply", "P_base_supply", "K_base_supply")),
         variable = revalue(variable, c("N_base_supply" = "N",
                                        "P_base_supply" = "P",
                                        "K_base_supply" = "K"))) %>%
  ggplot(aes(x = cluster, y = value, fill =cluster )) + 
  geom_boxplot()+
  scale_fill_manual(values = c("grey30", "grey60","grey70"))+
  facet_wrap(~variable, nrow=1) +
  theme_bw() +
  ylab("Indigenous nutrient supply (kg/ha)\n") +
  theme(axis.title.y = element_text(size = 15, face="bold"),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(size = 12, angle=45, hjust=1),
        strip.text = element_text(size = 14, face="bold"))
ggINS
ggsave(paste(pathOut1, "cluster_soilINS.pdf", sep=""), ggINS, width = 6, height = 6)


## TODO 
## Since the data is coming from trials located very close with each other, using geo-spatial data will not work for this application
## because the geo-spatial data is not variable within the sampling zone so it does not add any information to the analytics

## for every trial, there is soil INS, yield from the blanket recommendation, attainable yield used in the reverse QUEFTS (yield in DW)
## estimate how much more NPK will be needed to get x% yield increase from the blanket yield
## add the amount of NPK needed to get the blanket an have total NPK for 
## aggregate soil INS by Cluster * LGA, assuming cluster and LGA is some sort of zonning for the user in the area
## run cost benefit for every x% yield increase and see where profit is maximum


head(INS)
head(supply_Qy)
INS <- INS %>%
  left_join(supply_Qy %>% dplyr::select(TLID, Treatment, yieldQUEFTS) %>% unique())



INS_Blanlet <- INS %>% 
  dplyr::filter(Treatment == "Blanket") %>% 
  dplyr::select(c(TLID, cluster, LGA, N_base_supply, P_base_supply,K_base_supply, Ya, blup)) %>% 
  dplyr::mutate(BlanketY = blup * 0.20 ) %>% 
  dplyr::rename(soil_N = N_base_supply,
                soil_P = P_base_supply,
                soil_K = K_base_supply) %>% 
  unique()
  head(INS_Blanlet) ## all 


###########################################################
## Calculate what yield the current blanket recommendation (650 kg = 13 bags of NPK 15:15:15 per ha at $ 18.16 / 50 kg)
## price of potato is $ 500 / ton
###########################################################

my_ferts <- data.frame(name = c("NPK151515", "Urea", "MOP", "TSP"), 
                       N = c(15, 46, 0, 0), P = c(6.6, 0, 0, 20), K = c(12, 0, 50, 0),
                       price=c(18.16, 21.57, 31.90, 35.29))
  
my_ferts$priceNratio <- my_ferts$price/my_ferts$N
my_ferts$pricePratio <- my_ferts$price/my_ferts$P
my_ferts$priceKratio <- my_ferts$price/my_ferts$K
## in-terms of price NPK 15:15:15 is not wise
## use Urea as basal and then give K rich fertilizer and then P

my_ferts <- my_ferts %>% dplyr::filter(name != "NPK151515")


### fertilizer recommendation by cluster and LGA  
fertrecom_sector <- NULL
for(j in 1:nrow(INS_Blanlet)){
  fertperc <- NULL
  for(perc in seq(0.1, 1, 0.1)){
    print(perc)
    fertrecom1 <-   rec_targetdY(my_ferts=my_ferts, 
                                 dY = perc, 
                                 target = "relative", 
                                 start = rep(1, nrow(my_ferts)), 
                                 supply = c(INS_Blanlet$soil_N[j], INS_Blanlet$soil_P[j], INS_Blanlet$soil_K[j]),
                                 att_GY = INS_Blanlet$Ya[j] , 
                                 GY_br = INS_Blanlet$BlanketY[j] ,
                                 crop = "Potato",
                                 SeasonLength = 120,
                                 isBlanketRef = TRUE, 
                                 df_link  = INS_Blanlet[j, ])
    fertperc <- rbind(fertperc, fertrecom1)
  }
  fertrecom_sector <- rbind(fertrecom_sector, fertperc)
}


saveRDS(fertrecom_sector, paste(pathOut1, "fertrecom_sector.RDS", sep=""))
head(fertrecom_sector)

# TODO 
pathOut1 <- "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Nigeria_MercyCorps/Potato/transform/"
fertrecom_sector <- readRDS(paste(pathOut1, "fertrecom_sector.RDS", sep=""))
head(fertrecom_sector)
fertrecom_sector[fertrecom_sector$LGA == "Bassa" ,]

### changing DW to FW
fertrecom_sector <- fertrecom_sector %>% 
  dplyr::mutate(Blanket_FWY = BlanketY  / 0.2,
                Target_FWY = targetYield / 0.2,
                Attainable_FWY = Ya / 0.2) %>% 
  dplyr::select(c(cluster,LGA,soil_N,soil_P,soil_K,Attainable_FWY, Blanket_FWY, Target_FWY, yieldPercinc, Urea, MOP, TSP))

## aggregate by percent increase, LGA and cluster
fertrecom_agg <- ddply(fertrecom_sector, .(cluster, LGA, yieldPercinc), summarise, 
                       soil_Nagg = mean(soil_N),
                       soil_Pagg = mean(soil_P), 
                       soil_Kagg = mean(soil_K),
                       Attainable_FWYagg = mean(Attainable_FWY),
                       Blanket_FWYagg = mean(Blanket_FWY)
)
fertrecom_agg[fertrecom_agg$LGA == "Bassa" ,]

# 
# 
# 
# fertrecom_agg <- fertrecom_sector %>% 
#   group_by(cluster, LGA, yieldPercinc) %>% 
#   summarise(soil_Nagg = mean(soil_N), soil_Pagg = mean(soil_P), soil_Kagg = mean(soil_K),
#             Attainable_FWYagg = mean(Attainable_FWY),
#             Blanket_FWYagg = mean(Blanket_FWY))


fertrecom_sector[fertrecom_sector$LGA == "Bassa" & fertrecom_sector$yieldPercinc == "10 %",]

fertrecom_sector <- fertrecom_sector %>% dplyr::filter(Urea > 10 & MOP > 10 & TSP > 10)
fertrate_agg <- ddply(fertrecom_sector, .(cluster, LGA, yieldPercinc), summarise, 
                       ureaAgg = median(Urea), 
                       MOPAgg = median(MOP), 
                       TSpAgg = median(TSP))
fertrate_agg[fertrate_agg$LGA == "Bassa" ,]

fertRate_NG_potato <- merge(fertrecom_agg, fertrate_agg, by=c("cluster", "LGA", "yieldPercinc"))
fertRate_NG_potato[fertRate_NG_potato$LGA == "Bassa" ,]

fertRate_NG_potato$targetYield <- ifelse(fertRate_NG_potato$yieldPercinc  == "10 %", fertRate_NG_potato$Blanket_FWYagg *1.1,
                                         ifelse(fertRate_NG_potato$yieldPercinc  == "20 %", fertRate_NG_potato$Blanket_FWYagg *1.2,
                                                ifelse(fertRate_NG_potato$yieldPercinc  == "30 %", fertRate_NG_potato$Blanket_FWYagg *1.3,
                                                       ifelse(fertRate_NG_potato$yieldPercinc  == "40 %", fertRate_NG_potato$Blanket_FWYagg *1.4,
                                                              ifelse(fertRate_NG_potato$yieldPercinc  == "50 %", fertRate_NG_potato$Blanket_FWYagg *1.5,
                                                                     ifelse(fertRate_NG_potato$yieldPercinc  == "60 %", fertRate_NG_potato$Blanket_FWYagg *1.6,
                                                                            ifelse(fertRate_NG_potato$yieldPercinc  == "70 %", fertRate_NG_potato$Blanket_FWYagg *1.7,
                                                                                   ifelse(fertRate_NG_potato$yieldPercinc  == "80 %", fertRate_NG_potato$Blanket_FWYagg *1.8,
                                                                                          ifelse(fertRate_NG_potato$yieldPercinc  == "90 %", fertRate_NG_potato$Blanket_FWYagg *1.9,fertRate_NG_potato$Blanket_FWYagg *2)))))))))

fertRate_NG_potato <- fertRate_NG_potato[, c("cluster","LGA", "yieldPercinc", "Attainable_FWYagg", "Blanket_FWYagg", "targetYield", "ureaAgg", "MOPAgg", "TSpAgg" )]
names(fertRate_NG_potato) <- c("Cluster","LGA", "percIncrease", "attainableYield", "blanketYield", "targetYield", "Urea", "MOP", "TSP" )
write.csv(fertRate_NG_potato, "~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Nigeria_MercyCorps/Potato/transform/fertRate_NG_potato.csv")


#### translate the 650 NPK151515 to a combination of Urea, MOP and TSP
N=650*15
P=650*6.6
K=650*12

fertrecom_sector <- fertrecom_sector %>% 
  dplyr::mutate(N_tot = 90 + N_base_supply,
                P_tot = 40 + P_base_supply,
                K_tot = 75 + K_base_supply,
                BlanketY = blup * 0.20 )

## TODO change fertilizer price
fertrecom_sector$fertilizerCost <- fertrecom_sector$NPK151515 * 18.16 + 
  fertrecom_sector$Urea * 21.57 + fertrecom_sector$MOP * 31.90 +
  fertrecom_sector$TSP * 35.29






















