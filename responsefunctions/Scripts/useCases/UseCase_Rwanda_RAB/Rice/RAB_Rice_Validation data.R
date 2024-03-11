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
pathIn <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Rice/raw/")
pathOut <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_Rwanda_RAB/Rice/transform/validation/")
#get the data
ds <- readRDS(paste(pathIn, "validationData.RDS", sep=""))
nrow(ds)
ds %>% dplyr::select(intro.wrong_ENID_1, intro.wrong_ENID_1, intro.enumerator_ID, intro.wrong_ENID) %>% unique()
ds %>% dplyr::select(planting.plantingDetails.variety, planting.plantingDetails.variety_other) %>% unique()
ds %>% dplyr::select(intro.wrong_ENID_1, intro.wrong_ENID_1, intro.enumerator_ID, intro.wrong_ENID) %>% unique()

### check differences in effect by varieties 

ds <- ds %>%
  dplyr:: filter(crop =="rice") %>%
  dplyr::rename(Date = today,
                enID = intro.wrong_ENID,
                hhID = intro.wrong_ID,
                lon = intro.longitude,
                lat = intro.latitude,
                event = intro.event,
                crop = crop,
                Variety3 = Variety3,
                marshLand = Marshland,
                plantingDate = planting.plantingDetails.planting_date,
                fertOrg = fertilizer_org_used,
                plantDensity = planting.plantingDetails.plant_density,
                BR_area = plotDescription.plotLayout_BR.plot_area_control,
                SSR1_area = plotDescription.plotLayout_AEZ.plot_area_aez,
                SSR2_area = plotDescription.plotLayout_SSR.plot_area_ssr,
                BR_PlantNr = plotDescription.plantStand.plant_number_BR,
                SSR1_PlantNr = plotDescription.plantStand.plant_number_SSR1,
                SSR2_PlantNr = plotDescription.plantStand.plant_number_SSR2,
                BR_riceGrainsFW = cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_BR,             
                BR_riceGrainsMC = cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_BR,             
                SSR1_riceGrainsFW = cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_SSR1,           
                SSR1_riceGrainsMC = cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_SSR1,           
                SSR2_riceGrainsFW = cropManagement.Harvest.harvest.rice_harvest.riceGrains_FW_SSR2,           
                SSR2_riceGrainsMC  = cropManagement.Harvest.harvest.rice_harvest.riceGrains_MC_SSR2) %>%
  dplyr::mutate(plantingDate = as.Date(plantingDate, format="%d/%m/%Y"),
                harvestDate = as.Date(Date, format="%d/%m/%Y"),
                Date = as.Date(Date, format="%d/%m/%Y"))%>%
  dplyr::mutate(LGP = harvestDate-plantingDate)%>%
  dplyr::select(lat,lon,enID,hhID,plantingDate,harvestDate,LGP,crop,event,Variety3,marshLand,BR_area,SSR1_area,SSR2_area,BR_PlantNr,SSR1_PlantNr,SSR2_PlantNr,
                BR_riceGrainsFW,BR_riceGrainsMC,SSR1_riceGrainsFW,SSR1_riceGrainsMC,
                SSR2_riceGrainsFW,SSR2_riceGrainsMC)%>%
  dplyr::mutate(lat = if_else(lat > 1, lat * -1, lat))

## remove enID == RSENRW000019
 ds <- ds%>% dplyr::filter(enID !="RSENRW000019")%>%unique()



## info on variety:: at one hhID, the GPS readings vary by event so when the hhiD = n/a, it is not possible to link data across events, e.g variety data 
ds_variety <- ds %>% 
  dplyr::filter(Variety3 != "n/a") %>% 
  dplyr::select(c(Variety3, enID, hhID)) %>% 
  mutate(enhhID = paste(enID, hhID, sep="_")) %>% 
  dplyr::select(c(enhhID, Variety3)) %>% unique()

ds_variety$Variety3 <- gsub(" ", "", ds_variety$Variety3)
unique(ds_variety$Variety3)
## filter rows only with yield data
ds_filtr <- ds%>%
  dplyr::filter(BR_riceGrainsFW != "n/a" & SSR1_riceGrainsFW != "n/a" & SSR2_riceGrainsFW != "n/a") %>%
  dplyr::select(-c(event, Variety3,plantingDate, harvestDate,LGP, crop, BR_PlantNr, SSR1_PlantNr, SSR2_PlantNr)) %>% 
  unique()

## check how many data points by hhiD
ds_filtr %>% dplyr::select(lat,lon, hhID) %>%  unique() %>% dplyr::count(hhID)

## use location to identify households instead of the hhiD, because there are some n/a in hhID
## add variety info
ds_filtr %>% dplyr::filter(hhID =="n/a")%>%unique()
ds_filtr <- ds_filtr %>% 
  mutate(location = paste(lon, lat, sep="_"),
         enhhID = paste(enID, hhID, sep="_"))
ds_filtr <- merge(ds_filtr, ds_variety, by="enhhID", all.x = TRUE)

head(ds_filtr)
length(unique(ds_filtr$location)) ##157 validation trials 

## which varieties are used 
gg1 <- ggplot(ds_filtr, aes(x = Variety3, fill = Variety3)) +
  geom_bar(stat = "count", width = 0.5) +
  labs(title = "Total Number of Observations by Rice varieties",
       x = "Variety",
       y = "Count") +
    theme_minimal() +
            theme(axis.title = element_text(size = 12, face="bold"),
                  plot.title = element_text(hjust = 0.5, face = "bold"),
                  axis.text.x = element_text(angle=90, hjust=1, vjust=1, size=12),
            legend.position = "none")
gg1
ggsave(paste(pathOut,"Varieties.pdf"), gg1, width=8, height = 6)

head(ds_filtr)
ds_filtr <- ds_filtr %>%
  dplyr::mutate(across(c(
    BR_area, SSR1_area, SSR2_area,
    BR_riceGrainsFW, SSR1_riceGrainsFW, SSR2_riceGrainsFW,
    BR_riceGrainsMC, SSR1_riceGrainsMC, SSR2_riceGrainsMC), as.numeric)) %>% 
  dplyr::rename(
      SSR_riceGrainsFW=SSR1_riceGrainsFW,
      IBR_riceGrainsFW = SSR2_riceGrainsFW,
      SSR_area = SSR1_area,
      IBR_area = SSR2_area, 
      SSR_riceGrainsMC = SSR1_riceGrainsMC,
      IBR_riceGrainsMC = SSR2_riceGrainsMC)

## yield distribution by treatment 
par(mfrow=c(2,2))
hist(ds_filtr$BR_riceGrainsFW,main="BR")
hist(ds_filtr$SSR_riceGrainsFW, main="SSR")
hist(ds_filtr$IBR_riceGrainsFW, main="IBR")

### a very extreme yield for BR    
ds_filtr[ds_filtr$hhID == "RSHHRW000322", ]
ds_filtr <- ds_filtr[ds_filtr$BR_riceGrainsFW <= 60, ]

## yield distribution by treatment 
par(mfrow=c(2,2))
hist(ds_filtr$BR_riceGrainsFW,main="BR", xlim=c(0,35), xlab=("Rice FW yield"))
hist(ds_filtr$SSR_riceGrainsFW, main="SSR", xlim=c(0,35), xlab=("Rice FW yield"))
hist(ds_filtr$IBR_riceGrainsFW, main="IBR", xlim=c(0,35), xlab=("Rice FW yield"))


DW_rice <- function(area, riceGrainsFW, riceGrainsMC) {
  yield_per_area <- riceGrainsFW / area
  # Ya.DW <-yield_per_area* 0.86 #convert to dry weight t/ha at 14% moisture content
  Ya.DW <- (yield_per_area* 0.14)/(riceGrainsMC)#convert to dry weight t/ha at 14% moisture content, taking the moisture at yield measurment
  YaDW <- Ya.DW*10000/1000 
  return(YaDW)
}


ds_rice <- ds_filtr %>% 
  mutate(BR_riceGrainsMC = BR_riceGrainsMC/100, 
         SSR_riceGrainsMC = SSR_riceGrainsMC/100, 
         IBR_riceGrainsMC = IBR_riceGrainsMC/100) %>% 
  tidyr::pivot_longer(cols = starts_with(c("BR", "IBR", "SSR")),
                      names_to = c("trialCode", ".value"),
                      names_sep = "_")%>%
  dplyr::mutate(YaDW = DW_rice(area, riceGrainsFW, riceGrainsMC)) %>% 
  unique()
head(ds_rice)

ds_rice[ds_rice$YaDW < 1, ]$YaDW
ds_rice[ds_rice$enhhID == "RSENRW000034_RSHHRW000335", ]

### drop this extreme outlier 
ds_rice <- unique(droplevels(ds_rice[!ds_rice$enhhID == "RSENRW000034_RSHHRW000335", ]))


###### 
pathIn2 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Maize/Landing/", sep="")
AEZ <-  suppressMessages(suppressWarnings(readOGR(dsn=paste(pathIn2, "/AEZ", sep=""),  layer="AEZ_DEM_Dissolve")))
RW_aez <- suppressWarnings(spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84")))
#RW_aez <- RW_aez[RW_aez$Names_AEZs %in% c("Birunga", "Congo-Nile watershed divide", "Buberuka highlands"),]
rwAEZ <- st_as_sf(RW_aez)
names(rwAEZ)[names(rwAEZ) == "Names_AEZs"] <- "AEZ"
names(rwAEZ$AEZ)
ds_rice1<- st_as_sf(ds_rice, coords = c("lon", "lat"), crs = st_crs(RW_aez))
ds_rice <- st_intersection(ds_rice1, rwAEZ )
#plot showing yield ranges by variety and different data sources:
gg1_rice <- ds_rice %>%
  unique() %>% 
  ggplot(aes(x = trialCode, y = YaDW, fill=as.factor(trialCode))) +
  geom_boxplot() +
  theme_bw()+
  ylab("\nRice dry weight yield [t/ha]")+
  theme(axis.title.y = element_text(size = 15, face="bold"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        strip.text = element_text(size = 14, face="bold", hjust=0))
gg1_rice

ggsave(paste(pathOut,"Dryweight yield.pdf"), gg1_rice, width=8, height = 6)



########################################
## Compare yield by treat: BR and IBR are supposed to be similar, while SSR is expected to have more yield
## density plot to see the global distribution 
gg_density <- ds_rice %>%
  ggplot(aes(x = YaDW,
             colour=paste0(trialCode, " (", trialCode, ")"),
             fill=paste0(trialCode, " (", trialCode, ")"))) +
  geom_density(alpha=.2, linewidth=1) +
  # facet_wrap(~AEZ) + 
  ggtitle("Rice, yield density distribution") + 
  xlab("\nRice yield [t/ha]")+
  ylab("Density")+
  theme_bw()+
  theme(axis.title = element_text(size = 12, face="bold"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank())
gg_density
ggsave(paste(pathOut,"Dry Weight density.pdf"), gg_density, width=8, height = 6)

## the BR and IBR are showing major overlap which is a good indication that in most cases they perform comparable
## SSR is shifts to the left indicting a higher performance for SSR over BR and IBR

# Bar Chart (mean yield)
gg3_rice<- ggplot(ds_rice, aes(x = trialCode, y = YaDW)) +
  geom_bar(stat = "summary", fun = "mean", fill = c("blue","red", "yellow"),width = 0.5) +
  ggtitle("Rice Mean Yield by Fertilizer") + 
  xlab("trialCode")+
  ylab("Average Yield [t/ha]")+
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))
gg3_rice

ggsave(paste(pathOut,"Average yield barplot.pdf"), gg3_rice, width=8, height = 6)

ds_rice <- ds_rice %>% 
  mutate(variety3 = as.factor(Variety3),
         trialCode = as.factor(trialCode),
         AEZ = as.factor(AEZ))

mod <- aov(YaDW ~ trialCode * variety3 * AEZ, data = ds_rice)
anova(mod)
TukeyHSD(mod, conf.level=.95)$trialCode




##################################################################
## look at the effect by AEZ, variety, ... 

## 1. by AEZ
gg_AEZ <- ds_rice %>%
  ggplot(aes(x = trialCode, y = YaDW, fill=trialCode)) +
  geom_boxplot() +
  facet_wrap(~AEZ) + 
  ggtitle("Rice, yield by AEZ and package") + 
  theme_bw()+
  ylab("\nRice dry weight yield [t/ha]")+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12, face="bold"),
        axis.title.x = element_blank(),
        legend.title=element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text.x = element_text(size = 12))

gg_AEZ

ggsave(paste(pathOut,"Rice yield by AEZ+Fertlizer package.pdf"), gg_AEZ, width=15, height = 15)
## 2. effect of AEZ and variety: splitting AEZ where there is > 2 varieties are tested from the rest for visualization

gg_AEZ_var1 <- ds_rice %>% ## imbo has unique variety 
  # dplyr::filter(AEZ %in% c("Imbo")) %>% 
  ggplot(aes(x = trialCode, y = YaDW, fill = Variety3)) +
  geom_boxplot() +
  facet_grid(Variety3~AEZ) + 
  ggtitle("Rice, yield by AEZ and variety") + 
  theme_bw()+
  ylab("\nRice dry weight yield [t/ha]")+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12, face="bold"),
        axis.title.x = element_blank(),
        legend.position="none" ,
        # axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(size = 11, face="bold"))
gg_AEZ_var1


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

ds_rice_wide <- ds_rice %>% 
  unique() %>% 
  dplyr::select(enhhID,enID,hhID,location, Variety3,trialCode,YaDW, AEZ) %>% 
  tidyr::spread(key = trialCode, value = YaDW) %>% 
  mutate(IBR_BR = IBR - BR,
         SSR_BR = SSR - BR,
         SSR_BR20 = SSR - (BR + 0.2*BR))


## cumulative distribution plot of yield effects
dev.off()
plot(ecdf(ds_rice_wide$IBR_BR))
plot(ecdf(ds_rice_wide$SSR_BR))
plot(ecdf(ds_rice_wide$SSR_BR20))



ds_rice_wide <- ds_rice_wide %>% 
  mutate(AEZV = paste(AEZ, Variety3, sep="-"))

################# IBR versus BR

ds_rice_wide %>% 
  # dplyr::filter(IBR_BR > -5 & IBR_BR < 5 & AEZ %in% c("Bugesera", "Central plateau", "Granitic ridge", "Mayaga", "Imbo","Eastern plateau", "Eastern savana")) %>% 
  ggplot( aes(IBR_BR, colour = AEZ)) +
  geom_vline(xintercept = 0, linetype="dashed", col="grey2")+
  stat_ecdf(linewidth = 1)+
  facet_wrap(~ AEZV,  ncol=4)+
  xlab("Yield difference [t/ha]")+ ylab("")+
  ggtitle("Rice, yield effect of (IBR - BR) by AEZ varieties")+
  theme_bw()+
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(size = 11, face="bold"))

### for IBR - BR: 
# farmers who get increase yield benefited twice from lower cost of fertilizer and yield increase
# from those who get lower yield with IBR, for some, the yield loss is compensated by fertilizer cost reduction and for others not
# yield effect and fertilizer cost should be translated to money to identify clearly where IBR is not working
# effect of fertilizer is dependent on varieties used  
### Bugesera: the fist variety allow 75% of farmers to see yield increase with IBR over BR, but the other variety the percentage drops to 20%
### Central Plateau: with the NA variety almost all farmers get yield increase with IBR while with the other it is the opposite
### Imbo: one variety allows 75% of farmers to be benefited twice from yield increase and low fertilizer investment the other not
### Mayaga: both varieties make 75% farmers see yield increase. 

################ SSR versus BR 

ds_rice_wide %>% 
  ggplot( aes(SSR_BR, colour = AEZ)) +
  geom_vline(xintercept = 0, linetype="dashed", col="grey2")+
  stat_ecdf(linewidth = 1)+
  facet_wrap(~ AEZV,  ncol=4)+
  xlab("")+ ylab("")+
  ggtitle("Rice, yield effect of (SSR - BR) by AEZ - varieties")+
  theme_bw()+
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(size = 11, face="bold"))




##################################### when the SSR is compared with strictly 20% + BR  

ds_rice_wide %>% 
  # dplyr::filter(SSR_BR20 > -5 & AEZ %in% c("Bugesera", "Central plateau", "Granitic ridge", "Mayaga", "Imbo","Eastern plateau", "Eastern savana")) %>% 
  ggplot( aes(SSR_BR20, colour = AEZ)) +
  geom_vline(xintercept = 0, linetype="dashed", col="grey2")+
  stat_ecdf(linewidth=1) +
  facet_wrap(~AEZV,  ncol=4) +
  xlab("Yield difference [t/ha]")+ ylab("")+
  ggtitle("Rice, yield effect of (SSR - BR+20%) by AEZ - varieties")+
  theme_bw()+
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(size = 11, face="bold"))



### for SSR vs BR:
# in most cases the best case scenario is 50% of farmers realizing the 20% yield increase
## but in Bugesera all farmers do not achiev a 20% yield increase and in Central Plateau one vairety, all farmers get 20% yield increase
# the hypothesis was with 50 kg fertilizer increase, 20% yield increase form BR, but it is possible the yield 
# increase is not 20% + BR yield but the investment could be still profitable.
# The result shows, in most of the AEZ, SSR is beneficial for 75% to 100% of the farmers except in Bugesera and Eastern Savana


### get cost benfit
### cost 
ricepriceton <- 500000 
pricefert1kg <- 675

## covert yield diff to money equivalent
## for IBR _BR, add the money saved from using 100 kg less fertilizer as money saved 
## for SSR _BR, add the cost of additional 50 kg fertilizer 
ds_rice_wide <- ds_rice_wide %>% 
  mutate(IBR_BR_price = IBR_BR * ricepriceton,
         SSR_BR_price = SSR_BR * ricepriceton,
         SSR_BR20_price = SSR_BR20 * ricepriceton,
         fert100kgcost = 20 * pricefert1kg,
         fert50kgcost = 25 * pricefert1kg,
         IBR_BR_profit = IBR_BR_price + fert100kgcost,
         SSR_BR_profit = SSR_BR_price - fert50kgcost, 
         SSR_BR20_profit = SSR_BR20_price - fert50kgcost)


## IBR_BR: 
### 85 farmers have higher IBR yield than BR, they pass twice
## 60 have less yield with IBR but for 8 of these the loss due to low yield was positively compensated by low fertilizer investement 
## low yield ok some are still ok because fert100kgcost - IBR_BR_price is > 0
nrow(ds_rice_wide[ds_rice_wide$IBR_BR >= 0,]) ## 95 hhID benfited twice

ds_rice_wide %>% dplyr::filter(IBR_BR < 0)%>% 
  unique() %>% 
  dplyr::filter( IBR_BR_profit >= 0) %>% 
 nrow() ## 2 made profit 
 ### IBR vs BR : 95 farmers benefited twice, 2 saved money : 97/155 = 63 % is positive 

##########################################################################



## low yield ok some are still ok because fert100kgcost - IBR_BR_price is > 0

nrow(ds_rice_wide[ds_rice_wide$SSR_BR20 >= 0, ]) ## 74 hhId with min 20% yield increase
D1 <- ds_rice_wide %>% dplyr::filter(SSR_BR20 < 0) %>% unique()
nrow(D1) # 81

ds_rice_wide %>% dplyr::filter(SSR_BR > 0 & SSR_BR20 < 0) %>% 
  unique() %>% ## 56 hhid yield incrase but not 20%
  dplyr::filter( SSR_BR_profit >= 0) %>% 
nrow() ## 56 hhID, yield increase < 20% but still made prfit by using SSR

(74 + 56)/155 ## 84% hhiD benefiting from using SSR



## Does IBR gave at least the same yield as BR
x <- ds_rice_wide$IBR_BR
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
  layout(title = 'Effects on grain yield of IBR vs BR')

## wrt profit
pos <- 63 
neg <- 37
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


a <- ds_rice_wide$SSR_BR
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
  layout(title = 'Effects on grain yield of SSR vs BR')


## if the SSR yield increase 20% + the BR yield?
Q <- ds_rice_wide$SSR_BR20
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
  layout(title = 'Effects on grain yield of SSR vs BR')

### interms of profitability 
pos <- 84
neg <- 16
ds_SSRBR_profit <- data.frame(labels = c("Yield Difference <br> (SSR profit - BR profit)", "Profitable", "Not profitable"),
                       values = c(NA, pos, neg))

plot_ly(data = ds_SSRBR_profit,
        labels = ~labels,
        values = ~values,
        parents = c("", "Profit based <br> (SSR profit - BR profit)", "Profit based <br> (SSR profit - BR profit)"),  # Adjusted parents based on the hierarchy
        type = "sunburst",
        branchvalues = 'total',
        textinfo = "label+percent entry") %>% 
  layout(title = 'Effects on grain yield of SSR vs BR')



