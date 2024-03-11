#CUSTOM QUEFTS FUNCTION to calculate yield for a data frame with fertilizer nutrient application rates
#Requires estimates of soil N, P and K supply, and the attainable yield.
#nut_rates = data frame with headers N, P and K, and the nutrient rates applied in kg/ha. 
#supply = vector with indigenous soil N, P and K supply in kg/ha
#crop = crop for which to run Rquefts, by default "Maize"
#att_GY = attainable yield (without nutrient limitations) in kg/ha
#SeasonLength = required parameter to estimate the attainable yield, default = 120 days
#TODO: Other QUEFTS parameters can be added...

runQUEFTS <- function(nut_rates, #df with columns N, P and K with fertilizer nutrient rates in kg/ha
                      supply, #vector with N, P and K supply
                      crop = c("Maize", "Potato", 'Rice'), #crop to be defined by QUEFTS
                      Ya, #attainable yield
                      SeasonLength = 120){ #parameter important for the attainable yield estimation
  
  require("Rquefts")
  

  
  qCrop <- quefts_crop(crop) #crop params for QUEFTS
  
  qSoil <- quefts_soil() #soil params for QUEFTS
  #replacing the default by the values for N, P and K supply:
  qSoil$N_base_supply <- supply[1]
  qSoil$P_base_supply <- supply[2]
  qSoil$K_base_supply <- supply[3]
  
  #attainable yield, assuming ds contains a treatment which eliminates nutrient deficiency
  ## assuming the leaf ratio is leaf biomass estimation knowing the yield (leaf / yield)
  
  hi <- data.frame(crop       = c("Maize", "Potato", "Rice"),
                   leaf_ratio = c(0.46, 0.17, 0.18),
                   stem_ratio = c(0.56, 0.14, 0.67))
  

  
  qYa <- list(leaf_att  = Ya * hi[hi$crop == crop,]$leaf_ratio, #assumed ratio based on mean HI
              stem_att  = Ya * hi[hi$crop == crop,]$stem_ratio, #assumed ratio based on mean HI
              store_att = Ya, 
              SeasonLength=120)  
  
  Yq <- NULL
  for(i in 1:nrow(nut_rates)){
    qFertilizer <- list(N=nut_rates$N[i], 
                        P=nut_rates$P[i], 
                        K=nut_rates$K[i])
    q <- quefts(qSoil, qCrop, qFertilizer, qYa)
    Yq <- c(Yq, run(q)["store_lim"])
  }
  
  return(as.vector(Yq))
  
}

#REVERSE QUEFTS FUNCTION USING THE runQUEFTS function
#Estimates the apparent soil nutrient supply based on a set of observed crop responses in dataset ds
#start = vector with starting values for soil N, P and K supply in kg/ha, by default c(10, 60, 60). 
#ds = dataset with columns N, P and K as the fertiliser nutrient application rates in kg/ha, and
#     Y as the storage organ yield observed for the different fertiliser treatments.
#     This ds must include at least 3 (but 4-5 is better) treatments with orthogonal N, P and K rates,
#     of which one treatment has rates that can be assumed to eliminate nutrient deficiency.
#crop = crop for which to run Rquefts, by default "Maize"
#SeasonLength = required parameter to estimate the attainable yield, default = 120 days
#TODO: Other QUEFTS parameters can be added...
#TODO: add boundaries to avoid that supply values exceed 400 kg/ha (higher values are unlikely).

revQUEFTS <- function(ds,
                      Ya,
                      start = c(60, 10, 60), #starting values for the optimization algorithm.
                      crop = c("Maize", "Potato", "Rice"), #crop to be defined by QUEFTS
                      SeasonLength = 120){ #parameter important for the attainable yield estimation
  
  require("limSolve")
  require("lpSolve")
  

  
  
  #build the function to optimise
  qmo <- function(supply, 
                  ds, 
                  Ya,
                  crop=crop, 
                  SeasonLength=SeasonLength
  ){
    
    #obtain QUEFTS yield estimates for each of the rows in ds
    Yq <- runQUEFTS(nut_rates = subset(ds, select=c(N, P, K)),
                    supply = supply,
                    crop = crop,
                    Ya = Ya,
                    SeasonLength = SeasonLength)
    
    #calculate the total sum of squared errors (SSE) against the observed yields:
    SSE <- sum((Yq - ds$Y)**2)
    
    return(SSE)
    
  }
  
  #now performing the optimisation that minimises SSE:
  result <- optim(start,
                  #method = "L-BFGS-B",
                  #lower = c(0, 0, 0),
                  #upper = c(400, 400, 400),
                  qmo,
                  ds = ds,
                  Ya = Ya,
                  crop = crop,
                  SeasonLength = SeasonLength)
  
  #return the vector with the optimal solution for soil N, P and K supply:
  return(result$par)
  
}



revQUEFTS2 <- function(ds, 
                       start = c(60, 10, 60, 10000), #starting values for INS, IPS and IKS for the optimisation algorithm.
                       crop = c("Maize", "Potato", "Rice"), #crop to be defined by QUEFTS
                       SeasonLength = 120){ #parameter important for the attainable yield estimation
  
  require("limSolve")
  require("lpSolve")
  
  #build the function to optimise
  qmo <- function(inputvalues, 
                  ds = ds, 
                  crop=crop, 
                  SeasonLength=SeasonLength
  ){
    
    #obtain QUEFTS yield estimates for each of the rows in ds
    Yq <- runQUEFTS(nut_rates = subset(ds, select=c(N, P, K)),
                    supply = inputvalues[1:3],
                    crop = crop,
                    Ya = inputvalues[4],
                    SeasonLength = SeasonLength)
    
    #calculate the total sum of squared errors (SSE) against the observed yields:
    SSE <- sum((Yq - ds$Y)**2)
    
    return(SSE)
    
  }
  
  #now performing the optimisation that minimises SSE:
  result <- optim(c(start),
                  #method = "L-BFGS-B",
                  #lower = c(0, 0, 0),
                  #upper = c(400, 400, 400),
                  qmo,
                  ds = ds,
                  crop = crop,
                  SeasonLength = SeasonLength,
                  control = list(trace = TRUE))
  
  #return the vector with the optimal solution for soil N, P and K supply:
  return(result$par)
  
}
  


#Alternative: run reverse QUEFTS in parallel on multiple CPUs
#create a function to run revQUEFTS optimisation:
## TODO this is tested only for potato, need some work to be generic
calculate_supply <- function(TLID, Crop){

  print(TLID)

  #subsetting and preparing data for revQUEFTS:
  dsi <- ds[ds$TLID == i,]
  names(dsi)[names(dsi) == "blup"] <- "Y"
  dsi$Y <- dsi$Y * 1000 * 0.21 #converting to kg DM/ha, assuming 79% moisture content

  #attainable yield is set to 20% above yield obtained with high NPK rate:
  Yai <- mean(dsi[dsi$N > 75 & dsi$P > 30 & dsi$K > 50,]$Y) * 1.2

  #at least 3 rows of data are needed + attainable yield:
  if(length(unique(dsi$treat)) > 2 & !is.na(Yai)){

    si <- revQUEFTS(ds = dsi,
                    Ya = Yai,
                    crop = Crop)
    print(si)

    supply <- data.frame(TLID = i,
                         Ya = Yai,
                         N_base_supply = si[1],
                         P_base_supply = si[2],
                         K_base_supply = si[3])
  }

  return(supply)

}



rec_targetdY_maize <- function(my_ferts,
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




#OPTIMISATION FUNCTION to calculate fertilizer rates to achieve a specified yield increase
#Requires a set of fertilizers and the target yield increase specified in absolute or relative terms.
#my_ferts = data frame with fertilizer details similar to Rquefts::fertilizers()
#dY = target yield increase in absolute terms or relative terms
#     e.g., dY = 2000 and target = "absolute": calculates fertiliser rates required to increase yield by 2000 kg/ha;
#     or, e.g., dY = 0.4 and target = "relative": calculates fertiliser rates required to increase yield by 40%.
#start = start values for optimisation algorithm, set by default to 50 kg/ha for each fertiliser in my_ferts.
##supply = vector with indigenous soil N, P and K supply in kg/ha
#crop = crop for which to run Rquefts, by default "Maize"
#att_GY = attainable yield (without nutrient limitations) in kg/ha
#SeasonLength = required parameter to estimate the attainable yield, default = 120 days
# df_link: is the data frame continating all the other information on marshland and AEZ and used to attache ther fert rate
#TODO: Other QUEFTS parameters can be added...
#TODO: Requires improvement to improve stability across range of start values... 
#      Currently attempts to minimise the total quantity of fertiliser
#      May require constraints to avoid negative values or values below 25 kg/ha.



rec_targetdY <- function(my_ferts,
                         dY,
                         target = c("relative", "absolute"), #is dY relative or absolute? 
                         start = rep(50, nrow(my_ferts)), #starting values for the optimisation algorithm.
                         supply, #indigenous nutrient supply
                         crop = crop, #crop to be defined by QUEFTS
                         att_GY, #attainable yield
                         GY_br, #blnaket recom yield
                         SeasonLength = 120,
                         isBlanketRef = FALSE,
                         df_link){ #parameter important for the attainable yield estimation
  
  
  df_link$Y_att <- att_GY * 1.2
  df_link$yieldPercinc <- paste(dY*100, " %", sep="")
  att_GY <- att_GY * 0.82 * 1000 *1.2
  
  #calculate the control yield $for Rice. this is the yield estimated by QUEFTS for soil INS + blanket recommendation
  if(isBlanketRef == TRUE){
    GY0  <- GY_br * 0.82 * 1000 
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
    df_link$targetYield <- df_link$Y_BR
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


# #An example with a typical nutrient omission treatment design...
# set.seed(777)
# ds <- data.frame(treat = c("CON", "NPK", "NPK", "PK", "NK", "NP"),
#                  N = c(0, 120, 120, 0, 120, 120),
#                  P = c(0, 30, 30, 30, 0, 30),
#                  K = c(0, 60, 60, 60, 60, 0),
#                  Y = c(2000, 6000, 6000, 2500, 4500, 5500) + rnorm(6, 0, 500))
# 
# supply <- revQUEFTS(ds, crop="Potato", Ya = max(ds$Y)) #default starting values
# supply <- revQUEFTS(ds, start = c(50, 50, 50), Ya = max(ds$Y)) #
# supply
# 
# Yq <- runQUEFTS(nut_rates = subset(ds, select=c(N, P, K)),
#                 supply = supply,
#                 Ya = max(ds$Y),
#                 crop = "Maize")
# 
# plot(Yq, ds$Y)
# abline(0,1)
# 
# #An example to calculate nutrient-limited yield:
# runQUEFTS(nut_rates = data.frame("N"=0, "P"=0, "K"=0),
#           supply = c(90,50,130),
#           crop = "Maize",
#           Ya = 12000,
#           SeasonLength = 120)




getRDY <- function(HD, RFY, country){
  #SHORT DEF:   Function to convert root FM yield into root dry matter yield (RDY): user define CY in FM in ton/ha, QUEFTS require Cy in DM kg/ha
  #RETURNS:     RDY: root dry yield in the same units as root FM yield input
  #INPUT:       HD: harvest date (Date format)
  #             RFY: root fresh matter yield (user's units)
  #             country = c("NG", "TZ")
  d <- HD
  fd <- read.csv("/home/akilimo/lintul/dataSources/fd2.csv")
  DC <- merge(data.frame(dayNr=d), fd[fd$country==country,], sort=FALSE)$DMCont
  RDY <- (RFY * DC)/100
  return(RDY)
}



require(lpSolve)
nutrient_uptake <- function(S1=NA, S2=NA, d1=NA, a1=NA, d2=NA, a2=NA, r1=NA, r2=NA) {	
  # N, P and K uptakes based on QUEFTS
  if (S1 < r1 + ((S2 - r2) * a2 / d1)) {
    uptakeX_givenY = S1
  } else if (S1 > r1 + ((S2 - r2) * (2 * d2 / a1 - a2 / d1))) {
    uptakeX_givenY = r1 + (S2 - r2) * (d2 / a1)
  } else {
    uptakeX_givenY = S1 - 0.25 * (S1 - r1 - (S2 - r2) * (a2 / d1))^2 / ((S2 - r2) * (d2 / a1 - a2 / d1))
  }
  # Nutrient uptake given availability of other nutrient
  return(uptakeX_givenY)
}

yield_nutrients_combined <- function(U1=NA, d1=NA, a1=NA, Y2A=NA, Y2D=NA, Y3D=NA, r1=NA){	
  # Determine which nutrient limited to force yield being the lowest.
  YxD = min(Y2D, Y3D)	
  # If the uptake of one of the nutrients, and therefore the yield associated with that 
  # nutrient, is zero the overall yield is also zero.
  if (U1 == 0 || YxD == 0) {
    Y12 = 0
  }else{
    Y12 = Y2A + (2 * (YxD - Y2A) * (U1 - r1 - Y2A / d1)) / (YxD / a1 - Y2A / d1) -
      (YxD - Y2A) * (U1 - r1 - Y2A / d1)^2 / (YxD / a1 - Y2A / d1)^2
  }
  # Return the calculated yield based on the uptake of nutrients 1 and 2
  return(Y12)
}

water_dependent_nutrient_uptake <- function(S1=NA, WLY=NA, d1=NA, a1=NA, r1=NA) {	
  if (S1 < r1 + WLY / d1) {
    uptakeX_givenWater = S1    
  } else if (S1 > r1 + 2*WLY/a1 - WLY/d1) {
    uptakeX_givenWater = WLY / a1    
  } else {
    uptakeX_givenWater = S1 - 0.25 * (S1 - r1 - WLY/d1)^2 / (WLY / a1 - WLY / d1)    
  }
  
  return(uptakeX_givenWater)
}


NUE <- function(HI, CmaxNroots=6.6, CminNroots=2.5, CmaxNtops=17.9, CminNtops=7.9, CmaxProots=1.5, CminProots=0.8, CmaxPtops=2.8, CminPtops=0.9, 
                CmaxKroots=11, CminKroots=2.8, CmaxKtops=18.8, CminKtops=3.4 ){
  aN = round(1000 * HI/(HI * CmaxNroots + (1 - HI) * CmaxNtops), digits=0)
  dN = round(1000 * HI/(HI * CminNroots + (1 - HI) * CminNtops), digits=0)
  
  aP = round(1000 * HI/(HI * CmaxProots + (1 - HI) * CmaxPtops), digits=0)
  dP = round(1000 * HI/(HI * CminProots + (1 - HI) * CminPtops), digits=0)
  
  aK = round(1000 * HI/(HI * CmaxKroots + (1 - HI) * CmaxKtops), digits=0)
  dK = round(1000 * HI/(HI * CminKroots + (1 - HI) * CminKtops), digits=0)
  
  return(data.frame(aN=aN, dN=dN,aP=aP,dP=dP,aK=aK,dK=dK))
  
}

NUE(HI=0.5)


## Maize: aN=29	; dN=74	; aP=95	; dP=476; aK=38	; dK=143	; rN=5	; rP=0.4	, rK=2
## Cassava: aN=41, dN=96, rN=0, aP=233, dP=588, rP=0, aK=34, dK=161, rK=0)
##Yield_S <- function(SN, SP, SK, WLY, aN=41, dN=96, rN=0, aP=233, dP=588, rP=0, aK=34, dK=161, rK=0){ ## cassava
Yield_S <- function(SN, SP, SK, WLY, aN=41, dN=96, rN=0, aP=233, dP=588, rP=0, aK=34, dK=161, rK=0){
  ## uptake of one nutrient conditioned by availablility of one other nutrient or water, rule of minimum
  UNP <- nutrient_uptake(S1 = SN, S2 = SP, d1 = dN, a1 = aN, d2 = dP, a2 = aP, r1 = rN, r2 = rP)
  UNK <- nutrient_uptake(S1 = SN, S2 = SK, d1 = dN, a1 = aN, d2 = dK, a2 = aK, r1 = rN, r2 = rK)
  UNW <- water_dependent_nutrient_uptake(S1 = SN, WLY = WLY, d1 = dN, a1 = aN, r1 = rN)
  UN <- min(UNP, UNK, UNW)
  
  UPN <- nutrient_uptake(S1 = SP, S2 = SN, d1 = dP, a1 = aP, d2 = dN, a2 = aN, r1 = rP, r2 = rN)
  UPK <- nutrient_uptake(S1 = SP, S2 = SK, d1 = dP, a1 = aP, d2 = dK, a2 = aK, r1 = rP, r2 = rK)
  UPW <- water_dependent_nutrient_uptake(S1 = SP, WLY = WLY, d1 = dP, a1 = aP, r1 = rP)
  UP <- min(UPN, UPK, UPW)				
  
  UKN <- nutrient_uptake(S1 = SK, S2 = SN, d1 = dK, a1 = aK, d2 = dN, a2 = aN, r1 = rK, r2 = rN)
  UKP <- nutrient_uptake(S1 = SK, S2 = SP, d1 = dK, a1 = aK, d2 = dP, a2 = aP, r1 = rK, r2 = rP)
  UKW <- water_dependent_nutrient_uptake(S1 = SK, WLY = WLY, d1 = dK, a1 = aK, r1 = rK)
  UK <- min(UKN, UKP, UKW)		
  
  ## yield base don uptake of Nitrogen at dilution or accumulation
  YNA <- max((UN - rN), 0) * aN
  YND <- max((UN - rN), 0) * dN
  YPA <- max((UP - rP), 0) * aP
  YPD <- max((UP - rP), 0) * dP
  YKA <- max((UK - rK), 0) * aK
  YKD <- max((UK - rK), 0) * dK	
  
  ## yield as defined by uptake of one nutrient conditioned to max and min yield based on secnd nutrient not to be constrined by a third nutrient
  YNP <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YPA, Y2D = YPD, Y3D = YKD, r1 = rN)
  YNK <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YKA, Y2D = YKD, Y3D = YPD, r1 = rN)
  YPN <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YNA, Y2D = YND, Y3D = YKD, r1 = rP)
  YPK <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YKA, Y2D = YKD, Y3D = YND, r1 = rP)
  YKN <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YNA, Y2D = YND, Y3D = YPD, r1 = rK)
  YKP <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YPA, Y2D = YPD, Y3D = YND, r1 = rK)
  
  # Make sure the nutrient limited yields do not exceed the maximum possible yield = WLY
  YNPc <- min(c(YNP, YND, YPD, YKD, WLY))
  YNKc <- min(c(YNK, YND, YPD, YKD, WLY))
  YPNc <- min(c(YPN, YND, YPD, YKD, WLY))
  YPKc <- min(c(YPK, YND, YPD, YKD, WLY))
  YKNc <- min(c(YKN, YND, YPD, YKD, WLY))
  YKPc <- min(c(YKP, YND, YPD, YKD, WLY))
  
  #Final estimate
  YEc <- mean(c(YNPc, YNKc, YPNc, YPKc, YKNc, YKPc))
  return(YEc)				
}




## get yield estimates using QUEFTS and giving in SN, SP, SK and WLY.  get TSS for the estimates of yield and NOT blup. 
## SN, SP and SK are supposed to be the sum of what the soil originally has and added fertilizer. 
## added fertilizer is provided as it is defined in NOT trials and INS is the one to be defined by iterating on while controling the output
## by the squared yield difference between simulated and measured from NOT  (Blups of NOT)
optim_INS <- function(INS, addedFertilizer, YieldMatrix, WLY){	
  
  SoilN <- INS[1]
  SoilP <- INS[2]
  SoilK <- INS[3]
  
  #Yield_Control <- Yield_S(SN = addedFertilizer[1,1] + SoilN , SP = addedFertilizer[1,2] + SoilP, SK = addedFertilizer[1,3] + SoilK, WLY = WLY)
  Yield_pk <- Yield_S(SN = (addedFertilizer[2,1]*RecoveryFraction$rec_N) + SoilN , 
                      SP = (addedFertilizer[2,2]*RecoveryFraction$rec_P) + SoilP, 
                      SK = (addedFertilizer[2,3]*RecoveryFraction$rec_K) + SoilK, WLY = WLY)
  
  Yield_nk <- Yield_S(SN = (addedFertilizer[3,1]*RecoveryFraction$rec_N) + SoilN ,
                      SP = (addedFertilizer[3,2]*RecoveryFraction$rec_P) + SoilP, 
                      SK = (addedFertilizer[3,3]*RecoveryFraction$rec_K) + SoilK, WLY = WLY)
  
  Yield_np <- Yield_S(SN = (addedFertilizer[4,1]*RecoveryFraction$rec_N) + SoilN , 
                      SP = (addedFertilizer[4,2]*RecoveryFraction$rec_P) + SoilP, 
                      SK = (addedFertilizer[4,3]*RecoveryFraction$rec_K) + SoilK, WLY = WLY)
  #Yield_halfnpk <- Yield_S(SN = addedFertilizer[5,1] + SoilN , SP = addedFertilizer[5,2] + SoilP, SK = addedFertilizer[5,3] + SoilK, WLY = WLY)
  
  
  #control_SS <- (Yield_Control - YieldMatrix$NOT_yield[1])^2
  pk_SS <- (Yield_pk - YieldMatrix$NOT_yield[2])^2
  nk_SS <- (Yield_nk - YieldMatrix$NOT_yield[3])^2
  np_SS <- (Yield_np - YieldMatrix$NOT_yield[4])^2
  #halfnpk_SS <- (Yield_halfnpk - YieldMatrix$NOT_yield[5])^2
  
  TSS <- sum( pk_SS, nk_SS, np_SS)	
  return(TSS)	
}


#######################################################################
## FR & SP
#######################################################################
#'  @param fertilizers: data frame with type, N_cont, P_cont, K_cont, price. Price is per kg of fertilizer
#'  @param NG_CY_Fertdata: data frame with lat,long,fert_N,fert_P,fert_K, water_limited_yield, CurrentYield,location, pl_Date,zone, harvestDay, harvestmonth, daysOnField
#'  @param SoilData: data frame with lat,long,soilN,soilP,soilK,rec_N, rec_P, rec_K, rel_N,rel_P,rel_K
#'  @param rootUP: a price of 1 tonne of cassava in freshwt. It is used as freshwt price, after QUEFTS give drywt root yield (kg/ha) it is converted to freshwt in tonne/ha and this price is then used
#'  @param areaHa is area of land in ha
#'  @param WLY_FertRecom is the file with WLY, CY and fert recom for WLY for all lcoations. WLY and CY are in DM kg/ha
#'  @return a data frame with NPK elemental amount required for target yield with total cost, net revenu,and list of fertilizers with the 
#' amount required in kg for the user defined land size

# PD = "2018-03-01"; HD = "2019-05-31"; lat = 10.024; lon = 4.025; country = "NG"; cassUW = 1000; cassUP = 12000; maxInv = 72000;
# SoilData = SoilGridData_NG , userName = "acai cassava", userPhoneCC = 254, userPhoneNr = 702974480,userEmail = "acai.akilimo@gmail.com",
# cassPD = "roots"
#' 
#' @param dss 
#' @returnType 
#' @return 
#' 
#' @author Meklit
#' @export
getsupply <- function(dss){
  supply <- data.frame(lat=dss$lat, long=dss$long, rel_N=dss$rel_N, rel_P=dss$rel_P, rel_K=dss$rel_K, SN=dss$soilN, SP=dss$soilP, SK=dss$soilK, water_limited_yield = dss$water_limited_yield,
                       aN=dss$aN, dN=dss$dN, aP=dss$aP, dP=dss$dP, aK=dss$aK, dK=dss$dK, rN=dss$rN, rP=dss$rP, rK=dss$rK, max_yield=dss$max_yield,  tolerance=dss$tolerance,
                       WLY = dss$water_limited_yield)
}


#' The soil NPK as obtained from randdom forest model
#' @param zone, is used to define the varieties and HI to get NUE. Lake zone (the default) is proxy for Mkobozi and E & S zone is Kiroba  
#' @param Queft_Input_Data: per lat and long, crop param and soil param, water limited yield, fertlizer recommendation
#' @return 
#' 
#' @author Meklit
#' @export
QUEFTS_WLY_CY <- function(SoilData=SoilData, country=country, wlyd=wlyd){	
  #wly_plDate <- wly_data[wly_data$plantingDate == pl_Date, c("lat", "long", "wly_KgHa")]
  wly_plDate <- wlyd[,  c("lat", "long", "water_limited_yield")]
  
  # colnames(wly_plDate) <- c("lat", "long", "water_limited_yield")	
  Quefts_Input_Data_wly <- merge(SoilData, wly_plDate, by=c("lat", "long"))
  
  ## HI: Median for Nigeria=0.55 and Tanzania=0.52. Q3, Nigeria=0.63 and Tanzania=0.61
  if(country == "NG"){
    crop_param <- cbind(NUE(HI=0.55), data.frame(rN=0, rP=0, rK=0, max_yield=Quefts_Input_Data_wly$water_limited_yield, tolerance=0.01))
  }else{
    crop_param <- cbind(NUE(HI=0.55), data.frame(rN=0, rP=0, rK=0, max_yield=Quefts_Input_Data_wly$water_limited_yield, tolerance=0.01))
  }
  
  ## 1. get soil nutrient supply
  Queft_Input_Data_Var <- cbind(Quefts_Input_Data_wly, crop_param)	
  supply <- getsupply(Queft_Input_Data_Var) ## to get yield at zero input level
  
  
  ## 2. Current yield: 
  actualUptake <- merge(supply,ddply(supply,.(lat, long), actual_uptake_tool), by=c("lat","long"))
  minmax_Yield <-  merge(actualUptake, ddply(actualUptake,.(lat, long), max_min_yields_tools), by=c("lat","long"))
  Current_Yield <- ddply(minmax_Yield,.(lat, long), final_yield_tools)## yield at zero input
  colnames(Current_Yield) <- c("lat", "long", "CurrentYield")
  Yield_Fertilizer <- merge(wly_plDate, Current_Yield, by=c("lat", "long"))
  return(Yield_Fertilizer$CurrentYield)
}



QUEFTS_WLYCY <- function(soilN, soilP, soilK, WLY){	
  FCY_N = data.frame(soilN=soilN, soilP = soilP, soilK=soilK)
  FCY_N$lat <- -6
  FCY_N$long <- 39
  FCY_N$rec_N <- 0.5
  FCY_N$rec_P <- 0.15
  FCY_N$rec_K <- 0.5
  FCY_N$rel_N <- 1
  FCY_N$rel_P <- FCY_N$soilP / FCY_N$soilN
  FCY_N$rel_K <- FCY_N$soilK / FCY_N$soilN  
  FCY_N$water_limited_yield <- WLY
  Quefts_Input_Data_wly <- FCY_N
  
  crop_param <- data.frame(aN=29, dN=74, aP=95, dP=476,  aK=38, dK=143,rN=5,rP=0.4, rK=2, max_yield = WLY, tolerance = 0.01 )
  
  ## 1. get soil nutrient supply
  Queft_Input_Data_Var <- cbind(Quefts_Input_Data_wly, crop_param)	
  supply <- getsupply(Queft_Input_Data_Var) ## to get yield at zero input level
  
  
  ## 2. Current yield: 
  actualUptake <- merge(supply,ddply(supply,.(lat, long), actual_uptake_tool), by=c("lat","long"))
  minmax_Yield <-  merge(actualUptake, ddply(actualUptake,.(lat, long), max_min_yields_tools), by=c("lat","long"))
  getcy <- ddply(minmax_Yield,.(lat, long), final_yield_tools)## yield at zero input
  colnames(getcy) <- c("lat", "long", "CurrentYield")
  FCY_N$Current_Yield <- getcy$CurrentYield
  return(FCY_N[, c("soilN","soilP","soilK", "water_limited_yield", "Current_Yield")])
}




getFRrecommendations <- function(lat, lon, PD, maxInv, fertilizers, wlyd , SoilData, rootUP, areaHa=1, country){
  
  require(plyr)
  
  WLYData <- unique(wlyd[wlyd$pl_Date == PD, ])
  if(WLYData$harvestDay  > 365){
    HD <- WLYData$harvestDay  - 365
  }else{
    HD <- WLYData$harvestDay 
  }
  ## 1. get WLY, CY, fert recom and soil data
  water_limited_yield <- WLYData$water_limited_yield ## DM in kg/ha
  DCY <- WLYData$Current_Yield## DM in kg/ha
  
  ## 2. change investment from given areaHa to 1ha
  InvestHa <- (maxInv / areaHa)	
  
  ## 3. optimize the fertilizer recommendation for maxInv in local currency and provide expected target yield in kg
  fert_optim <- run_Optim_NG2(rootUP=rootUP, QID=SoilData, fertilizer=fertilizers, invest=InvestHa, plDate=WLYData$pl_Date, 
                              WLYData=WLYData, lat=lat, lon=lon, areaHa, HD=HD, DCY = DCY, WLY=water_limited_yield)
  fert_optim$harvestDate <- HD
  
  if(fert_optim$NR <= 0){ ## all fertilizer recom == 0
    fert_optim$urea <- 0
    fert_optim$DAP <- 0
    fert_optim$YaraMila_UNIK <- 0
    fert_optim$NR <- 0
    fert_optim$TC <- 0
    fert_optim$TargetY <- fert_optim$CurrentY
    fert_optim$N <- fert_optim$P <- fert_optim$K <- 0
    return(fert_optim)
  }else{
    ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
    onlyFert <- subset(fert_optim, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR,harvestDate))
    fertinfo <- subset(fert_optim, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate))
    RecomperHa <- onlyFert/areaHa
    RecomperHa2 <- gather(RecomperHa, type, amount)
    onlyFert2 <- droplevels(RecomperHa2[RecomperHa2$amount > 25, ])
    if(nrow(onlyFert2) == 0 ){ ## if all fertilizer recom < 25 kg/ha all will be set to 0
      fert_optim$urea <- fert_optim$DAP <- fert_optim$YaraMila_UNIK <- fert_optim$NR <- fert_optim$TC <- fert_optim$N <- fert_optim$P <- fert_optim$K <- 0
      fert_optim$TargetY <- fert_optim$CurrentY
      return(fert_optim)
    }else if (ncol(onlyFert) == nrow(onlyFert2)){ ## if all fertilizer recom are >= 25 kg/ha they will be kept and only checked for NR >= 18% of invest
      Reset_fert_Cont <- fert_optim
      GPS_fertRecom <- NRabove18Cost(ds=Reset_fert_Cont)
      if(!'urea' %in% names(GPS_fertRecom)){GPS_fertRecom$urea <- 0}
      if(!'DAP' %in% names(GPS_fertRecom)){GPS_fertRecom$DAP <- 0}
      if(!'YaraMila_UNIK' %in% names(GPS_fertRecom)){GPS_fertRecom$YaraMila_UNIK <- 0}
      GPS_fertRecom <- GPS_fertRecom[, c(names(fertinfo), c("urea", "DAP", "YaraMila_UNIK"))]
      return(GPS_fertRecom)
    }else{
      fert25 <- spread(onlyFert2, type, amount) ## when some fertilizer recom are dropped b/c < 25 kg/ha, ty and NR should be recalculated
      fert_optim2 <- cbind(fertinfo, fert25)	
      fertilizer <- fertilizers[fertilizers$type %in% onlyFert2$type, ]
      Reset_fert_Cont <- Rerun_25kgKa_try(rootUP=rootUP, rdd=fert_optim2, fertilizer=fertilizer, QID=SoilData, onlyFert=onlyFert2,
                                          country = country, WLY=water_limited_yield, DCY = DCY)
      if(Reset_fert_Cont$NR <= 0){ ## after rerunning after avoiding <25KG/ha fertilizers, if NR <=0
        Reset_fert_Cont <- fertinfo
        Reset_fert_Cont$N <- Reset_fert_Cont$P <- Reset_fert_Cont$K <- Reset_fert_Cont$TC <- Reset_fert_Cont$NR <- 0
        Reset_fert_Cont$TargetY <- Reset_fert_Cont$CurrentY
        Reset_fert_Cont$urea <- Reset_fert_Cont$DAP <- Reset_fert_Cont$YaraMila_UNIK <- 0
        return(Reset_fert_Cont)
      }else{
        GPS_fertRecom <- NRabove18Cost(ds=Reset_fert_Cont)
        if(!'urea' %in% names(GPS_fertRecom)){GPS_fertRecom$urea <- 0}
        if(!'DAP' %in% names(GPS_fertRecom)){GPS_fertRecom$DAP <- 0}
        if(!'YaraMila_UNIK' %in% names(GPS_fertRecom)){GPS_fertRecom$YaraMila_UNIK <- 0}
        GPS_fertRecom <- GPS_fertRecom[, c(names(fertinfo), c("urea",  "DAP", "YaraMila_UNIK"))] ## for Tanzania
        return(GPS_fertRecom)
      }
    }
  }
  
}


Fix_NR_TY <- function(rootUP, rdd, QID, country, fertilizers, wlyD){
  QID$water_limited_yield <- wlyD
  N <- rdd$N
  P  <- rdd$P
  K  <- rdd$K
  rec <- c(N, P, K)
  fertilizers$amount <- c(rdd$urea, rdd$DAP, rdd$YaraMila_UNIK)
  #TC <- (sum(fertilizers$price %*% fertilizers$amount) )
  HD <- rdd$harvestDate
  TY  <- QUEFTS1_Pedotransfer(QID, rec)					#dry wt yield in kg/ha
  rdd$TargetY <- ((getRFY(HD = HD, RDY = TY, country = country))/1000)
  rdd$NR <- ((rdd$TargetY - rdd$CurrentY)*rootUP) - rdd$TC
  
  if(rdd$TargetY <= rdd$CurrentY | rdd$NR <= 0 ){
    rdd$N <- rdd$P <- rdd$K <- rdd$TC <- rdd$NR <- rdd$urea <- rdd$DAP <- rdd$YaraMila_UNIK <- 0
    rdd$TargetY <- rdd$CurrentY
  }
  
  rdd <- NRabove18Cost(ds=rdd)
  return(rdd)
}

keepRows <- function(fertilizers, onlyFert){
  if(any(!fertilizers$type %in% names(onlyFert))){
    nn <- droplevels(fertilizers$type[!fertilizers$type %in% names(onlyFert)])
    tt <- as.data.frame(matrix(ncol=length(nn), nrow=1))
    colnames(tt) <- nn
    tt[,1:ncol(tt)] <- 0
    onlyFert <- cbind(onlyFert, tt)
  }
  onlyFert <- onlyFert[, fertilizers$type]
  return(onlyFert)
}



getFRrecommendations_NO <- function(lat, lon, PD, maxInv, fertilizers, wlyd , SoilData, rootUP, areaHa=1, country){
  require(plyr)
  
  WLYData <- unique(wlyd[wlyd$pl_Date == PD, ])
  HD <- WLYData$HD
  water_limited_yield <- WLYData$water_limited_yield ## DM in kg/ha
  DCY <- WLYData$Current_Yield## DM in kg/ha
  
  ## 2. change investment from given areaHa to 1ha
  InvestHa <- (maxInv / areaHa)	
  
  ## 3. optimize the fertilizer recommendation for maxInv in local currency and provide expected target yield in kg
  fert_optim <- run_Optim_NG2(rootUP=rootUP, QID=SoilData, fertilizer=fertilizers, invest=InvestHa, plDate=WLYData$pl_Date, 
                              WLYData=WLYData, lat=lat, lon=lon, areaHa, HD=HD, DCY = DCY, WLY=water_limited_yield)
  fert_optim$harvestDate <- HD
  
  
  
  onlyFert <- subset(fert_optim, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR,harvestDate))
  fertinfo <- subset(fert_optim, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate))
  
  
  if(fertinfo$NR <= 0){ ## all fertilizer recom == 0
    fertinfo$NR <- 0
    fertinfo$TC <- 0
    fertinfo$TargetY <- fertinfo$CurrentY
    fertinfo$N <- fertinfo$P <- fertinfo$K <- 0
    onlyFert[,1:ncol(onlyFert)] <- 0
    onlyFertQ <- keepRows(fertilizers, onlyFert)
    return(cbind(fertinfo, onlyFertQ))
  }else{
    ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
    RecomperHa <- onlyFert/areaHa
    RecomperHa2 <- gather(RecomperHa, type, amount)
    onlyFert2 <- droplevels(RecomperHa2[RecomperHa2$amount > 25, ])
    if(nrow(onlyFert2) == 0 ){ ## if all fertilizer recom < 25 kg/ha all will be set to 0
      fertinfo$NR <- 0
      fertinfo$TC <- 0
      fertinfo$TargetY <- fertinfo$CurrentY
      fertinfo$N <- fertinfo$P <- fertinfo$K <- 0
      onlyFert[,1:ncol(onlyFert)] <- 0
      return(cbind(fertinfo, onlyFert))
    }else if (ncol(onlyFert) == nrow(onlyFert2)){ ## if all fertilizer recom are >= 25 kg/ha they will be kept and only checked for NR >= 18% of invest
      onlyFertQ <- keepRows(fertilizers, onlyFert)
      Reset_fert_Cont <-  cbind(fertinfo, onlyFertQ)
      GPS_fertRecom <- NRabove18Cost_FertTest(ds=Reset_fert_Cont)
      return(GPS_fertRecom)
    }else{
      fert25 <- spread(onlyFert2, type, amount) ## when some fertilizer recom are dropped b/c < 25 kg/ha, ty and NR should be recalculated
      fert_optim2 <- cbind(fertinfo, fert25)	
      fertilizer <- fertilizers[fertilizers$type %in% onlyFert2$type, ]
      Reset_fert_Cont <- Rerun_25kgKa_try(rootUP=rootUP, rdd=fert_optim2, fertilizer=fertilizer, QID=SoilData, onlyFert=onlyFert2,
                                          country = country, WLY=water_limited_yield, DCY = DCY)
      if(Reset_fert_Cont$NR <= 0){ ## after rerunning after avoiding <25KG/ha fertilizers, if NR <=0
        fertinfo$NR <- 0
        fertinfo$TC <- 0
        fertinfo$TargetY <- fertinfo$CurrentY
        fertinfo$N <- fertinfo$P <- fertinfo$K <- 0
        onlyFert[,1:ncol(onlyFert)] <- 0
        onlyFertQ <- keepRows(fertilizers, onlyFert)
        return(cbind(fertinfo, onlyFertQ))
      }else{
        GPS_fertRecom <- NRabove18Cost_FertTest(ds=Reset_fert_Cont)
        onlyFert <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR,harvestDate))
        fertinfo <- subset(GPS_fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate))
        onlyFertQ <- keepRows(fertilizers, onlyFert)
        return(cbind(fertinfo, onlyFertQ))
      }
    }
  }
}


getFRrecommendations_NG <- function(lat, lon, PD, maxInv, fertilizers, wlyd , SoilData, rootUP, areaHa=1, country){
  
  require(plyr)
  
  WLYData <- unique(wlyd[wlyd$pl_Date == PD, ])
  HD <- WLYData$HD
  # if(WLYData$harvestDay  > 365){
  #   HD <- WLYData$harvestDay  - 365
  # }else{
  #   HD <- WLYData$harvestDay 
  # }
  ## 1. get WLY, CY, fert recom and soil data
  water_limited_yield <- WLYData$water_limited_yield ## DM in kg/ha
  DCY <- WLYData$Current_Yield## DM in kg/ha
  
  # water_limited_yield <- getRDY(HD = HD, RFY =WLYData$water_limited_yield, country=country)## DM in kg/ha
  # DCY <- getRDY(HD = HD, RFY = FCY, country=country)*1000 ## CY in kg/ha DM
  
  
  ## 2. change investment from given areaHa to 1ha
  InvestHa <- (maxInv / areaHa)	
  
  ## 3. optimize the fertilizer recommendation for maxInv in local currency and provide expected target yield in kg
  fert_optim <- run_Optim_NG2(rootUP=rootUP, QID=SoilData, fertilizer=fertilizers, invest=InvestHa, plDate=WLYData$pl_Date, 
                              WLYData=WLYData, lat=lat, lon=lon, areaHa, HD=HD, DCY = DCY, WLY=water_limited_yield)
  fert_optim$harvestDate <- HD
  
  if(fert_optim$NR <= 0){ ## all fertilizer recom == 0
    fert_optim$urea <- 0
    # fert_optim$DAP <- 0
    fert_optim$NPK15_15_15 <- 0 
    fert_optim$NR <- 0
    fert_optim$TC <- 0
    fert_optim$TargetY <- fert_optim$CurrentY
    fert_optim$N <- fert_optim$P <- fert_optim$K <- 0
    fert_optim <- fert_optim[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea", "NPK15_15_15", "harvestDate")]
    return(fert_optim)
  }else{
    ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
    onlyFert <- subset(fert_optim, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR,harvestDate))
    fertinfo <- subset(fert_optim, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate))
    RecomperHa <- onlyFert/areaHa
    RecomperHa2 <- gather(RecomperHa, type, amount)
    onlyFert2 <- droplevels(RecomperHa2[RecomperHa2$amount > 25, ])
    if(nrow(onlyFert2) == 0 ){ ## if all fertilizer recom < 25 kg/ha all will be set to 0
      fert_optim$urea <- 0
      # fert_optim$DAP <- 0
      fert_optim$NPK15_15_15 <- 0
      fert_optim$NR <- 0
      fert_optim$TC <- 0
      fert_optim$N <- fert_optim$P <- fert_optim$K <- 0
      fert_optim$TargetY <- fert_optim$CurrentY
      fert_optim <- fert_optim[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea", "NPK15_15_15", "harvestDate")]
      return(fert_optim)
    }else if (ncol(onlyFert) == nrow(onlyFert2)){ ## if all fertilizer recom are >= 25 kg/ha they will be kept and only checked for NR >= 18% of invest
      Reset_fert_Cont <- fert_optim
      GPS_fertRecom <- NRabove18Cost_NG(ds=Reset_fert_Cont)
      if(!'urea' %in% names(GPS_fertRecom)){GPS_fertRecom$urea <- 0}
      # if(!'DAP' %in% names(GPS_fertRecom)){GPS_fertRecom$DAP <- 0}
      if(!'NPK15_15_15' %in% names(GPS_fertRecom)){GPS_fertRecom$NPK15_15_15 <- 0}
      GPS_fertRecom <- GPS_fertRecom[, c(names(fertinfo), c("urea", "NPK15_15_15"))]
      GPS_fertRecom <- GPS_fertRecom[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea", "NPK15_15_15", "harvestDate")]
      return(GPS_fertRecom)
    }else{
      fert25 <- spread(onlyFert2, type, amount) ## when some fertilizer recom are dropped b/c < 25 kg/ha, ty and NR should be recalculated
      fert_optim2 <- cbind(fertinfo, fert25)	
      fertilizer <- fertilizers[fertilizers$type %in% onlyFert2$type, ]
      Reset_fert_Cont <- Rerun_25kgKa_try(rootUP=rootUP, rdd=fert_optim2, fertilizer=fertilizer, QID=SoilData, onlyFert=onlyFert2,
                                          country = country, WLY=water_limited_yield, DCY = DCY)
      if(Reset_fert_Cont$NR <= 0){ ## after rerunning after avoiding <25KG/ha fertilizers, if NR <=0
        Reset_fert_Cont <- fertinfo
        Reset_fert_Cont$N <- Reset_fert_Cont$P <- Reset_fert_Cont$K <- Reset_fert_Cont$TC <- Reset_fert_Cont$NR <- 0
        Reset_fert_Cont$TargetY <- Reset_fert_Cont$CurrentY
        Reset_fert_Cont$NR <- 0
        Reset_fert_Cont$urea <- 0
        #Reset_fert_Cont$DAP <- 0
        Reset_fert_Cont$NPK15_15_15 <- 0
        Reset_fert_Cont <- Reset_fert_Cont[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea", "NPK15_15_15", "harvestDate")]
        return(Reset_fert_Cont)
      }else{
        GPS_fertRecom <- NRabove18Cost_NG(ds=Reset_fert_Cont)
        if(!'urea' %in% names(GPS_fertRecom)){GPS_fertRecom$urea <- 0}
        if(!'NPK15_15_15' %in% names(GPS_fertRecom)){GPS_fertRecom$NPK15_15_15 <- 0} ## for Nigeria
        # if(!'DAP' %in% names(GPS_fertRecom)){GPS_fertRecom$DAP <- 0}
        GPS_fertRecom <- GPS_fertRecom[, c(names(fertinfo), c("urea", "NPK15_15_15"))] ## for Nigeria
        GPS_fertRecom <- GPS_fertRecom[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea", "NPK15_15_15", "harvestDate")]
        return(GPS_fertRecom)
      }
    }
  }
  
}


getFRrecommendations_TZ <- function(lat, lon, PD, maxInv, fertilizers, wlyd , SoilData, rootUP, areaHa=1, country){
  
  require(plyr)
  
  WLYData <- unique(wlyd[wlyd$pl_Date == PD, ])
  HD <- WLYData$HD
  
  ## 1. get WLY, CY, fert recom and soil data
  water_limited_yield <- WLYData$water_limited_yield ## DM in kg/ha
  DCY <- WLYData$Current_Yield## DM in kg/ha
  
  
  ## 2. change investment from given areaHa to 1ha
  InvestHa <- (maxInv / areaHa)	
  
  ## 3. optimize the fertilizer recommendation for maxInv in local currency and provide expected target yield in kg
  fert_optim <- run_Optim_NG2(rootUP=rootUP, QID=SoilData, fertilizer=fertilizers, invest=InvestHa, plDate=WLYData$pl_Date, 
                              WLYData=WLYData, lat=lat, lon=lon, areaHa, HD=HD, DCY = DCY, WLY=water_limited_yield)
  fert_optim$harvestDate <- HD
  
  if(fert_optim$NR <= 0){ ## all fertilizer recom == 0
    fert_optim$urea <- 0
    fert_optim$DAP <- 0
    fert_optim$NPK17_17_17 <- 0 
    fert_optim$NR <- 0
    fert_optim$TC <- 0
    fert_optim$TargetY <- fert_optim$CurrentY
    fert_optim$N <- fert_optim$P <- fert_optim$K <- 0
    fert_optim <- fert_optim[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea","DAP" ,"NPK17_17_17", "harvestDate")]
    return(fert_optim)
  }else{
    ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
    onlyFert <- subset(fert_optim, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR,harvestDate))
    fertinfo <- subset(fert_optim, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate))
    RecomperHa <- onlyFert/areaHa
    RecomperHa2 <- gather(RecomperHa, type, amount)
    onlyFert2 <- droplevels(RecomperHa2[RecomperHa2$amount > 25, ])
    if(nrow(onlyFert2) == 0 ){ ## if all fertilizer recom < 25 kg/ha all will be set to 0
      fert_optim$urea <- 0
      fert_optim$DAP <- 0
      fert_optim$NPK17_17_17 <- 0
      fert_optim$NR <- 0
      fert_optim$TC <- 0
      fert_optim$N <- fert_optim$P <- fert_optim$K <- 0
      fert_optim$TargetY <- fert_optim$CurrentY
      fert_optim <- fert_optim[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea","DAP", "NPK17_17_17", "harvestDate")]
      return(fert_optim)
    }else if (ncol(onlyFert) == nrow(onlyFert2)){ ## if all fertilizer recom are >= 25 kg/ha they will be kept and only checked for NR >= 18% of invest
      Reset_fert_Cont <- fert_optim
      GPS_fertRecom <- NRabove18Cost_TZ(ds=Reset_fert_Cont)
      if(!'urea' %in% names(GPS_fertRecom)){GPS_fertRecom$urea <- 0}
      if(!'DAP' %in% names(GPS_fertRecom)){GPS_fertRecom$DAP <- 0}
      if(!'NPK17_17_17' %in% names(GPS_fertRecom)){GPS_fertRecom$NPK17_17_17 <- 0}
      GPS_fertRecom <- GPS_fertRecom[, c(names(fertinfo), c("urea","DAP", "NPK17_17_17"))]
      GPS_fertRecom <- GPS_fertRecom[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea","DAP", "NPK17_17_17", "harvestDate")]
      return(GPS_fertRecom)
    }else{
      fert25 <- spread(onlyFert2, type, amount) ## when some fertilizer recom are dropped b/c < 25 kg/ha, ty and NR should be recalculated
      fert_optim2 <- cbind(fertinfo, fert25)	
      fertilizer <- fertilizers[fertilizers$type %in% onlyFert2$type, ]
      Reset_fert_Cont <- Rerun_25kgKa_try(rootUP=rootUP, rdd=fert_optim2, fertilizer=fertilizer, QID=SoilData, onlyFert=onlyFert2,
                                          country = country, WLY=water_limited_yield, DCY = DCY)
      if(Reset_fert_Cont$NR <= 0){ ## after rerunning after avoiding <25KG/ha fertilizers, if NR <=0
        Reset_fert_Cont <- fertinfo
        Reset_fert_Cont$N <- Reset_fert_Cont$P <- Reset_fert_Cont$K <- Reset_fert_Cont$TC <- Reset_fert_Cont$NR <- 0
        Reset_fert_Cont$TargetY <- Reset_fert_Cont$CurrentY
        Reset_fert_Cont$NR <- 0
        Reset_fert_Cont$urea <- 0
        Reset_fert_Cont$DAP <- 0
        Reset_fert_Cont$NPK17_17_17 <- 0
        Reset_fert_Cont <- Reset_fert_Cont[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea","DAP", "NPK17_17_17", "harvestDate")]
        return(Reset_fert_Cont)
      }else{
        GPS_fertRecom <- NRabove18Cost_TZ(ds=Reset_fert_Cont)
        if(!'urea' %in% names(GPS_fertRecom)){GPS_fertRecom$urea <- 0}
        if(!'NPK17_17_17' %in% names(GPS_fertRecom)){GPS_fertRecom$NPK17_17_17 <- 0} ## for Nigeria
        if(!'DAP' %in% names(GPS_fertRecom)){GPS_fertRecom$DAP <- 0}
        GPS_fertRecom <- GPS_fertRecom[, c(names(fertinfo), c("urea","DAP", "NPK17_17_17"))] ## for Nigeria
        GPS_fertRecom <- GPS_fertRecom[, c("lat","lon", "plDate", "N", "P", "K","WLY", "CurrentY","TargetY", "TC", "NR", "urea","DAP", "NPK17_17_17", "harvestDate")]
        return(GPS_fertRecom)
      }
    }
  }
}






# Urea: 0.42 USD/kg                    (7,500 NGN per 50 kg bag)
# DAP: 0.74 USD/kg                     (13,250 NGN per 50 kg bag)
# MOP: 0.75 USD/kg                    (13,500 USD per 50 kg bag)
# Root price: 33 USD / tonne        (12,000 NGN per tonne)
#' function to creat a data frame with fertilizers
#' @return: data frame with (type, N_cont, P_cont, K_cont, price) The NPK is elemental concentration and price is per kg of fertilizer
#' @example  
#TODO: price of fertilizers for tanzania is different so from GPS we need to define the zone and take the correct price accordingly. default is at the end of the script
fertilizerFunc <- function(ureaavailable=TRUE, ureaCostperBag=NA,ureaBagWt=50,
                           MOPavailable=TRUE, MOPCostperBag=NA, MOPBagWt=50,
                           DAPavailable=TRUE, DAPCostperBag=NA, DAPBagWt=50, 
                           NPK201010available=TRUE, NPK201010CostperBag=NA, NPK201010BagWt=50, 
                           NPK151515available=TRUE, NPK151515CostperBag=NA, NPK151515BagWt=50,
                           TSPavailable=TRUE, TSPCostperBag=NA, TSPBagWt=50, 
                           NPK171717available=TRUE, NPK171717CostperBag=NA, NPK171717BagWt=50, 
                           Nafakaavailable=TRUE, NafakaCostperBag=NA, NafakaBagWt=50,
                           CANavailable=TRUE, CANCostperBag=NA, CANBagWt=50, 
                           SSPavailable=TRUE, SSPCostperBag=NA, SSPBagWt=50,
                           newFert1name=NULL, newFert1N_cont=NA, newFert1P2O5=NA, 
                           newFert1K2O=NA, newFert1CostperBag=NA, newFert1BagWt=NA,
                           newFert2name=NA, newFert2N_cont=NA, newFert2P2O5=NA, 
                           newFert2K2O=NA, newFert2CostperBag=NA, newFert2BagWt=NA,
                           newFert3name=NA, newFert3N_cont=NA, newFert3P2O5=NA, 
                           newFert3K2O=NA, newFert3CostperBag=NA, newFert3BagWt=NA,
                           newFert4name=NA, newFert4N_cont=NA, newFert4P2O5=NA, 
                           newFert4K2O=NA, newFert4CostperBag=NA, newFert4BagWt=NA,
                           newFert5name=NA, newFert5N_cont=NA, newFert5P2O5=NA, 
                           newFert5K2O=NA, newFert5CostperBag=NA, newFert5BagWt=NA,country){
  
  if(country == "NG"){
    if(is.na(ureaCostperBag)){ureaCostperBag <- 7500}
    if(is.na(MOPCostperBag)) {MOPCostperBag <- 13500}
    if(is.na(DAPCostperBag))  {DAPCostperBag <- 13250}
    if(is.na(NPK201010CostperBag)) {NPK201010CostperBag <- 7200}
    if(is.na(NPK151515CostperBag)) {NPK151515CostperBag <- 6538}
    if(is.na(TSPCostperBag)) {TSPCostperBag <- 13250}
    if(is.na(NPK171717CostperBag)) {NPK171717CostperBag <- NA}						
    if(is.na(CANCostperBag)) {CANCostperBag <- 7500}
    if(is.na(SSPCostperBag)) {SSPCostperBag <- 22364}
  }else{							
    if(is.na(ureaCostperBag)){ureaCostperBag <- 56208}
    if(is.na(MOPCostperBag)) {MOPCostperBag <- 96608}
    if(is.na(DAPCostperBag))  {DAPCostperBag <- NA}
    if(is.na(NafakaCostperBag)) {NafakaCostperBag = 60072}
    if(is.na(NPK201010CostperBag)) {NPK201010CostperBag <- NA}
    if(is.na(NPK151515CostperBag)) {NPK151515CostperBag <- NA}
    if(is.na(TSPCostperBag)) {TSPCostperBag <- NA}
    if(is.na(NPK171717CostperBag)) {NPK171717CostperBag <- NA}						
    if(is.na(CANCostperBag)) {CANCostperBag <- NA}
    if(is.na(SSPCostperBag)) {SSPCostperBag <- NA}							
  }							
  
  
  ureaFert <- data.frame(type = 'urea', available =ureaavailable,  N_cont = 0.46, P_cont = 0, K_cont=0, costPerBag = ureaCostperBag, bagWeight=ureaBagWt ) 
  MOPFert <- data.frame(type = 'MOP', available = MOPavailable, N_cont = 0.00, P_cont = 0.00, K_cont=0.50, costPerBag = MOPCostperBag, bagWeight=MOPBagWt)
  DAPFert <- data.frame(type = 'DAP',available = DAPavailable,  N_cont = 0.18, P_cont = 0.20, K_cont=0.0, costPerBag = DAPCostperBag, bagWeight=DAPBagWt)
  NPK201010Fert <- data.frame(type = 'NPK20_10_10',available = NPK201010available,  N_cont = 0.20, P_cont = 0.044, K_cont=0.083, costPerBag = NPK201010CostperBag, bagWeight=NPK201010BagWt)
  NPK151515Fert <- data.frame(type = 'NPK15_15_15',available = NPK151515available,  N_cont = 0.15, P_cont = 0.07, K_cont=0.125, costPerBag = NPK151515CostperBag, bagWeight=NPK151515BagWt)
  TSPFert <- data.frame(type = 'TSP',available = TSPavailable,  N_cont = 0.0, P_cont = 0.2, K_cont=0.0, costPerBag = TSPCostperBag, bagWeight=TSPBagWt)
  NPK171717Fert <- data.frame(type = 'NPK17_17_17',available = NPK171717available,  N_cont = 0.17, P_cont = 0.083, K_cont=0.15, costPerBag = NPK171717CostperBag, bagWeight=NPK171717BagWt)# TODO get price
  Minjingu_Nafaka <- data.frame(type = 'Minjingu_Nafaka+',available = Nafakaavailable,  N_cont = 0.09, P_cont = 0.07, K_cont=0.06, costPerBag = NafakaCostperBag, bagWeight=NafakaBagWt)
  CANFert <- data.frame(type = 'CAN',available = CANavailable,  N_cont = 0.01, P_cont = 0.01, K_cont=0.01,costPerBag = CANCostperBag, bagWeight=CANBagWt)## not correct value TODO check
  SSPFert <- data.frame(type = 'SSP', available = SSPavailable, N_cont = 0.01, P_cont = 0.01, K_cont=0.01, costPerBag = SSPCostperBag, bagWeight=SSPBagWt)## not correct value TODO check
  
  if(country == "NG"){
    fd_cont <- rbind(ureaFert,MOPFert , DAPFert,CANFert,NPK171717Fert,NPK151515Fert, NPK201010Fert,TSPFert,SSPFert)
  }else{
    fd_cont <- rbind(ureaFert,MOPFert , DAPFert,CANFert,NPK171717Fert,NPK151515Fert, NPK201010Fert,TSPFert,SSPFert,Minjingu_Nafaka)
  }
  
  
  fd_cont <- droplevels(fd_cont[fd_cont$available == TRUE, ])
  fd_cont$price <- fd_cont$costPerBag / fd_cont$bagWeight	
  fd_cont <- subset(fd_cont, select=-c(available))
  
  OtherFertilizers <- data.frame(expand.grid(name = c(newFert1name, newFert2name, newFert3name, newFert4name, newFert5name)),
                                 expand.grid(N_cont=c(newFert1N_cont, newFert2N_cont, newFert3N_cont, newFert4N_cont, newFert5N_cont)),
                                 expand.grid(P2O5_cont=c(newFert1P2O5, newFert2P2O5, newFert3P2O5, newFert4P2O5, newFert5P2O5)),
                                 expand.grid(K2O_cont=c(newFert1K2O, newFert2K2O, newFert3K2O, newFert4K2O, newFert5K2O)),
                                 expand.grid(newFertCostperBag=c(newFert1CostperBag, newFert2CostperBag, newFert3CostperBag, newFert4CostperBag, newFert5CostperBag)),
                                 expand.grid(newFertBagWt=c(newFert1BagWt, newFert2BagWt, newFert3BagWt, newFert4BagWt, newFert5BagWt)))			
  OtherFertilizers <- droplevels(OtherFertilizers[!is.na(OtherFertilizers$name), ])
  
  if(nrow(OtherFertilizers) > 0){
    newfert <- NULL
    for(k in 1:nrow(OtherFertilizers)){
      OF <- OtherFertilizers[k, ]
      
      if(OF$N_cont == 0){
        N_cont <- 0
      }else{
        N_cont <- round(OF$N_cont/100,digits=3)
      }
      
      if(OF$P2O5_cont == 0){
        P_cont <- 0
      }else{
        P_cont <- round(0.44/OF$P2O5_cont,digits=3)
      }
      
      if(OF$K2O_cont == 0){
        K_cont <- 0
      }else{
        K_cont <- round(0.83/OF$K2O_cont,digits=3)
      }		
      
      fnew <- data.frame(type = OF$name, N_cont = N_cont, 
                         P_cont = P_cont, K_cont = K_cont,
                         costPerBag = OF$newFertCostperBag, bagWeight = OF$newFertBagWt)
      newfert <- rbind(newfert, fnew)
    }
    newfert$price <- newfert$costPerBag / newfert$bagWeight
    
    fd_cont <- rbind(fd_cont, newfert)
  }
  
  fd_cont <- subset(fd_cont, select=-c(costPerBag, bagWeight))
  
  return(fd_cont)
}



#' @param for a location get the closests pixel with WLY and CY, get the closest planting date and harvesting dates as well
#' @param PD <- "2018-03-16"
#' @param HD <- "2019-05-25"
#' @param lat <- 6.225
#' @param lon <- 4.675
#' @param NG_CY_FertRecom is QUEEFTS output with lat, lon, CY, WLY, nutrient rates NPK, planting dates and harvest dates
Onepx_WLY_CY <- function(lat, lon, PD, HD, NG_CY_Fertdata){
  # pDate <-  datesIn365(PD, pl=TRUE)
  # hDate <-  datesIn365(HD, hv=TRUE)
  # ## if planting and harvestins is in different years the days in the planting year should be added to the days in harvesting year
  # if(pDate$year_pl < hDate$year_hv){
  #   hDate$day_hv <- hDate$day_hv + (365 - pDate$day_pl)
  # }
  ## subset WLY, Cu and fert recom for rteh planting and harvest days
  possiblePlDate <- data.frame(p_pl= as.numeric(unique(NG_CY_Fertdata$pl_Date)), a_pl =PD) 
  possiblePlDate$diffl <- abs(possiblePlDate$p_pl - possiblePlDate$a_pl)
  plantingDate <- possiblePlDate[possiblePlDate$diffl == min(possiblePlDate$diffl),]$p_pl
  
  possibleHvDate <- data.frame(p_hv= as.numeric(unique(NG_CY_Fertdata$daysOnField)), a_hv = HD) 
  possibleHvDate$diffl <- abs(possibleHvDate$p_hv - possibleHvDate$a_hv)
  harvestDate <- possibleHvDate[possibleHvDate$diffl == min(possibleHvDate$diffl),]$p_hv
  fertRecom_dates <- droplevels(NG_CY_Fertdata[NG_CY_Fertdata$pl_Date == plantingDate & NG_CY_Fertdata$daysOnField == harvestDate, ])
  
  ## subset for lat and lon
  point_px <- data.frame(lat=lat, lon=lon)
  fertRecom_dates$latDiff <- abs(fertRecom_dates$lat - lat)
  minlat_coor <- droplevels(fertRecom_dates[fertRecom_dates$latDiff == min(fertRecom_dates$latDiff), ])
  
  minlat_coor$lonDiff <- abs(minlat_coor$long - lon)
  minlatlon_coor <- droplevels(minlat_coor[minlat_coor$lonDiff == min(minlat_coor$lonDiff), ])
  rownames(minlatlon_coor) <- NULL
  minlatlon_coor <- subset(minlatlon_coor, select = -c(latDiff, lonDiff))
  return(minlatlon_coor)
}



getRFY <- function(HD, 
                   RDY, 
                   country = c("NG", "TZ")){
  
  #SHORT DEF:   Function to convert root DM yield into root fresh matter yield (RFY)
  #RETURNS:     RFY: root fresh yield in the same units as root DM yield input
  #DESCRIPTION: Function to predict root FM yield based on date of harvest and country, using data from gravimetric starch measurements conducted across ACAI trials.
  #INPUT:       HD: harvest date (Date format)
  #             RDY: root dry matter yield (user's units)
  #             country = c("NG", "TZ")
  
  # d  <- as.numeric(strftime(HD, format = "%j"))
  d <- HD
  fd <- read.csv("fd2.csv") #data.frame with day of the year (dayNr = [1..366]) and %DM (DMCont = [0..100], by country)
  DC <- merge(data.frame(dayNr=d), fd[fd$country==country,], sort=FALSE)$DMCont
  RFY <- RDY / DC * 100
  
  return(RFY)
  
}



getRDY <- function(HD, RFY, country){
  #SHORT DEF:   Function to convert root FM yield into root dry matter yield (RDY): user define CY in FM in ton/ha, QUEFTS require Cy in DM kg/ha
  #RETURNS:     RDY: root dry yield in the same units as root FM yield input
  #INPUT:       HD: harvest date (Date format)
  #             RFY: root fresh matter yield (user's units)
  #             country = c("NG", "TZ")
  if(country=="NG") {
    ## current yield is given by the user as FM ton/ha, we need t change it to DM in Kg/ha for QUEFTS
    #d  <- as.numeric(strftime(HD, format = "%j"))
    if(HD > 366){
      HD <- HD - 366
    }
    d <- HD
    fd <- read.csv("fd2.csv")  #data.frame with day of the year (dayNr = [1..366]) and %DM (DMCont = [0..100], by country)
    DC <- merge(data.frame(dayNr=d), fd[fd$country==country,], sort=FALSE)$DMCont
    RDY <- (RFY * DC)/100
    return(RDY)
  }
}




#' get optimized fertilizer rate, target yield for the recommended rate and net revenue given cost and investment
run_Optim_NG2 <- function(rootUP, QID, fertilizer, invest, areaHa, HD, WLY=WLY, DCY = DCY){	
  require(tidyr)
  
  ## input of CY and WLY are in dry wt in KG/ha
  QID$water_limited_yield <- WLY	
  initial <- rep(0, nrow(fertilizer))
  lowerST <- rep(0, nrow(fertilizer))
  
  ## both CY and TY should be changed to user land size in ton/ha and fresh wt
  CY_user <- ((getRFY(HD = HD, RDY =  DCY , country = country))/1000)  * areaHa
  # WLY_user <- ((getRFY(HD = as.Date(HD), RDY =  WLY , country = country))/1000)  * areaHa
  WLY_user <- ((getRFY(HD = HD, RDY =  WLY , country = country))/1000)  * areaHa
  
  
  FR <- optim(par=initial, fn=optim_NR, lower=lowerST, method = "L-BFGS-B", control=list(fnscale=-1), rootUP=rootUP, 
              QID=QID, CY=DCY, fertilizer=fertilizer, invest=invest, HD=HD)$par
  
  if(all(FR == 0)){
    return(data.frame(N=0, P=0, K= 0,WLY=WLY_user, CurrentY = CY_user, TargetY = CY_user,  TC =0, NR=0))
  }else{
    
    fertilizer$FR <- FR	
    
    ## NPK rate for ha of land	
    N <- as.vector(FR %*% fertilizer$N_cont)
    P <- as.vector(FR %*% fertilizer$P_cont)
    K <- as.vector(FR %*% fertilizer$K_cont)
    rec <- c(N, P, K)	
    
    ## NPK rate for user land size
    NPK_user <- rec * areaHa
    
    ## TY for ha of land 	
    TY <- QUEFTS1_Pedotransfer_LGA(QID, rec)	# Yield possible at recommended NPK in kg/ha dry wt. 
    
    ## both CY and TY should be changed to user land size in ton/ha and fresh wt
    TY_user <- ((getRFY(HD = HD, RDY = TY, country = country))/1000)  * areaHa
    
    ## total cost per ha
    TC <- (sum(FR * fertilizer$price))* areaHa
    
    ## net revenue on the users land size
    GR <- (TY_user - CY_user) * rootUP                      # Gross revenue given root up is for fresh wt ton/ha
    NR <- round(GR - TC, digits=0)    											# Net Revenue
    
    ## reporting the recommended fertilizers
    Recomfr <- fertilizer[fertilizer$FR > 0, ]
    Recomfr$FR <- Recomfr$FR * areaHa
    Recomfr_wide <- spread(Recomfr[, c('type', 'FR')], type, FR)
    
    d1 <- data.frame(N=NPK_user[1], P=NPK_user[2], K= NPK_user[3],
                     WLY=WLY_user, CurrentY = CY_user, TargetY = TY_user,  TC =TC, NR=NR)
    d2 <- cbind(d1, Recomfr_wide)
    d2$NR <- ((d2$TargetY - d2$CurrentY)*rootUP) - d2$TC
    if(d2$TargetY < d2$CurrentY){
      d2$TargetY <- d2$CurrentY
      # d2$N <- d2$P <- d2$K <- d2$TC <- d2$NR <- d2$urea <- d2$DAP <- d2$YaraMila_UNIK <- 0
      d2$N <- d2$P <- d2$K <- d2$TC <- d2$NR <- d2$urea <- d2$NPK15_15_15 <- 0
    }
    
    return(d2)
  }
  
}



Check_25kg_CB <- function (fert_optim){
  ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
  onlyFert <- subset(fert_optim, select = -c(N, P, K, WLY, CurrentY, TargetY, TC, NR))
  fertinfo <- subset(fert_optim, select = c(N, P, K, WLY, CurrentY, TargetY, TC, NR))
  RecomperHa2 <- gather(onlyFert, type, amount)
  onlyFert2 <- droplevels(RecomperHa2[RecomperHa2$amount > 25, ])
  if(nrow(onlyFert2) == 0 ){ ## if all fertilizer recom < 25 kg/ha all will be set to 0
    fert_optim$urea <- fert_optim$DAP <- fert_optim$MOP <- fert_optim$NR <- fert_optim$TC <- fert_optim$N <- fert_optim$P <- fert_optim$K <- 0
    fert_optim$TargetY <- fert_optim$CurrentY
    return(fert_optim)
  }else if (ncol(onlyFert) == nrow(onlyFert2)){ ## if all fertilizer recom are >= 25 kg/ha they will be kept and only checked for NR >= 18% of invest
    GPS_fertRecom <- NRabove18Cost_Maize(ds=fert_optim)
    if(!'urea' %in% names(GPS_fertRecom)){GPS_fertRecom$urea <- 0}
    if(!'DAP' %in% names(GPS_fertRecom)){GPS_fertRecom$DAP <- 0}
    if(!'MOP' %in% names(GPS_fertRecom)){GPS_fertRecom$MOP <- 0}
    GPS_fertRecom <- GPS_fertRecom[, c(names(fertinfo), c("urea", "DAP", "MOP"))]
    return(GPS_fertRecom)
  }else{
    fert25 <- spread(onlyFert2, type, amount) ## when some fertilizer recom are dropped b/c < 25 kg/ha, ty and NR should be recalculated
    fert_optim2 <- cbind(fertinfo, fert25)	
    fertilizer <- fertilizers[fertilizers$type %in% onlyFert2$type, ]
    Reset_fert_Cont <- Rerun_25kgKa_Maize(rootUP=maizePrice, rdd=fert_optim2, fertilizer=fertilizer, QID=FCY1_WLYCY, onlyFert=onlyFert2)
    
    if(Reset_fert_Cont$NR <= 0){ ## after rerunning avoid <25KG/ha fertilizers, if NR <=0
      Reset_fert_Cont <- fertinfo
      Reset_fert_Cont$N <- Reset_fert_Cont$P <- Reset_fert_Cont$K <- Reset_fert_Cont$TC <- Reset_fert_Cont$NR <- 0
      Reset_fert_Cont$TargetY <- Reset_fert_Cont$CurrentY
      Reset_fert_Cont$urea <- Reset_fert_Cont$DAP <- Reset_fert_Cont$MOP <- 0
      return(Reset_fert_Cont)
    }else{
      GPS_fertRecom <- NRabove18Cost_Maize(ds=Reset_fert_Cont)
      if(!'urea' %in% names(GPS_fertRecom)){GPS_fertRecom$urea <- 0}
      if(!'DAP' %in% names(GPS_fertRecom)){GPS_fertRecom$DAP <- 0}
      if(!'MOP' %in% names(GPS_fertRecom)){GPS_fertRecom$MOP <- 0}
      GPS_fertRecom <- GPS_fertRecom[, c(names(fertinfo), c("urea",  "DAP", "MOP"))] ## for Tanzania
      return(GPS_fertRecom)
    }
  }
  
}


#'  Optimize the UREA, TSP and MOP needed to maximize the NR. x1, x2, x3 = Urea, MOP and Nafaka kg/ha. 
optim_NR <- function(fertRate, rootUP, QID, CY, fertilizer, invest, HD){ 
  f_price <-fertilizer$price	
  TC <- sum(fertRate * f_price)					
  
  ## Kg of Urea, Kg of NPK151515, Kg of NPK201010, Kg of MOP
  
  N <- as.vector(fertRate %*% fertilizer$N_cont)
  P <- as.vector(fertRate %*% fertilizer$P_cont)
  K <- as.vector(fertRate %*% fertilizer$K_cont)		
  
  rec <- c(N,P,K)	
  
  TotalYield <- QUEFTS1_Pedotransfer_LGA(QID, rec)
  
  #AdditionalYield <- (getRFY(HD = as.Date(HD), RDY = (TotalYield - CY), country = country))/1000 ## DM is converted to FW and then from KG/ha to ton/ha
  AdditionalYield <- (getRFY(HD = HD, RDY = (TotalYield - CY), country = country))/1000 ## DM is converted to FW and then from KG/ha to ton/ha
  #AdditionalYield <- (TotalYield - CY)*0.003
  PriceYield <- AdditionalYield * rootUP
  NetRev <- PriceYield - TC
  if (!is.na(invest) & TC > invest) {NetRev <- NetRev - (invest-TC)^2} #penalize NR if costs exceed investment cap	
  return(NetRev)		
}


#' computes target yield in tonnes/ha from a given NPK rate
#' @param QID a data frame containing soil NPK, WLY (kg/ha dry wt.), 
#' @param rec recomended NPK rate
#' @returnType 
#' @return target yield in ton/ha dry matter
#' 
#' @author Meklit
#' @export
QUEFTS1_Pedotransfer <- function(QID, rec){		
  QID$WLY <- QID$water_limited_yield
  
  crop_param <- cbind(NUE(HI=0.52), data.frame(rN=0, rP=0, rK=0, max_yield=QID$WLY, tolerance=0.01))	## nutrient use efficiency of the crop
  
  
  Queft_Input_Data_Var1 <- cbind(QID, crop_param)
  indata <- Queft_Input_Data_Var1[,c("lat","long" ,"WLY","aN", "dN", "aP", "dP","aK","dK", "rN", "rP", "rK", "soilN", "soilP", "soilK","max_yield", "tolerance")]
  
  N_rate <- rec[1]
  P_rate <- rec[2]
  K_rate <- rec[3]
  
  TargetYield_from_NPK <- NPK_TargetYield_forOutput(NutrUse_soilNPK=indata, N_rate, P_rate, K_rate)
  
  return(TargetYield_from_NPK$TargetYield)
}


NUE <- function(HI, CmaxNroots=6.6, CminNroots=2.5, CmaxNtops=17.9, CminNtops=7.9, CmaxProots=1.5, CminProots=0.8, CmaxPtops=2.8, CminPtops=0.9, 
                CmaxKroots=11, CminKroots=2.8, CmaxKtops=18.8, CminKtops=3.4 ){
  aN = round(1000 * HI/(HI * CmaxNroots + (1 - HI) * CmaxNtops), digits=0)
  dN = round(1000 * HI/(HI * CminNroots + (1 - HI) * CminNtops), digits=0)
  
  aP = round(1000 * HI/(HI * CmaxProots + (1 - HI) * CmaxPtops), digits=0)
  dP = round(1000 * HI/(HI * CminProots + (1 - HI) * CminPtops), digits=0)
  
  aK = round(1000 * HI/(HI * CmaxKroots + (1 - HI) * CmaxKtops), digits=0)
  dK = round(1000 * HI/(HI * CminKroots + (1 - HI) * CminKtops), digits=0)
  
  return(data.frame(aN=aN, dN=dN,aP=aP,dP=dP,aK=aK,dK=dK))
  
}






#' using the output of function "NPK_TargetYield_forinput" and a dat frame per lon and lat for intended NPK input
#' this function calculates the yield that can be obtained for intended NPK rate.
#' @param NutrUse_soilNPK 
#' @param NPKdata: needs to be provided
#' @return 
#' 
#' @author Meklit
#' @export
NPK_TargetYield_forOutput <- function(NutrUse_soilNPK, N_rate, P_rate, K_rate){	
  NutrUse_soilNPK$N_rate <- N_rate
  NutrUse_soilNPK$P_rate <- P_rate
  NutrUse_soilNPK$K_rate <- K_rate
  
  ## Supply of nutrients to the crop
  NutrUse_soilNPK$SN <- NutrUse_soilNPK$N_rate + NutrUse_soilNPK$soilN
  NutrUse_soilNPK$SP <- NutrUse_soilNPK$P_rate + NutrUse_soilNPK$soilP
  NutrUse_soilNPK$SK <- NutrUse_soilNPK$K_rate + NutrUse_soilNPK$soilK
  
  ## Actual Uptake of nutrients: crop param + nutrient supply
  tmp <- ddply(NutrUse_soilNPK,.(lat, long), actual_uptake_tool)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, tmp, by=c("lat", "long"))
  
  ## max and min yield: actual uptake and crop param. min of N uptake constrianed by availability of P, K and water
  maxminY <- ddply(NutrUse_soilNPK,.(lat, long), max_min_yields_tools)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, maxminY, by=c("lat", "long"))	
  
  ## final yield: min yield for combined uptake of 2 nutrients assuming the 3rd is not limiting, should be < WLY, and take meanof the six combinations 
  Target_Yield <- ddply(NutrUse_soilNPK,.(lat, long), quefts_tools)	
  TY <- data.frame(lat=Target_Yield$lat, lon=Target_Yield$long, TargetYield=Target_Yield$FinalYield)
  
  return(TY)
}


actual_uptake_tool <- function(ds_supply){	
  with(ds_supply,
       {
         UNP <- nutrient_uptake(S1 = SN, S2 = SP, d1 = dN, a1 = aN, d2 = dP, a2 = aP, r1 = rN, r2 = rP)
         UNK <- nutrient_uptake(S1 = SN, S2 = SK, d1 = dN, a1 = aN, d2 = dK, a2 = aK, r1 = rN, r2 = rK)
         UNW <- water_dependent_nutrient_uptake(S1 = SN, WLY = WLY, d1 = dN, a1 = aN, r1 = rN)
         UN <- min(UNP, UNK, UNW)
         
         
         UPN <- nutrient_uptake(S1 = SP, S2 = SN, d1 = dP, a1 = aP, d2 = dN, a2 = aN, r1 = rP, r2 = rN)
         UPK <- nutrient_uptake(S1 = SP, S2 = SK, d1 = dP, a1 = aP, d2 = dK, a2 = aK, r1 = rP, r2 = rK)
         UPW <- water_dependent_nutrient_uptake(S1 = SP, WLY = WLY, d1 = dP, a1 = aP, r1 = rP)
         UP <- min(UPN, UPK, UPW)
         
         
         UKN <- nutrient_uptake(S1 = SK, S2 = SN, d1 = dK, a1 = aK, d2 = dN, a2 = aN, r1 = rK, r2 = rN)
         UKP <- nutrient_uptake(S1 = SK, S2 = SP, d1 = dK, a1 = aK, d2 = dP, a2 = aP, r1 = rK, r2 = rP)
         UKW <- water_dependent_nutrient_uptake(S1 = SK, WLY = WLY, d1 = dK, a1 = aK, r1 = rK)
         UK <- min(UKN, UKP, UKW)
         
         
         return(data.frame(UN=UN, UP=UP, UK=UK))
       })
}



#' Nutrient uptake depends on the soil supply of the nutrient and the supply of other nutrients
nutrient_uptake <- function(S1=NA, S2=NA, d1=NA, a1=NA, d2=NA, a2=NA, r1=NA, r2=NA) {	
  # N, P and K uptakes based on QUEFTS
  if (S1 < r1 + ((S2 - r2) * a2 / d1)) {
    uptakeX_givenY = S1
  } else if (S1 > r1 + ((S2 - r2) * (2 * d2 / a1 - a2 / d1))) {
    uptakeX_givenY = r1 + (S2 - r2) * (d2 / a1)
  } else {
    uptakeX_givenY = S1 - 0.25 * (S1 - r1 - (S2 - r2) * (a2 / d1))^2 / ((S2 - r2) * (d2 / a1 - a2 / d1))
  }
  # Nutrient uptake given availability of other nutrient
  return(uptakeX_givenY)
}



water_dependent_nutrient_uptake <- function(S1=NA, WLY=NA, d1=NA, a1=NA, r1=NA) {	
  if (S1 < r1 + WLY / d1) {
    uptakeX_givenWater = S1    
  } else if (S1 > r1 + 2*WLY/a1 - WLY/d1) {
    uptakeX_givenWater = WLY / a1    
  } else {
    uptakeX_givenWater = S1 - 0.25 * (S1 - r1 - WLY/d1)^2 / (WLY / a1 - WLY / d1)    
  }
  
  return(uptakeX_givenWater)
}


max_min_yields_tools <- function(dss){	
  
  YNA <- max((dss$UN - dss$rN), 0) * dss$aN
  YND <- max((dss$UN - dss$rN), 0) * dss$dN
  YPA <- max((dss$UP - dss$rP), 0) * dss$aP
  YPD <- max((dss$UP - dss$rP), 0) * dss$dP
  YKA <- max((dss$UK - dss$rK), 0) * dss$aK
  YKD <- max((dss$UK - dss$rK), 0) * dss$dK
  
  
  return(data.frame(YNA=YNA, YND=YND, YPA=YPA, YPD=YPD, YKA=YKA, YKD=YKD))
  
}



quefts_tools <- function(supply_wly){	
  # Actual uptake of nutrients.
  tmp <- actual_uptake_tool(supply_wly)
  supply_wly$UN <- tmp[[1]]
  supply_wly$UP <- tmp[[2]]
  supply_wly$UK <- tmp[[3]]
  
  # Maximum and minimum yields, depending on maximum accumulation and dilution.
  yields <- max_min_yields_tools(supply_wly)
  supply_wly$YNA <- yields$YNA
  supply_wly$YND <- yields$YND
  supply_wly$YPA <- yields$YPA
  supply_wly$YPD <- yields$YPD
  supply_wly$YKA <- yields$YKA
  supply_wly$YKD <- yields$YKD
  
  # Final yield based on the combinations of nutrient uptake and minimum + maximum yields.
  supply_wly$FinalYield <- final_yield_tools(supply_wly)
  
  return(supply_wly)
}


#' after setting fertilizer recommendation <25 kg/ha Urea, MOP or Nafaka, target yield with the remaining recommended fertilizer is  re-estimated  and 
#'  total cost, gross and net revenue are re calcuated.
#' @param rootUP cassava root price 
#' @param zone 
#' @param wdd has dry wt
#' @param rdd has fresh wt
#' @param fertilizer 
#' @author Meklit
#' @export

Rerun_25kgKa_try <- function(rootUP, rdd, fertilizer, QID, onlyFert, country, WLY=WLY, DCY = DCY){
  
  QID$water_limited_yield <- WLY
  
  fertilizer <- merge(fertilizer, onlyFert, by='type')
  TC <- (sum(fertilizer$price %*% fertilizer$amount) ) * areaHa
  N  <- as.vector(fertilizer$amount %*% fertilizer$N_cont)
  P  <- as.vector(fertilizer$amount %*% fertilizer$P_cont)
  K  <- as.vector(fertilizer$amount %*% fertilizer$K_cont)
  rec <- c(N, P, K)
  TY  <- QUEFTS1_Pedotransfer_LGA(QID, rec)					#dry wt yield in kg/ha
  
  TY_user  <- ((getRFY(HD = HD, RDY = TY, country = country))/1000) * areaHa
  CY_user  <- ((getRFY(HD = HD, RDY = DCY, country = country))/1000) * areaHa
  rdd$CurrentY <- CY_user
  rdd$TargetY <- TY_user
  rdd$TC <- TC
  rdd$NR <- ((rdd$TargetY - rdd$CurrentY)*rootUP) - rdd$TC
  rdd$N <- N
  rdd$P <- P
  rdd$K <- K
  
  if(rdd$TargetY <= rdd$CurrentY | rdd$NR <= 0 ){
    rdd$N <- rdd$P <- rdd$K <- rdd$TC <- rdd$NR <- 0
    rdd$TargetY <- rdd$CurrentY
  }
  
  return(rdd)
  #	
  #	return(data.frame(lat=recalc_data$lat, lon=recalc_data$lon, rateUrea,  rateNPK151515, rateNPK201010,rateMOP, currentY= CY, 
  #					targetY=TY, WLY=WLY, netRev=NR,totalCost = TC, N=N, P=P, K=K, plDate=recalc_data$plDate))
}




NRabove18Cost_NG <- function(ds){
  #if(ds$NR <= (ds$TC + (ds$TC * 0.18))){
  if(ds$NR < (ds$TC * 0.18)){
    fertRecom <- subset(ds, select = c(lat,lon, plDate, N, P, K, WLY, CurrentY,TargetY, TC, NR, harvestDate))
    fertRecom$N <- fertRecom$P <- fertRecom$K <- fertRecom$TC <- fertRecom$NR <- 0
    fertRecom$TargetY <- fertRecom$CurrentY
    fertRecom$urea <- fertRecom$NPK15_15_15 <- 0
    ds <- fertRecom
  }	
  return(ds)
}

NRabove18Cost_FertTest <- function(ds) {
  if (ds$NR < ds$TC * 0.2) {
    fertRecom <- subset(ds, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate))
    fertRecom$N <- fertRecom$P <- fertRecom$K <- fertRecom$TC <- fertRecom$NR <- 0
    fertRecom$TargetY <- fertRecom$CurrentY
    
    onlyFert <- subset(ds, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate))
    row.names(onlyFert) <- NULL
    onlyFert[, 1:ncol(onlyFert)] <- 0
    
    fertRecom <- cbind(fertRecom, onlyFert)
    ds <- fertRecom
  }
  row.names(ds) <- NULL
  return(ds)
}


#' Yield calculated based on the combined uptake of 2 nutrients, while taking into account the availability of the third nutrient.  
yield_nutrients_combined <- function(U1=NA, d1=NA, a1=NA, Y2A=NA, Y2D=NA, Y3D=NA, r1=NA){	
  # Determine which nutrient limited yield is lowest.
  YxD = min(Y2D, Y3D)	
  # If the uptake of one of the nutrients, and therefore the yield associated with that 
  # nutrient, is zero the overall yield is also zero.
  if (U1 == 0 || YxD == 0) {
    Y12 = 0
  }else{
    Y12 = Y2A + (2 * (YxD - Y2A) * (U1 - r1 - Y2A / d1)) / (YxD / a1 - Y2A / d1) -
      (YxD - Y2A) * (U1 - r1 - Y2A / d1)^2 / (YxD / a1 - Y2A / d1)^2
  }
  # Return the calculated yield based on the uptake of nutrients 1 and 2
  return(Y12)
}


final_yield_tools <- function(Uptake_Yield){	
  with(Uptake_Yield, 
       {
         YNP <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YPA, Y2D = YPD, Y3D = YKD, r1 = rN)
         YNK <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YKA, Y2D = YKD, Y3D = YPD, r1 = rN)
         YPN <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YNA, Y2D = YND, Y3D = YKD, r1 = rP)
         YPK <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YKA, Y2D = YKD, Y3D = YND, r1 = rP)
         YKN <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YNA, Y2D = YND, Y3D = YPD, r1 = rK)
         YKP <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YPA, Y2D = YPD, Y3D = YND, r1 = rK)
         
         # Make sure the nutrient limited yields do not exceed the maximum possible yield = WLY
         YNPc <- min(c(YNP, YND, YPD, YKD, WLY))
         YNKc <- min(c(YNK, YND, YPD, YKD, WLY))
         YPNc <- min(c(YPN, YND, YPD, YKD, WLY))
         YPKc <- min(c(YPK, YND, YPD, YKD, WLY))
         YKNc <- min(c(YKN, YND, YPD, YKD, WLY))
         YKPc <- min(c(YKP, YND, YPD, YKD, WLY))
         
         #Final estimate
         YEc <- mean(c(YNPc, YNKc, YPNc, YPKc, YKNc, YKPc))
         
         if(YEc > WLY){
           YEc == WLY
         }
         
         return(YEc)         
       })
}





###########################################################################################################################
## sms, email and R markdown
###########################################################################################################################
#'  @param fertilizers: data frame with type, N_cont, P_cont, K_cont, price. Price is per kg of fertilizer
#'  @param NG_CY_Fertdata: data frame with lat,long,fert_N,fert_P,fert_K, water_limited_yield, CurrentYield,location, pl_Date,zone, harvestDay, harvestmonth, daysOnField
#'  @param SoilData: data frame with lat,long,soilN,soilP,soilK,rec_N, rec_P, rec_K, rel_N,rel_P,rel_K
#'  @param rootUP: a price of 1 tonne of cassava in freshwt. It is used as freshwt price, after QUEFTS give drywt root yield (kg/ha) it is converted to freshwt in tonne/ha and this price is then used
#'  @param areaHa is area of land in ha
#'  @return a data frame with ...

# PD = "2018-03-01"; HD = "2019-05-31"; lat = 10.024; lon = 4.025; country = "NG"; cassUW = 1000; cassUP = 12000; maxInv = 72000;
# SoilData = SoilGridData_NG , userName = "acai cassava", userPhoneCC = 254, userPhoneNr = 702974480,userEmail = "acai.akilimo@gmail.com",
# cassPD = "roots"
# GPS_fertRecom <- (getFRrecommendations)





#' develope the model based on the reltionship between soil NPK, ISRIC soil data and the root yield for the control treatment for the control
#' ISRIC_SoilData is the new location for which we seek soil NPK estimates
#' FCY is Farmer-reported current yield, in tonnes FM per ha (optional, default value = 11)
#' testN_RF the training set for soil N RF model
#' testP_RF the training set for soil P RF model
#' testK_RF the training set for soil K RF model
#' It also requires the RF_N1.RData, RF_P1.RData, RF_K1.RData
getsoilNPK_RFmodel <- function(ISRIC_SoilData, country, lat, long){ 
  #getsoilNPK_RFmodel <- function(ISRIC_SoilData, FCY, testN_RF, testP_RF, testK_RF, country){ 
  
  ISRIC_SoilData$ncluster <- as.factor(ISRIC_SoilData$ncluster)
  
  # ## read data used for modelling, used to fix a bug in RF predict, read nrmally in R_Wrapper wih sme default values for testing
  # testN_RF$ncluster <- as.factor(testN_RF$ncluster)
  # testP_RF$ncluster <- as.factor(testP_RF$ncluster)
  # testK_RF$ncluster <- as.factor(testK_RF$ncluster)
  
  ## soil N
  dcolnames <- c("soilN","exchK","olsenP","Clay_5","Clay_15","Clay_30","percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
                 "pH_15","pH_30","silt_5","silt_15","silt_30","BD_5","BD_15","BD_30","CEC_5","CEC_15", "CEC_30","percentSOC_5",
                 "percentSOC_15", "percentSOC_30", "FC_5","FC_15","FC_30","wp_5","wp_15","wp_30", "sws_5","sws_15","sws_30",
                 "TotalN","Mn","B", "Ca","Fe","Cu", "Al","Mg","Na","ncluster", "country", "CON")
  
  ISRIC_SoilData1 <- ISRIC_SoilData[, dcolnames]
  
  for (f in 1:length(names(ISRIC_SoilData1))) {
    levels(ISRIC_SoilData1[, f]) <- levels(Ndata_Train[, f])
  }
  # 
  # ISRIC_SoilData1 <- rbind(testN_RF[1,], ISRIC_SoilData1)
  # ISRIC_SoilData1 <- ISRIC_SoilData1[-1,]
  # ISRIC_SoilData1$ncluster <- as.factor(ISRIC_SoilData1$ncluster)
  ISRIC_SoilData$soilN <-  exp(predict(RF_N_B, ISRIC_SoilData1))
  
  ## soil P
  Pcolnames <- c("soilP","exchK","olsenP","Clay_5","Clay_15","Clay_30","percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
                 "pH_15","pH_30","silt_5","silt_15","silt_30","BD_5","BD_15","BD_30","CEC_5","CEC_15", "CEC_30","percentSOC_5",
                 "percentSOC_15", "percentSOC_30", "FC_5","FC_15","FC_30","wp_5","wp_15","wp_30", "sws_5","sws_15","sws_30",
                 "TotalN","Mn","B", "Ca","Fe","Cu", "Al","Mg","Na","ncluster","country","CON")
  
  ISRIC_SoilData2 <- ISRIC_SoilData[, Pcolnames]
  for (f in 1:length(names(ISRIC_SoilData2))) {
    levels(ISRIC_SoilData2[, f]) <- levels(Pdata_Train[, f])
  }
  # 
  # ISRIC_SoilData2 <- rbind(testP_RF[1,], ISRIC_SoilData2)
  # ISRIC_SoilData2 <- ISRIC_SoilData2[-1,]
  ISRIC_SoilData$soilP <-  exp(predict(RF_P_B, ISRIC_SoilData2))
  
  ## soil K
  Kcolnames <- c("soilK","exchK","olsenP","Clay_5","Clay_15","Clay_30","percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
                 "pH_15","pH_30","silt_5","silt_15","silt_30","BD_5","BD_15","BD_30","CEC_5","CEC_15", "CEC_30","percentSOC_5",
                 "percentSOC_15", "percentSOC_30", "FC_5","FC_15","FC_30","wp_5","wp_15","wp_30", "sws_5","sws_15","sws_30",
                 "TotalN","Mn","B", "Ca","Fe","Cu", "Al","Mg","Na","ncluster","country","CON")
  
  ISRIC_SoilData3 <- ISRIC_SoilData[, Kcolnames]
  
  for (f in 1:length(names(ISRIC_SoilData3))) {
    levels(ISRIC_SoilData3[, f]) <- levels(Kdata_Train[, f])
  }
  # ISRIC_SoilData3 <- rbind(testK_RF[1,], ISRIC_SoilData3)
  # ISRIC_SoilData3 <- ISRIC_SoilData3[-1,]
  ISRIC_SoilData$soilK <-  exp(predict(RF_K_B, ISRIC_SoilData3))
  
  ISRIC_SoilData$rec_N <- 0.5
  ISRIC_SoilData$rec_P <- 0.15
  ISRIC_SoilData$rec_K <- 0.5
  ISRIC_SoilData$rel_N <- 1
  ISRIC_SoilData$rel_P <- ISRIC_SoilData$soilP / ISRIC_SoilData$soilN
  ISRIC_SoilData$rel_K <- ISRIC_SoilData$soilK / ISRIC_SoilData$soilN
  ISRIC_SoilData$lat <- lat
  ISRIC_SoilData$long <- long
  ISRIC_SoilData$latlong <- paste(ISRIC_SoilData$lat, ISRIC_SoilData$long, sep="_")
  ISRIC_SoilData$Zone <- country
  ISRIC_SoilData <- ISRIC_SoilData[, c("latlong","lat", "long", "soilN","soilP","soilK", "Zone","rec_N", "rec_P", "rec_K", "rel_N","rel_P","rel_K")]
  
  
  return(ISRIC_SoilData)
  
}




















































##' For every lat and long it provides, current yield with zero fertlizer input, and target yield and fertilizer recommendation to get that yield
##' @param zone, is used to define the varieties and HI to get NUE. Lake zone (the default) is proxy for Mkobozi and E & S zone is Kiroba  
##' @param Queft_Input_Data: per lat and long, crop param and soil param, water limited yield, fertlizer recommendation
##' @return 
##' 
##' @author Meklit
##' @export
#QUEFTS_WLY_CY <- function(Quefts_Input_Data, country, pl_Date, wly_data){	
#	wly_plDate <- wly_data[wly_data$plantingDate == pl_Date, c("lat", "long", "wly_KgHa")]
#	colnames(wly_plDate) <- c("lat", "long", "water_limited_yield")	
#	Quefts_Input_Data_wly <- merge(Quefts_Input_Data, wly_plDate, by=c("lat", "long"))
#	
#	## HI: Median for Nigeria=0.55 and Tanzania=0.52. Q3, Nigeria=0.63 and Tanzania=0.61
#	if(country == "Nigeria"){
#		crop_param <- cbind(NUE(HI=0.55), data.frame(rN=0, rP=0, rK=0, max_yield=Quefts_Input_Data_wly$water_limited_yield, tolerance=0.01))
#	}else{
#		crop_param <- cbind(NUE(HI=0.55), data.frame(rN=0, rP=0, rK=0, max_yield=Quefts_Input_Data_wly$water_limited_yield, tolerance=0.01))
#	}
#	
#	## 1. get soil nutrient supply
#	Queft_Input_Data_Var <- cbind(Quefts_Input_Data_wly, crop_param)	
#	supply <- getsupply(Queft_Input_Data_Var) ## to get yield at zero input level
#	
#	
#	## 2. Current yield: either it is input from user or estimate form QUEFT based on soil supply and crop parameters (for now it is yield at zero fertlizer input)
#	actualUptake <- merge(supply,ddply(supply,.(lat, long), actual_uptake_tool), by=c("lat","long"))
#	minmax_Yield <-  merge(actualUptake, ddply(actualUptake,.(lat, long), max_min_yields_tools), by=c("lat","long"))
#	Current_Yield <- ddply(minmax_Yield,.(lat, long), final_yield_tools)## yield at zero input
#	colnames(Current_Yield) <- c("lat", "long", "CurrentYield")
#	Yield_Fertilizer <- merge(wly_plDate, Current_Yield, by=c("lat", "long"))
#	Yield_Fertilizer$plantingDate <- pl_Date
#	return(Yield_Fertilizer)
#}
#
#
##' @param dss 
##' @returnType 
##' @return 
##' 
##' @author Meklit
##' @export
#getsupply <- function(dss){
#	supply <- data.frame(lat=dss$lat, long=dss$long, rel_N=dss$rel_N, rel_P=dss$rel_P, rel_K=dss$rel_K, SN=dss$soilN, SP=dss$soilP, SK=dss$soilK, water_limited_yield = dss$water_limited_yield,
#			aN=dss$aN, dN=dss$dN, aP=dss$aP, dP=dss$dP, aK=dss$aK, dK=dss$dK, rN=dss$rN, rP=dss$rP, rK=dss$rK, max_yield=dss$max_yield,  tolerance=dss$tolerance,
#			WLY = dss$water_limited_yield)
#	
#}
#
#






