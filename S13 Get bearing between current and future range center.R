#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                       IBA analysis Script 13                      #
#            Calculate bearing between range centroids              #
#               Current and future range centroids                  #
#                          November 2017                            #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls())


#-#-# Load libraries #-#-#
library(geosphere)
library(circular)
library(plyr)
library(CircStats)
library(sp)


#-#-# Bearing between range centroids #-#-#
datapath <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/Range_centroids_and_extents/"
outpath <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/Summarized_range_centroids_and_extents/"


#-#-# List files to extract mean coords per species #-#-#
All_files <- list.files(datapath)


#-#-# Lists to go through sites, SDMs and GCMs #-#-#
Species <- c("all","trigger")
RCP <- c("rcp26","rcp45","rcp85")
Thres <- c("TSS","MaxKap")


#-#-# Loop throug files to get coordinates #-#-#
Species_type  <- lapply(Species, function(s){
  print(s)
  Spec <- s
  
  Spec_sub <- grep(All_files, pattern = Spec)
  Spec_scenarioes <- All_files[Spec_sub]
  
    ##Loop through thresholds
    Thres_type <- lapply(Thres, function(t){
      print(t)
      Thres <- t
  
      Thres_sub <- grep(Spec_scenarioes, pattern = Thres)
      Thres_scenarioes <- Spec_scenarioes[Thres_sub]
  
        ##Loop through the different RCPs
        RCP_type <- lapply(RCP, function(r){
    
          print(r)
          RCP <- r
    
          RCP_sub <- grep(Thres_scenarioes, pattern = RCP)
          RCP_scenarioes <- Thres_scenarioes[RCP_sub]
          
          ##Loop through list and get mean coordinates
          Calc_coords <- lapply(RCP_scenarioes, function(c){
            print(c)
            Scenario_name <- c
            Coords <- read.csv(paste0(datapath,c))
            Coords <- Coords[c("SpName","CurrentCentroid.x","CurrentCentroid.y","FutureCentroid.x","FutureCentroid.y")]
            print(nrow(Coords))
            return(Coords)
            
          })
          
          ##Summarize one scenario
          One_scenario <- Reduce(function(...) merge(..., by = "SpName"),Calc_coords)
          SpName <- One_scenario[c("SpName")]
          CurrentCentroid.x <- rowMeans(One_scenario[c(2,6,10,14,18,22,26,30,34,38,42,46)],na.rm = T)
          CurrentCentroid.y <- rowMeans(One_scenario[c(3,7,11,15,19,21,27,31,35,39,43,47)],na.rm = T)
          FutureCentroid.x <- rowMeans(One_scenario[c(4,8,12,16,20,22,28,32,36,40,44,48)],na.rm = T)
          FutureCentroid.y <- rowMeans(One_scenario[c(5,9,13,17,21,23,29,33,37,41,45,49)],na.rm = T)
          
          Final <- cbind(SpName,CurrentCentroid.x,CurrentCentroid.y,FutureCentroid.x,FutureCentroid.y)
          write.csv(Final,paste0(outpath,"Summarized_range_centroids_",Spec,"_",RCP,"_",Thres,".csv"))
          
        })
    })
})         
       
   
#---#---#---#---#---#---# Summarize the bearings #---#---#---#---#---#---#---#---#
#-#-# Set filepaths #-#-#
outpathII <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/Bearings_between_range_centroids/"


#-#-# List output files summarized coordinates #-#-#
Coordfiles <- list.files(outpath)


#-#-# Extract bearings #-#-#
Get_bearing <- lapply(Coordfiles, function(x){
  
  print(x)
  Coords <- read.csv(paste0(outpath,x))
  Coords <- na.omit(Coords)
  
  Name <- paste0(strsplit(x,split="_")[[1]][4],"_",strsplit(x,split="_")[[1]][5],"_",strsplit(x,split="_")[[1]][6])
  
  ##Set dataframe
  DegData <- data.frame(name= character(0), bearing= numeric(0), Direction= character(0))
  
    for(i in 1:nrow(Coords)){
  
      print(i)
  
      n <- Coords[i,]
      name <- n[c("SpName")]
  
      cur  <- cbind(n[c("CurrentCentroid.x")],n["CurrentCentroid.y"])
      colnames(cur) <- c("x","y")

      fut <- cbind(n["FutureCentroid.x"],n["FutureCentroid.y"])
      colnames(fut) <- c("x","y")
  
      bear <- bearing(cur,fut)
      print(bear)
      
      distance <- spDistsN1(as.matrix(cur),as.matrix(fut),longlat = TRUE) 
      print(distance)
  
      alldata <- (c(name=name,bearing=bear,distance=distance))
      alldata <- as.data.frame(alldata)
      
      ##Add directions
      alldata$Direction <- 1
      alldata$Direction[alldata$bearing > 0 & alldata$bearing <= 45] <- "NNE"
      alldata$Direction[alldata$bearing > 45 & alldata$bearing <= 90] <- "ENE"
      alldata$Direction[alldata$bearing > 90 & alldata$bearing <= 135] <- "ESE"
      alldata$Direction[alldata$bearing > 135 & alldata$bearing <= 180] <- "SSE"
      
      alldata$Direction[alldata$bearing > -180 & alldata$bearing <= -135] <- "SSW"
      alldata$Direction[alldata$bearing > -135 & alldata$bearing <= -90] <- "WSW"
      alldata$Direction[alldata$bearing > -90 & alldata$bearing <= -45] <- "WNW"
      alldata$Direction[alldata$bearing > -45 & alldata$bearing <= -1] <- "NNW"
      
      DegData <- rbind(DegData,alldata)
      
    }
  
  write.csv(DegData,paste0(outpathII,"Bearing_between_range_centers_", Name))  

})


#-#-# Extract overall directions for manuscript #-#-#
All_sum_files <- list.files(outpathII)

## Set to the right file for manuscript info and supplements
CDdata <- read.csv(All_sum_files[8])
head(CDdata)

Allsp <- nrow(CDdata)
PercNNE <- sum(CDdata$Direction == "NNE")/(Allsp/100)
PercENE <- sum(CDdata$Direction == "ENE")/(Allsp/100)
PercESE <- sum(CDdata$Direction == "ESE")/(Allsp/100)
PercSSE <- sum(CDdata$Direction == "SSE")/(Allsp/100)
PercSSW <- sum(CDdata$Direction == "SSW")/(Allsp/100)
PercWSW <- sum(CDdata$Direction == "WSW")/(Allsp/100)
PercWNW <- sum(CDdata$Direction == "WNW")/(Allsp/100)
PercNNW <- sum(CDdata$Direction == "NNW")/(Allsp/100)

PercMovingNorth <- PercNNW + PercNNE
PercMovingSouth <- PercSSW + PercSSE

NoMovingNorth <- sum(CDdata$Direction == "NNW") + sum(CDdata$Direction == "NNE")
NoMovingSouth <- sum(CDdata$Direction == "SSW") + sum(CDdata$Direction == "SSE")
PercMovingNorth
PercMovingSouth
Allsp
NoMovingNorth
NoMovingSouth

mean(CDdata$distance)
sd(CDdata$distance)


#---#---#---#---#---# Extract change in range extent for result section #---#---#---#---#---#
Extpath <- datapath

## Select relevant files
All_ex_files <- list.files(Extpath, pattern = "_rcp85_2050_MaxKap.csv")
Sub_ex_files <- grep(All_ex_files, pattern = "all")
Ex_files <- All_ex_files[c(Sub_ex_files)]

## Summarize
Sum <- lapply(Ex_files, function(x){
  print(x)
  data <- read.csv(paste0(Extpath, x))
  data$PercChange <- data$FutureRangeKm2/(data$CurrentRangeKm2/100)
  data <- data[c("SpName","PercChange")]
  return(data)
})

DF_perc_change <- Reduce(function(...) merge(..., by = "SpName"),Sum)
head(DF_perc_change)

DF_perc_change$MeanPercChange <- rowMeans(DF_perc_change[2:ncol(DF_perc_change)], na.rm = T)
DF_perc_change$MeanPercChangeSD <- rowMeans(DF_perc_change[2:(ncol(DF_perc_change)-1)], na.rm = T)
DF_perc_change <- DF_perc_change[c("SpName", "MeanPercChange", "MeanPercChangeSD")]

PercChange <- mean(na.omit(DF_perc_change$MeanPercChange))
PercChangeSD <- sd(na.omit(DF_perc_change$MeanPercChange))
PercChange - 100
PercChangeSD

