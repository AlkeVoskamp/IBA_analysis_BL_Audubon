#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#               Calculate range extent and overlap                  #
#               Current and future range centroids                  #
#                          November 2017                            #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


rm(list=ls())


#-#-# Load libraries #-#-#
library(geosphere)
library(circular)
library(plyr)
library(CircStats)
library(sp)


#-#-# Bearing between range centroids #-#-#
#setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")
datapath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/Range centroids and extents trig/"
outpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/Summarized range extent/"


#-#-# List files to extract mean coords per species #-#-#
All_files <- list.files(datapath)


#-#-# Lists to go through sites, SDMs and GCMs #-#-#
Species <- c("trigger")
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
        Coords <- Coords[c("SpName","CurrentRange","CurrentRangeKm2","FutureRange","FutureRangeKm2","OverlapRange","OverlapRangeKm2")]
        print(nrow(Coords))
        return(Coords)
        
      })
      
      ##Summarize one scenario
      One_scenario <- Reduce(function(...) merge(..., by = "SpName"),Calc_coords)
      SpName <- One_scenario[c("SpName")]
      CurrentRange <- rowMeans(One_scenario[c(2,8,14,20,26,32,38,44,50,56,62,68)],na.rm = T)
      CurrentRangeKm2 <- rowMeans(One_scenario[c(3,9,15,21,27,33,39,45,51,57,63,69)],na.rm = T)
      FutureRange <- rowMeans(One_scenario[c(4,10,16,22,28,34,40,46,52,58,64,70)],na.rm = T)
      FutureRangeKm2 <- rowMeans(One_scenario[c(5,11,17,23,29,35,41,47,53,59,65,71)],na.rm = T)
      OverlapRange <- rowMeans(One_scenario[c(6,12,18,24,30,36,42,48,54,60,66,72)],na.rm = T)
      OverlapRangeKm2 <- rowMeans(One_scenario[c(7,13,19,25,31,37,43,49,55,61,67,73)],na.rm = T)
      
      Final <- cbind(SpName,CurrentRange,CurrentRangeKm2,FutureRange,FutureRangeKm2,OverlapRange,OverlapRangeKm2)
      write.csv(Final,paste0(outpath,"Summarized_range_shifts_",Species,"_",RCP,"_",Thres,".csv"))
      
    })
  })
})  

#-#-# Summarize range changes #-#-#
Coordfiles <- list.files(outpath)


#-#-# Extract bearings #-#-#
Get_rangeExtent <- lapply(Coordfiles, function(x){
  
  print(x)
  Coords <- read.csv(paste0(outpath,x))
  Coords <- na.omit(Coords)
  
  Name <- paste0(strsplit(x,split="_")[[1]][4],"_",strsplit(x,split="_")[[1]][5],"_",strsplit(x,split="_")[[1]][6])
  Scenario <- strsplit(Name,split = ".csv")[[1]][1]
    
    Coords$PercRangeRemain <- Coords$FutureRangeKm2/(Coords$CurrentRangeKm2/100)
    Coords$PercOverlap <- Coords$OverlapRangeKm2/(Coords$CurrentRangeKm2/100)
    
    MeanCurRangeKm2 <- mean(Coords$CurrentRangeKm2)
    MeanCurRangeKm2SD <- sd(Coords$CurrentRangeKm2)
    
    MeanFutRangeKm2 <- mean(Coords$FutureRangeKm2)
    MeanFutRangeKm2SD <- sd(Coords$FutureRangeKm2)
    
    MeanOverlapRangeKm2 <- mean(Coords$OverlapRangeKm2)
    MeanOverlapRangeKm2SD <- sd(Coords$OverlapRangeKm2)
    
    MeanPercRangeRemain <- mean(na.omit(Coords$PercRangeRemain))
    MeanPercRangeRemainSD  <- sd(na.omit(Coords$PercRangeRemain))
    
    MeanPercOverlap <- mean(na.omit(Coords$PercOverlap))
    MeanPercOverlapSD <- sd(na.omit(Coords$PercOverlap))
    
    DF <- cbind(Scenario,MeanCurRangeKm2, MeanCurRangeKm2SD, MeanFutRangeKm2, MeanFutRangeKm2SD, MeanOverlapRangeKm2,
                MeanOverlapRangeKm2SD, MeanPercRangeRemain, MeanPercRangeRemainSD, MeanPercOverlap, MeanPercOverlapSD)
     
    return(DF)
  
})

Range_extent <- as.data.frame(do.call(rbind,Get_rangeExtent))
head(Range_extent)

#-#-# Save file #-#-#
write.csv(Range_extent,paste0(outpath,"Changes_in_Range_extent.csv"))
