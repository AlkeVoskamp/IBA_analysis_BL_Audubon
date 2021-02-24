#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#          Clip projections by natal dispersal          #
#               KBA network South America               #
#                   September 2020                      #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-#  Clear workspace #-#-#
rm(list=ls(all=TRUE))


#_#-# Load libraries #-#-#
library(raster)
library(rgdal)
library(maptools)
library(snowfall)


#-#-# List species projections #-#-#
setwd("/home/avoskamp/BirdLife/Projected distributions/Revision/Combined_raw_projections_AUDUBON_2050_MaxKap/")
allFiles <- list.files(pattern="_MaxKap.RData") # Change here for threshold type
#speciesList <- unlist(lapply(allFiles, function(x){paste(strsplit(x,"_",fixed=TRUE)[[1]][1:2],collapse="_")}))
#head(speciesList)


#-#-# Set file paths #-#-#
Futuredistpath <- "/home/avoskamp/BirdLife/Projected distributions/Revision/Combined_raw_projections_AUDUBON_2050_MaxKap/"
CurrentDistpath <- "/home/avoskamp/BirdLife/Projected distributions/All SA species orig dist/"
resultpath <- "/home/avoskamp/BirdLife/Projected distributions/Revision/Combined_raw_projections_AUDUBON_2050_MaxKap_disp/"


#-#-# Load the dispersal data #-#-#
setwd("/home/avoskamp/BirdLife/Projected distributions/Dispersal/")
distances <- read.csv("Dispersal till 2050 in km.csv")
head(distances)


#-#-# Check for missing files #-#-#
check <- lapply(allFiles, function(c){
  name <- strsplit(c, split = ".RData")[[1]][1]
  print(name)
  if(!file.exists(paste0(resultpath, name, "_disp.Rdata"))){
    return(c)
  }
})

allFiles <- do.call(rbind,check)

#x <- speciesList[1]

sfInit(parallel=TRUE, cpus=ceiling(0.8*parallel::detectCores()))
sfLibrary(raster); sfLibrary(snowfall); sfLibrary(rgdal)
sfExport(list=c("allFiles", "Futuredistpath", "CurrentDistpath","resultpath","distances"))

system.time(
  sfLapply(allFiles,function(x){
    
    SpName <-  unlist(lapply(x, function(x){paste(strsplit(x,"_",fixed=TRUE)[[1]][1:2],collapse="_")}))
    
    FileName <- unlist(lapply(x, function(x){paste(strsplit(x,".",fixed=TRUE)[[1]][1],collapse="_")}))
    print(FileName)
    
    if(!file.exists(paste0(resultpath,FileName,"_disp.Rdata",sep=""))){

      ##Get species future suitability
      onespfut <- get(load(paste0(Futuredistpath,FileName,".RData")))
    
      ##Get current species distributions
      if(file.exists(paste0(CurrentDistpath,SpName,"_05_WGS.Rdata"))){
        onespDist <- get(load(paste0(CurrentDistpath,SpName,"_05_WGS.Rdata")))
      
      onespDist[onespDist==0] <- NA
      
      ##Get the dispersal distance for the species
      distRow <- distances[distances$SpName == SpName,]
      distRow <- distRow[,2]
      distRow <- distRow*1000 #Multiply by 1000 meters
      
      ##Change species distribution dataframe to raster
      coordinates(onespDist)=~x+y
      gridded(onespDist) = TRUE
      spdataRast <- rasterFromXYZ(onespDist,digits=4)
      proj4string(spdataRast) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
      
      ##Creater buffer around current distribution using dispersal distance
      b <- buffer(spdataRast, width=distRow) 

      ##Change buffer raster back to data frame 
      coord <- round(coordinates(b),4)
      values <- getValues(b)
      spdataframe <- (as.data.frame(cbind(coord,values)))
      spdataframe <- na.omit(spdataframe)
      colnames(spdataframe) <- c("x","y","NatalDisp")
      
      ##Merge the buffer to the habitat suitability dataframe
      spFutBuf <- merge(onespfut,spdataframe, all.x = TRUE)
      
      ##Save species dataframe
      save(spFutBuf,file=paste0(resultpath,FileName,"_disp.Rdata",sep=""),compress="xz")
      }
    }
  })
)


####TEST
setwd("S:/Alke/Alke - Audubon/Ensemble mean HadGEMrcp4570 buffer/")
test <- get(load("Acanthidops_bairdi_LatLon_70_rcp45_SuitHab_inBuffer.Rdata"))
levelplot(pres~x*y,data=test)
head(test)
library(lattice)
levelplot(pres~x*y,data=FutDistDisp)
