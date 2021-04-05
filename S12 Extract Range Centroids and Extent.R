#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#         Extract the range centroids and range extents         #
#              Projected current and future ranges              #
#                          October 2020                         #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE))


#-#-# Load libraries #-#-#
library(raster)
library(lattice)
library(snowfall)


#-#-# Set file paths #-#-#
matrix_path <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/BL_matrixes/"
triggerpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/"
splistpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/"
areapath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/"
outpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/Range centroids and extents/"


#-#-# Get trigger species lists #-#-#
Good_mods <- read.csv(paste0(splistpath, "SDMs with high AUC all species.csv"))
Good_mods <- as.vector(Good_mods$Species)
Trigger <- get(load(paste0(triggerpath,"IBA trigger species.Rdata")))
Trigger <- Trigger[Trigger %in% Good_mods]


#-#-# Get the area in km2 per grid cell #-#-#
## The area per grid cell file was derived from the lat long coordinates using the raster package
area <- read.csv(paste0(areapath,"Realm_coordinates_Lat_Lon_area.csv"))
area <- area[c("x","y","km2")]


#-#-# Load current and future species matrixes #-#-#
## Baseline
Baseline_list <- list.files(matrix_path, pattern = "_baseline_")

##Future
Future_list <- list.files(matrix_path, pattern = "_rcp")


#-#-# Lists to go through sites, SDMs and GCMs #-#-#
SDMs <- c("GAM", "GBM", "GLM", "RF")
GCMs <- c("CCSM4", "GFDLCM3", "HadGEM2ES")
RCP <- c("rcp26","rcp45","rcp85")
Thres <- c("TSS","MaxKap")


#-#-# Extract range centroids and range extents #-#-#
Thres_type <- lapply(Thres, function(t){
  print(t)
  Thres <- t
  
  ##Loop through the different RCPs
  RCP_type <- lapply(RCP, function(r){  
    print(r)
    RCP <- r
    
    ##Loop through the different SDMs  
    SDM_type <- lapply(SDMs, function(s){
      print(s)
      SDM <- s
      SDM_name <- paste0("_", SDM)
      
      ## Subset by trigger species if nedded
      base_name <- paste0(SDM, "_baseline_",Thres,".RData")
      current <- get(load(paste0(matrix_path, base_name)))
      
      basenames <- colnames(current[3:ncol(current)])
      basenames <- lapply(basenames, function(n){
        print(n)
        name <- strsplit(n, split = SDM_name)[[1]][1]
        return(name)
      })
      Base.names <- do.call(rbind,basenames)
      Trigger.names <- Base.names[(Base.names %in% Trigger)] 
      
        ## Loop through the different GCMs
        GCM_type <- lapply(GCMs, function(g){ 
          print(g)
          GCM <- g
          fut_name <- paste0(SDM,"_",GCM,"_",RCP,"_50_",Thres,".RData") 
          future <- get(load(paste0(matrix_path, fut_name)))

          if(!file.exists(paste0(outpath,"Range_centroids_and_range_extent_trigger_species_turnover_",SDM,"_",GCM,"_",RCP,"_2050_",Thres,".csv"))){
          
            ## Loop through species list
            Species <- lapply(Base.names, function(x){  # Change here for trigger or all species
        
              Sp_name <- x
              
              Species_name <- paste0(x, "_", SDM) 
              print(Species_name) 
           
              #-#-# Current file #-#-#
              curDist <- current[c("x","y",Species_name)] 
              colnames(curDist) <- c("x","y","pres")
              #curDist$pres[curDist$pres > 0] <- 1 # For habitat clipped distributions
        
              ## Current range data
              currentPres <- subset(curDist, pres ==1)
              current.x <- round(as.numeric(as.character(mean(currentPres$x))),2)
              current.y <- round(as.numeric(as.character(mean(currentPres$y))),2)
        
              currentPres <- merge(currentPres, area, by = c("x","y"))
              currentRange <- nrow(currentPres)
              currentRangeKm2 <- round(sum(currentPres$km2),0)
        
              ## Future file
              futDist <- future[c("x","y",Species_name)] 
              colnames(futDist) <- c("x","y","pres")
              #futDist$pres[futDist$pres > 0] <- 1
        
              ## Future range data
              futurePres <- subset(futDist, pres ==1)
              future.x <- round(as.numeric(as.character(mean(futurePres$x))),2)
              future.y <- round(as.numeric(as.character(mean(futurePres$y))),2)
        
              futurePres <- merge(futurePres, area, by = c("x","y"))
              futureRange <- nrow(futurePres)
              futureRangeKm2 <- round(sum(futurePres$km2),0)
        
              ## Extract the overlap
              combi <- merge(currentPres,futurePres,by=c("x","y"))
              OverlapRange <- nrow(combi)
              OverlapRangeKm2 <- round(sum(combi$km2.x),0)
        
              speciesdata <- cbind(Sp_name,current.x,current.y,currentRange,currentRangeKm2,future.x,future.y,futureRange,futureRangeKm2,OverlapRange,OverlapRangeKm2)
              speciesdata <- as.data.frame(speciesdata)
              colnames(speciesdata) <- c("SpName","CurrentCentroid.x","CurrentCentroid.y","CurrentRange","CurrentRangeKm2","FutureCentroid.x","FutureCentroid.y","FutureRange","FutureRangeKm2","OverlapRange","OverlapRangeKm2")
        
              return(speciesdata)
            }) # Species close
        
          Species_scenario <- as.data.frame(do.call(rbind, Species))
          write.csv(Species_scenario,paste0(outpath,"Range_centroids_and_range_extent_trigger_species_turnover_",SDM,"_",GCM,"_",RCP,"_2050_",Thres,".csv")) # change here
          }
        }) # GCM close
      }) # SDM close
    }) # RCP close
}) # Thres close
