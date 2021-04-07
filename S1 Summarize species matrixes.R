#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                        IBA analysis Script 1                      #
#   Summarize individual species projections into one data matrix   # 
#                            October 2020                           #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## This script summarizes projected species occurrences into an x y presence datatframe (from 1 projection file per species)
## The current script has to be run for each scenario (SDM/GCM combination seperately) but is easier to use in a loop if many scenarioes are included
## The species matrixes are the input for all subsequent analyses presented in the paper - the matrixes are provided on GitHub

#-#-# Clear the memory
rm(list=ls())


#-#-# Set the filepath and list the data files #-#-#
predpath <- " "
outpath <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/"

AllData <- list.files(predpath, pattern = "_GAM_")


#-#-# Load the relevant columns of all species files #-#-#
oneScenario <- lapply(AllData, function(x){
  print(x)
  name <- paste0(strsplit(x,"_")[[1]][1],"_",strsplit(x,"_")[[1]][2],"_",strsplit(x,"_")[[1]][3])
  data <- get(load(x))
  
  #-# Dispersal #-#
  ## If a dispersal estimate is used subset to cells with projected presences that are inside the dispersal buffer
  dataDisp <- subset(data,NatalDisp==1)

  #-# Scenario #-#
  ## Select the right projected presense column (if the file contains several projections)
  ## tryCatch is used if a scenario is missing for a species - in that case the scenario is allocated NAs only
  
  possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_baseline")],error=function(e) e)
  if(inherits(possibleError, "error")){
    dataDisp <- dataDisp[c("x","y")]
    dataDisp$GAM_baseline <- NA
  }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_CCSM4_rcp26_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_CCSM4_rcp26_50 <- NA
  # }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_CCSM4_rcp45_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_CCSM4_rcp45_50 <- NA
  # }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_CCSM4_rcp85_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_CCSM4_rcp85_50 <- NA
  # }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_GFDLCM3_rcp26_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_GFDLCM3_rcp26_50 <- NA
  # }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_GFDLCM3_rcp45_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_GFDLCM3_rcp45_50 <- NA
  # }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_GFDLCM3_rcp85_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_GFDLCM3_rcp85_50 <- NA
  # }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_HadGEM2ES_rcp26_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_HadGEM2ES_rcp26_50 <- NA
  # }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_HadGEM2ES_rcp45_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_HadGEM2ES_rcp45_50 <- NA
  # }
  
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_HadGEM2ES_rcp85_50")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_HadGEM2ES_rcp85_50 <- NA
  # }
  
  #-# Rename the selected columns and return the data
  colnames(dataDisp) <- c("x","y",name)
  return(dataDisp)
})

#-#-# Reduce the data returned for all species into one matrix #-#-#
OneDispSc <- Reduce(function(...) merge(..., all=T), oneScenario)


#-#-# Save the final data matrix #-#-#
save(OneDispSc, file= paste0(outpath,"/GAM_HadGEM2ES_rcp85_50_MaxKap.RData"), compress="xz")


