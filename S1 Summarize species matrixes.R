rm(list=ls())

setwd("/home/avoskamp/BirdLife/Projected distributions/Revision/Combined_raw_projections_AUDUBON_2050_MaxKap_disp/")
AllData <- list.files(pattern = "_GAM_")

#"GAM_baseline" running
#"GAM_CCSM4_rcp26_50" running    
#"GAM_CCSM4_rcp45_50" running   
#"GAM_CCSM4_rcp85_50" running
#"GAM_GFDLCM3_rcp26_50" running   
#"GAM_GFDLCM3_rcp45_50" running  
#"GAM_GFDLCM3_rcp85_50" running
#"GAM_HadGEM2ES_rcp26_50" running
#"GAM_HadGEM2ES_rcp45_50" running
#"GAM_HadGEM2ES_rcp85_50" running
##"NatalDisp"


oneScenario <- lapply(AllData, function(x){
  print(x)
  name <- paste0(strsplit(x,"_")[[1]][1],"_",strsplit(x,"_")[[1]][2],"_",strsplit(x,"_")[[1]][3])
  data <- get(load(x))
  
  ##Dispersal
  dataDisp <- subset(data,NatalDisp==1)

  ##Scenario
  # possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_baseline")],error=function(e) e)
  # if(inherits(possibleError, "error")){
  #   dataDisp <- dataDisp[c("x","y")]
  #   dataDisp$GAM_baseline <- NA
  # }
  
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
  
  possibleError <- tryCatch(dataDisp <- dataDisp[c("x","y","GAM_HadGEM2ES_rcp85_50")],error=function(e) e)
  if(inherits(possibleError, "error")){
    dataDisp <- dataDisp[c("x","y")]
    dataDisp$GAM_HadGEM2ES_rcp85_50 <- NA
  }
  
  colnames(dataDisp) <- c("x","y",name)
  return(dataDisp)
})

OneDispSc <- Reduce(function(...) merge(..., all=T), oneScenario)

## save df just in case
predpath <- "/home/avoskamp/BirdLife/Projected distributions/Revision/Matrixes/"
save(OneDispSc, file= paste0(predpath,"/GAM_HadGEM2ES_rcp85_50_MaxKap.RData"), compress="xz")


