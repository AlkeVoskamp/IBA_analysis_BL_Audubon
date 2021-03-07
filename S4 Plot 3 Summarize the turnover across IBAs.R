#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 

library(snowfall)
library(dplyr)
library(data.table)
library(matrixStats)


#-#-# Set the data paths #-#-#
datapath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/"
TOpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_turnover_lists/"

## IBA turnover values
IBAs <- read.csv(paste0(TOpath,"IBA_trigger_species_turnover_GAM_CCSM4_rcp45_2050_MaxKap.csv"))[2]
TOfiles <- list.files(TOpath, pattern = "_rcp45_2050_TSS.csv")
TO <- lapply(TOfiles, function(b){
  print(b)
  Name  <- strsplit(b, split = "IBA_trigger_species_turnover_")[[1]][2]
  Name <- strsplit(Name, split = ".csv")[[1]][1]
  onefile <- read.csv(paste0(TOpath, b))
  IBA <- as.data.frame(onefile$TO_IBA)
  colnames(IBA) <- Name
  return(IBA)
})

data <- do.call(cbind,TO)
IBAdata <- cbind(IBAs,data)
IBAdata$SD <- rowSds(as.matrix(IBAdata[2:ncol(IBAdata)]))
extent <- ncol(IBAdata)-1
IBAdata$Mean <- rowMeans(as.matrix(IBAdata[2:extent]), na.rm = T)
head(IBAdata)
hist(IBAdata$SD)

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_turnover_lists_summary/")
write.csv(IBAdata,"IBA_trigger_species_turnover_rcp45_TSS.csv")

#-#-# Plots are generated in QGIS based on the output files #-#-#
