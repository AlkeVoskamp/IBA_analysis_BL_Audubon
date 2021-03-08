#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#      Summarize the turnover vales for one threshold and RCP       #
#              and calculate SD across all scenarioes               #
#                           November 2020                           #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 


#-#-# Load libraries #-#-#
library(snowfall)
library(dplyr)
library(data.table)
library(matrixStats)


#-#-# Set the data paths #-#-#
datapath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/"
TOpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_turnover_lists/"
outpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_turnover_lists_summarized/"

#-#-# Get list of IBA names #-#-#
IBAs <- read.csv(paste0(TOpath,"IBA_trigger_species_turnover_GAM_CCSM4_rcp45_2050_MaxKap.csv"))[2]


#-#-# List turnover values for one threshold and one RCP #-#-#
TOfiles <- list.files(TOpath, pattern = "_rcp45_2050_MaxKap.csv") #select threshold and scenario
Specis_ID <- grep(TOfiles, pattern = "_trigger") #select trigger or all
TOfiles <- TOfiles[Specis_ID]


#-#-# Summarize turnover values per IBA #-#-#
TO <- lapply(TOfiles, function(b){
  print(b)
  Name  <- strsplit(b, split = "IBA_trigger_species_turnover_")[[1]][2] #trigger or all
  Name <- strsplit(Name, split = ".csv")[[1]][1]
  onefile <- read.csv(paste0(TOpath, b))
  IBA <- as.data.frame(onefile$TO_IBA)
  colnames(IBA) <- Name
  return(IBA)
})

data <- do.call(cbind,TO)
IBAdata <- cbind(IBAs,data)
IBAdata$SD <- round(rowSds(as.matrix(IBAdata[2:ncol(IBAdata)])),2)
extent <- ncol(IBAdata)-1
IBAdata$Mean <- round(rowMeans(as.matrix(IBAdata[2:extent]), na.rm = T),2)
IBAdata <- na.omit(IBAdata)
head(IBAdata)
hist(IBAdata$SD)


write.csv(IBAdata,paste0(outpath,"IBA_trigger_species_turnover_rcp45_MaxKap.csv"))

#-#-# Plots are generated in QGIS based on the output files #-#-#
