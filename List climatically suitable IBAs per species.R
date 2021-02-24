#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#    Extract the number of IBAs with suitable climate per species   #
#           Currently and in future based on projections            #
#              Derived from the species lists per IBA               #
#                           November 2020                           #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 


#-#-# Load libraries #-#-#
library(snowfall)
library(stringr)


#-#-# Set working directories #-#-#
occurrence_path <- "/home/avoskamp/BirdLife/Projected distributions/Revision/IBA_occurrence_changes/"
matrix_path <- "/home/avoskamp/BirdLife/Projected distributions/Revision/Matrixes/"
data_path <- "/home/avoskamp/BirdLife/"
outpath <- "/home/avoskamp/BirdLife/Projected distributions/Revision/IBAs_suitable_per_species/"


#-#-# Get the list of all species included in the analysis #-#-#
full.table <- get(load(paste0(matrix_path, "GAM_baseline_TSS.RData")))
spnames <- colnames(full.table)
spnames <- lapply(spnames, function(n){
  print(n)
  name <- paste0(strsplit(n, "_")[[1]][1],"_",strsplit(n, "_")[[1]][2])
  return(name)
})
spnames <- do.call(rbind, spnames)
spnames <- spnames[3:length(spnames)]


#-#-# Set the file lists #-#-#
## Scenarioes
scenarioes <- read.csv(paste0(data_path, "File_list_IBA_changes_all.csv"))
scenarioes <- as.vector(scenarioes$File_types)

## IBAs
countryList <- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","French Guiana","Jamaica","Puerto Rico (to USA)",
                 "Guyana","Paraguay","Peru","Mexico","Uruguay","Venezuela","Belize","Costa Rica","Haiti","Cuba","Bahamas",
                 "El Salvador","Guatemala","Honduras","Nicaragua","Panama","Bahamas","Suriname","Dominican Republic")

ibaGridded <- get(load("/home/avoskamp/BirdLife/IBA_Global_2016_01/IBA_Gridded_0.5_Global_newMex.Rdata"))
ibaGriddedAmList <- sapply(ibaGridded,function(x){x$country})
ibaGriddedAm <- ibaGridded[grep(ibaGriddedAmList,pattern = paste(countryList,collapse="|"))]
polygonlist <- ibaGriddedAm 
Get_names <- lapply(1:1653, function(p){
  print(p)
  polyname <- polygonlist[[p]]$name
  return(polyname)
})
IBAlist <- do.call(rbind, Get_names)

#files <- list.files(occurrence_path,pattern = "Yungas Inferiores de Isiboro-Sécure")

#-#-# Extract the number of climatically suitable IBAs per species #-#-#
sfInit(parallel=TRUE,cpus=ceiling(0.8*parallel::detectCores()))
sfLibrary(stringr)
sfExport(list=c("occurrence_path","outpath","spnames","scenarioes","IBAlist"))
SpIBAs <- sfLapply(spnames,function(x){
  
  species <- x
  print(species)
  
  IBA_list <- lapply(IBAlist,function(n){
    
      IBA <- strsplit(n,split = "/")[[1]][1] # IBAs were saved under first name part only
      print(IBA)
      #IBA_name <- str_replace_all(IBA,"/","_")
    
    Scenario_list <- lapply(scenarioes, function(s){
      
      Scenario <- s
      Scenario_name <- strsplit(Scenario,".Rdata")[[1]][1]
      Scenario_name <- strsplit(Scenario_name,"_Splist")[[1]][2]
      Scenario_name <- paste0(strsplit(Scenario_name,"_all_species_")[[1]][1],"_",strsplit(Scenario_name,"_all_species_")[[1]][2])
      
      onePA <- get(load(paste0(occurrence_path,IBA,Scenario)))

      currentlist <- onePA[,1]
      currentlist <- na.omit(currentlist)
      currentlist <- lapply(currentlist, function(n){
        name <- paste0(strsplit(n, "_")[[1]][1],"_",strsplit(n, "_")[[1]][2])
        return(name)
      })
      currentlist <- do.call(rbind, currentlist)
      
      futurelist <- onePA[,2]
      futurelist <- na.omit(futurelist)
      futurelist <- lapply(futurelist, function(n){
        name <- paste0(strsplit(n, "_")[[1]][1],"_",strsplit(n, "_")[[1]][2])
        return(name)
      })
      futurelist <- do.call(rbind, futurelist)
      
    
      if(species %in% currentlist){
      current <- 1
      }else{
      current <- 0
      }
    
      if(species %in% futurelist){
      future <- 1
      }else{
      future <- 0
      }
    
      if((species %in% futurelist) & (species %in% currentlist)){
      both <- 1
      }else{
      both <- 0
      }
    
      Alldata <- cbind(current,future,both)
      Alldata <- as.data.frame(Alldata)
      
      current_col <- paste0("current",Scenario_name )
      future_col <- paste0("future",Scenario_name )
      both_col <- paste0("both",Scenario_name )
      colnames(Alldata) <- c(current_col,future_col,both_col)
      return(Alldata)
    
      })
    
    IBA_data <- do.call(cbind,Scenario_list)
    IBA_data <- cbind(IBA,IBA_data)
    return(IBA_data)
  })
  
  Combined_data <- do.call(rbind,IBA_list)
  save(Combined_data,file=paste0(outpath,species,"_IBA_occurence.Rdata"),compress="xz")
      
})
  
