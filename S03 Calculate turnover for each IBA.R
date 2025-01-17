#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                        IBA analysis Script 3                      #
#                  Calculate species turnover per IBA               #
#                 based on current and future richness              #
#                            September 2020                         #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear the memory #-#-#
rm(list=ls(all=TRUE)) 


#-#-# Set working directories #-#-#
matrix_path <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/Occurrence_matrixes"
data_path <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/"
outpath <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/IBA_turnover_lists/"


#-#-# Get the species lists #-#-#
Good_mods <- read.csv(paste0(data_path, "SDMs with high AUC all species.csv"))
Good_mods <- as.vector(Good_mods$Species)
Trigger <- get(load(paste0(data_path,"IBA trigger species.Rdata")))


#-#-# Load current and future species matrixes #-#-#
## Baseline
Baseline_list <- list.files(matrix_path, pattern = "_baseline_")

##Future
Future_list <- list.files(matrix_path, pattern = "_rcp")


#-#-# List of South American countries included in the analysis #-#-#
countryList <- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","French Guiana","Jamaica","Puerto Rico (to USA)",
                 "Guyana","Paraguay","Peru","Mexico","Uruguay","Venezuela","Belize","Costa Rica","Haiti","Cuba","Bahamas",
                 "El Salvador","Guatemala","Honduras","Nicaragua","Panama","Bahamas","Suriname","Dominican Republic")

ibaGridded <- get(load("/home/avoskamp/BirdLife/IBA_Global_2016_01/IBA_Gridded_0.5_Global_newMex.Rdata"))
ibaGriddedAmList <- sapply(ibaGridded,function(x){x$country})
ibaGriddedAm <- ibaGridded[grep(ibaGriddedAmList,pattern = paste(countryList,collapse="|"))]
polygonlist <- ibaGriddedAm 


#-#-# Lists to go through sites, SDMs and GCMs #-#-#
numberlist <- c(1:1653) 
SDMs <- c("GAM", "GBM", "GLM", "RF")
GCMs <- c("CCSM4", "GFDLCM3", "HadGEM2ES")
RCP <- c("rcp26","rcp45","rcp85")
Thres <- c("TSS","MaxKap")


#-#-# Extract turnover #-#-#
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
      
      ##Load the baseline matrix
      base_name <- paste0(SDM, "_baseline_",Thres,".RData")
      splistbase <- get(load(paste0(matrix_path, base_name)))
      basenames <- colnames(splistbase[3:ncol(splistbase)])
      basenames <- lapply(basenames, function(n){
        name <- strsplit(n, split = SDM_name)[[1]][1]
        print(name)
        return(name)
      })
      Base.names <- do.call(rbind,basenames)
      Base.names <- Base.names[(Base.names %in% Good_mods)] 
      
      Base_model <- lapply(Base.names, function(x){ #Change here for trigger or all species
        Base_name <- paste0(x, "_", SDM)
        return(Base_name)})
      Base_model <- do.call(rbind, Base_model)
      Base.List <- as.vector(c("x","y",Base_model))
      splistbase <- splistbase[, Base.List]
      
      ## Subset by trigger species 
      # Trigger_model <- lapply(Trigger, function(x){
      #   Trigger_name <- paste0(x, "_", SDM)
      #   return(Trigger_name)})
      # Trigger_model <- do.call(rbind, Trigger_model)
      # Trigger.Mods <- names(splistbase)[(names(splistbase) %in% Trigger_model)] # Check which trigger species were modelled
      # Trigger.List <- as.vector(c("x","y",Trigger.Mods)) # Make a list of the modelled trigger species
      # splistbase <- splistbase[, Trigger.List]
      splistbase[is.na(splistbase)] <- 0
      
      ## Loop through the different GCMs
      GCM_type <- lapply(GCMs, function(g){ 
        print(g)
        GCM <- g
        fut_name <- paste0(SDM,"_",GCM,"_",RCP,"_50_",Thres,".RData") # change here
        splistfut <- get(load(paste0(matrix_path, fut_name)))
        splistfut <- splistfut[, Base.List]
        
        ## Subset by trigger species
        # splistfut <- splistfut[, Trigger.List]
        # splistfut[is.na(splistfut)] <- 0
        
        Species_moving <- lapply(numberlist,function(x){
        print(x)
        polyname <- polygonlist[[x]]$name
        polycountry <-polygonlist[[x]]$country
        polyID <-polygonlist[[x]]$SitRecID
        if(length(polyID)==0){polyID <- "missing"}
        onepoly <-  polygonlist[[x]]$data
  
        baselist <- merge(onepoly,splistbase,by=c("x","y"),all.x=TRUE)
        l <- ncol(baselist)
        baselistsum <- colSums(baselist[,5:l])
        baselistsum <- as.data.frame(t(baselistsum))
        baselistsum[baselistsum >= 1] <- 1
        baselistsum[is.na(baselistsum)] <- 0
  
        futlist <- merge(onepoly,splistfut,by=c("x","y"))
        l <- ncol(futlist)
        futlistsum <- colSums(futlist[,5:l])
        futlistsum <- as.data.frame(t(futlistsum))
        futlistsum[futlistsum >= 1] <- 1
        futlistsum[is.na(futlistsum)] <- 0

        ## Calculate turnover
        TO_IBA <- sum(abs(futlistsum-baselistsum))/(sum(baselistsum,na.rm=T)+sum(futlistsum,na.rm=T))
        TO_IBA <- round(TO_IBA,2)
  
        return(c(IBA = polyname, ID = polyID, country = polycountry,TO_IBA = TO_IBA ))
  
        }) # GCM close

      GCM_scenario <- as.data.frame(do.call(rbind, Species_moving))
      write.csv(GCM_scenario,paste0(outpath,"IBA_all_species_turnover_",SDM,"_",GCM,"_",RCP,"_2050_",Thres,".csv")) # change here

      }) # SDM close
    }) # RCP close
  }) # Thres close
})

