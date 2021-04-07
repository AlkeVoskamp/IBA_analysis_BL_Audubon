#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#                        IBA analysis Script 5                      #
#     List all species per PA for which the climate is suitable     #
#           currently and in future based on projections            #
#          Simple overlay of PAs and projected suitability          #
#                             August 2016                           #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear the memory #-#-#
rm(list=ls(all=TRUE)) 


#-#-# Set working directories #-#-#
matrix_path <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/Occurrence_matrixes"
data_path <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/"
outpath <- "https://github.com/AlkeVoskamp/IBA_analysis_BL_Audubon/Data/IBA_occurrence_changes/"


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


#-#-# Extract the species lists #-#-#
Thres_type <- lapply(Thres[1], function(t){
  print(t)
  Thres <- t
  
    ##Loop through the different RCPs
    RCP_type <- lapply(RCP[1], function(r){  
    print(r)
    RCP <- r
    
      ##Loop through the different SDMs
      SDM_type <- lapply(SDMs[1], function(s){
        
        print(s)
        SDM <- s
        SDM_name <- paste0("_", SDM)
        
        ## Get the current matrix
        base_name <- paste0(SDM, "_baseline_", Thres, ".RData")
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
        Trigger_model <- lapply(Trigger, function(x){
          Trigger_name <- paste0(x, "_", SDM)
          return(Trigger_name)})
        Trigger_model <- do.call(rbind, Trigger_model)
        Trigger.Mods <- names(splistbase)[(names(splistbase) %in% Trigger_model)] # Check which trigger species were modelled
        Trigger.List <- as.vector(c("x","y",Trigger.Mods)) # Make a list of the modelled trigger species
        splistbase <- splistbase[, Trigger.List]
        splistbase[is.na(splistbase)] <- 0
  
          ## Loop through the different GCMs
          GCM_type <- lapply(GCMs, function(g){ 
            print(g)
            GCM <- g
            
            ## Get the future matrix
            fut_name <- paste0(SDM,"_",GCM, "_", RCP, "_50_", Thres, ".RData")
            splistfut <- get(load(paste0(matrix_path,fut_name)))
            splistfut <- splistfut[, Base.List]
            
            ## Subset by trigger species
            splistfut <- splistfut[, Trigger.List]
            splistfut[is.na(splistfut)] <- 0
    
              ## Extract species lists 
              Species_list <- lapply(numberlist[1:10],function(x){
                print(x)
                polyname <- polygonlist[[x]]$name
                polyname <- strsplit(polyname,split="/",fixed=T)[[1]][1]
                polycountry <-polygonlist[[x]]$country
                polyID <-polygonlist[[x]]$SitRecID
                if(length(polyID)==0){polyID <- "missing"}
                onepoly <-  polygonlist[[x]]$data
  
                baselist <- merge(onepoly,splistbase,by=c("x","y"),all.x=TRUE)
                l <- ncol(baselist)
                baselistsum <- colSums(baselist[,5:l])
                baselistsum <- as.data.frame(baselistsum)
                colnames(baselistsum) <- c("pres")
                baselistsum <- subset(baselistsum,pres > 0)
                currentlist <- rownames(baselistsum)
                length(currentlist)
  
                futlist <- merge(onepoly,splistfut,by=c("x","y"))
                l <- ncol(futlist)
                futlistsum <- colSums(futlist[,5:l])
                futlistsum <- as.data.frame(futlistsum)
                colnames(futlistsum) <- c("pres")
                futlistsum <- subset(futlistsum,pres > 0)
                futurelist <- rownames(futlistsum)
  
                n <- max(length(currentlist), length(futurelist))
                length(currentlist) <- n                      
                length(futurelist) <- n
  
                comblist <- cbind(currentlist,futurelist)
                print(head(comblist))
                rsave(comblist,file=paste0(outpath,polyname,"_Splist_",SDM,"_",GCM,"_trigger_species_",RCP,"_2050_",Thres,".Rdata"),compress="xz")
              
                }) # Species_list close
    
          }) # SDM close

      }) # RCP close
  
    }) # Thres close
    
})


