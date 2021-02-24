#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#        Calculate the adaptation management classes          #
#     following the methods of Hole et al 2011 Cons Biol      #
#                       September 2020                        #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 


#-#-# Load libraries #-#-#
library(sp)
library(mgcv)


#-#-# Set file paths #-#-#
species_lists <- "/Volumes/AG KBG 1/BL_exchange/IBA_species_changes_lists/"
outpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_management_classes/"


#-#-# List all scenarioes #-#-#
All_scenarioes <- list.files(species_lists)


#-#-# Calculate the proportion of colonists and emmigrants #-#-#
Calc_manag <- lapply(All_scenarioes, function(m){
  
  print(m)
  Scenario_name <- m
  
  #-#-# Load the data and calculate proportion colonists and emigrants #-#-#
  IBAdata <- read.csv(paste0(species_lists,m),stringsAsFactors=FALSE)

  Site_ID <- c(1:1653)
  IBAdata <- cbind(Site_ID,IBAdata)
  IBAdata <- IBAdata[c("Site_ID", "IBA", "country", "currentRich", "futureRich", "emmigrants", "colonists", "stablesp")]
  
  IBAdata$AllMoving <- rowSums(IBAdata[c("emmigrants", "colonists", "stablesp")], na.rm = T)
  IBAdata$propEm <- IBAdata$emmigrants/(IBAdata$currentRich)
  IBAdata$propCol <- IBAdata$colonists/(IBAdata$currentRich) + 1
  is.na(IBAdata) <- sapply(IBAdata, is.infinite)

  #-#-# Set the medians and quantiles #-#-#
  yMedian <- median(na.omit(IBAdata$propEm))
  xMedian <- median(log(na.omit(IBAdata$propCol)))
  yIQ1 <- quantile(IBAdata$propEm, na.rm = T)[2] 
  yIQ3 <- quantile(IBAdata$propEm, na.rm = T)[4] 
  xIQ1 <- quantile(log(IBAdata$propCol), na.rm = T)[2] 
  xIQ3 <- quantile(log(IBAdata$propCol), na.rm = T)[4] 
  xmax <- max(IBAdata$propCol, na.rm = T)
  ymax <- max(IBAdata$propEm, na.rm = T)
  
  #-#-# Points center polygon #-#-#
  P1 <- c(xIQ1,yMedian)
  P2 <- c(xMedian,yIQ3)
  P3 <- c(xIQ3,yMedian)
  P4 <- c(xMedian,yIQ1)
  
  #-#-# Other Points #-#-#
  P5 <- c(0,yMedian)
  P6 <- c(0,ymax)
  P7 <- c(xMedian,ymax)
  P8 <- c(xmax,ymax)
  P9 <- c(xmax,yMedian)
  P10 <- c(xmax,0)
  P11 <- c(xMedian,0)
  P12 <- c(0,0)
  
  #-#-# Prepaire IBA point dataframe #-#-#
  IBApoints <- IBAdata[c("Site_ID", "propEm", "propCol")]
  colnames(IBApoints) <- c("Site_ID","y","x")
  IBApoints$x <- log(IBApoints$x)
  IBApoints <- as.data.frame(na.omit(IBApoints))
  
  #-#-# Prepaire IBA xy only dataframe #-#-#
  IBAxy <- IBApoints[c(2:3)]
  IBAxy$x <- as.numeric(as.vector(IBAxy$x))
  IBAxy <- SpatialPoints(IBAxy)
  
  #-#-# Select points for polygon E #-#-#
  PointsE <- as.data.frame(do.call("rbind", list(P1,P2,P3,P4)))
  colnames(PointsE) <- c("x","y")
  
  #-#-# Make polygon E #-#-#
  PolygonE = Polygon(PointsE)
  PolygonE = Polygons(list(PolygonE),1)
  PolygonE = SpatialPolygons(list(PolygonE))
  plot(PolygonE)
  
  #-#-# Check which points lie within polygon E #-#-#
  ClassE <- over(IBAxy,PolygonE)
  IBApointsE <- cbind(IBApoints,ClassE)
  
  #-#-# Select points for polygon D #-#-#
  PointsD <- as.data.frame(do.call("rbind", list(P5,P6,P7,P2,P1)))
  colnames(PointsD) <- c("x","y")
  
  #-#-# Make polygon D #-#-#
  PolygonD = Polygon(PointsD)
  PolygonD = Polygons(list(PolygonD),1)
  PolygonD = SpatialPolygons(list(PolygonD))
  plot(PolygonD)
  
  #-#-# Check which points lie within polygon D #-#-#
  ClassD <- over(IBAxy,PolygonD)
  IBApointsDE <- cbind(IBApointsE,ClassD)
  
  #-#-# Select points for polygon C #-#-#
  PointsC <- as.data.frame(do.call("rbind", list(P7,P2,P3,P9,P8)))
  colnames(PointsC) <- c("x","y")
  
  #-#-# Make polygon C #-#-#
  PolygonC = Polygon(PointsC)
  PolygonC = Polygons(list(PolygonC),1)
  PolygonC = SpatialPolygons(list(PolygonC))
  plot(PolygonC)
  
  #-#-# Check which points lie within polygon C #-#-#
  ClassC <- over(IBAxy,PolygonC)
  IBApointsCDE <- cbind(IBApointsDE,ClassC)
  
  #-#-# Select points for polygon B #-#-#
  PointsB <- as.data.frame(do.call("rbind", list(P4,P3,P9,P10,P11)))
  colnames(PointsB) <- c("x","y")
  
  #-#-# Make polygon B #-#-#
  PolygonB = Polygon(PointsB)
  PolygonB = Polygons(list(PolygonB),1)
  PolygonB = SpatialPolygons(list(PolygonB))
  plot(PolygonB)
  
  #-#-# Check which points lie within polygon B #-#-#
  ClassB <- over(IBAxy,PolygonB)
  IBApointsBCDE <- cbind(IBApointsCDE,ClassB)
  
  #-#-# Select points for polygon A #-#-#
  PointsA <- as.data.frame(do.call("rbind", list(P12,P5,P1,P4,P11)))
  colnames(PointsA) <- c("x","y")
  
  #-#-# Make polygon A #-#-#
  PolygonA = Polygon(PointsA)
  PolygonA = Polygons(list(PolygonA),1)
  PolygonA = SpatialPolygons(list(PolygonA))
  plot(PolygonA)
  
  #-#-# Check which points lie within polygon A #-#-#
  ClassA <- over(IBAxy,PolygonA)
  IBApointsABCDE <- cbind(IBApointsBCDE,ClassA)
  
  IBApointsABCDE[is.na(IBApointsABCDE)] <- 0
  IBApointsABCDE <- IBApointsABCDE[c("Site_ID", "ClassE", "ClassD", "ClassC", "ClassB", "ClassA")]
  
  IBAclasses <- merge(IBAdata,IBApointsABCDE, by = "Site_ID", all.x = T)
  
  write.csv(IBAclasses,paste0(outpath,"Management_classes_", Scenario_name))
})


#-#-# Summarize the Mgt class informantion #-#-#
Mgt_classes <- intersect(list.files(outpath, pattern = "IBA_all"), list.files(outpath, pattern = "_MaxKap.csv"))
summarized_outpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_management_classes_summarized/"
RCPs <- c("rcp26", "rcp45", "rcp85")

Summarize <- lapply(RCPs, function(r){
  
  All_files <- grep(Mgt_classes, pattern = r)
  RCP <- r
  
  One_rcp <- lapply(All_files, function(o){
    
    Name <- Mgt_classes[o]
    Scenario <- paste0(strsplit(Name, split = "_")[[1]][7],"_",strsplit(Name, split = "_")[[1]][8],"_",strsplit(Name, split = "_")[[1]][9])
    print(Scenario)
    File <- read.csv(paste0(outpath, Name))
    File$MgtCat
    
    File$MgtCat[File$ClassA == 1] <- "A"
    File$MgtCat[File$ClassB == 1] <- "B"
    File$MgtCat[File$ClassC == 1] <- "C"
    File$MgtCat[File$ClassD == 1] <- "D"
    File$MgtCat[File$ClassE == 1] <- "E"
    
    File <- File[c("IBA", "MgtCat")] #"country",
    colnames(File) <- c("IBA", Scenario) #"country",
    print(nrow(File))
    return(File)
  })
  
  RCP_scenario <- do.call(cbind, One_rcp)
  IBA_names <- as.data.frame(RCP_scenario[1])
  RCP_scenarioes <- RCP_scenario[c(paste0("GAM_CCSM4_", RCP), paste0("GAM_GFDLCM3_", RCP), paste0("GAM_HadGEM2ES_", RCP),
                                   paste0("GBM_CCSM4_", RCP), paste0("GBM_GFDLCM3_", RCP), paste0("GBM_HadGEM2ES_", RCP),
                                   paste0("GLM_CCSM4_", RCP), paste0("GLM_GFDLCM3_", RCP), paste0("GLM_HadGEM2ES_", RCP),
                                   paste0("RF_CCSM4_", RCP), paste0("RF_GFDLCM3_", RCP), paste0("RF_HadGEM2ES_", RCP))]
  RCP_scenario <- cbind(IBA_names, RCP_scenarioes)
  
    ## Add max number of same mgt class per row
    Variance_mgt <- lapply(1:nrow(RCP_scenario), function(mgt){
      #print(mgt)
     
      One_IBA <- RCP_scenario[mgt,]
      Classes <- as.data.frame(t(One_IBA[3:ncol(One_IBA)]))
      colnames(Classes) <- c("MGT")
      A <- nrow(subset(Classes, MGT == "A"))
      B <- nrow(subset(Classes, MGT == "B"))
      C <- nrow(subset(Classes, MGT == "C"))
      D <- nrow(subset(Classes, MGT == "D"))
      E <- nrow(subset(Classes, MGT == "E"))
      
      MGT_frame <- data.frame(matrix(ncol = , nrow = 5))
      MGT_frame <- as.data.frame(rbind(A,B,C,D,E))
      
      Mgt_cat <- rownames(MGT_frame)
      rownames(MGT_frame) <- NULL
      Mgt_data <- cbind(Mgt_cat,MGT_frame)
      
      colnames(Mgt_data) <- c("Mgt_cat","MGT")
      Main_class_count <- max(Mgt_data$MGT)
      Main_class <- subset(Mgt_data, MGT == Main_class_count)
      Main_class <- Main_class$Mgt_cat[1] # Could opt to adjust this here to select the most likely class in case of a 50/50 dist
      
      Summed_data <- as.data.frame(cbind(as.character(Main_class),Main_class_count))
      colnames(Summed_data) <- c("Main_class", "Main_class_count")
      return(Summed_data)
    })
  
  Final_cat <- do.call(rbind,Variance_mgt)
  Combined_scenario <- cbind(RCP_scenario,Final_cat)
  
  write.csv(RCP_scenario,paste0(summarized_outpath, "Mgt_classes_summarized_","all_",RCP,"_MaxKap.csv"))
  
})


