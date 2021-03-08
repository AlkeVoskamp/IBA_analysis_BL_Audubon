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
library(dplyr)
library(data.table)


#-#-# Set file paths #-#-#
species_lists <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_species_changes_lists/"
outpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_management_classes/"

#-#-# List all scenarioes #-#-#
All_scenarioes <- list.files(species_lists)

# #-#-# Create ensemble scenarioes for plot #-#-#
# RCP <- c("rcp26","rcp45","rcp85")
# Thres <- c("TSS","MaxKap")
# species <- c("all", "trigger")
# 
# RCPs <- lapply(RCP, function(r){
# 
#   RCP_scenario <- r
#   print(RCP_scenario)
# 
#   One_rcp <- grep(All_scenarioes, pattern = r)
#   One_rcp <- All_scenarioes[c(One_rcp)]
# 
#     Threshold <- lapply(Thres, function(t){
# 
#       Thres_scenario <- t
#       print(Thres_scenario)
# 
#       One_thres <- grep(One_rcp, pattern = t)
#       One_thres <- One_rcp[c(One_thres)]
# 
#         Species <- lapply(species, function(s){
# 
#           Species_scenario <- s
#           print(Species_scenario)
# 
#           One_species <- grep(One_thres, pattern = Species_scenario)
#           One_species <- One_thres[c(One_species)]
# 
#             Get_data <- lapply(One_species, function(d){
#               print(d)
#               df <- read.csv(paste0(species_lists, d))
#               return(df)
#             })
# 
#   DF <- do.call(cbind, Get_data)
# 
#   DT <- data.table(DF)
# 
#   DT_comb <- DT[,.(currentRich=mean(currentRich, na.rm=T),
#                    futureRich=mean(futureRich, na.rm=T),
#                    emmigrants=mean(emmigrants,na.rm=T),
#                    colonists=mean(colonists,na.rm=T),
#                    stablesp=mean(stablesp,na.rm=T)),by=c("IBA","ID","country")]
#   DT_comb <- as.data.frame(DT_comb)
# 
#   write.csv(DT_comb,paste0(species_lists,"IBA_",Species_scenario,"_species_changes_Ensemble_",RCP_scenario,"_2050_",Thres_scenario,".csv"))
#   })
#  })
# })

# #-#-# Read in the ensemble files #-#-#
# All_scenarioes <- list.files(species_lists, pattern = "Ensemble")

#-#-# Calculate the proportion of colonists and emmigrants #-#-#
Calc_manag <- lapply(All_scenarioes, function(m){
  
  print(m)
  Scenario_name <- m
  
  #-#-# Load the data and calculate proportion colonists and emigrants #-#-#
  IBAdata <- read.csv(paste0(species_lists,m),stringsAsFactors=FALSE)
  IBAdata$Site_ID <- c(1:nrow(IBAdata))
  IBAdata_orig <- IBAdata[c("Site_ID", "IBA")]
  IBAdata <- subset(IBAdata,currentRich > 0) 
  
  #-#-# Calculate the proportion of colonists and emmigrants #-#-#
  IBAdata$AllMoving <- rowSums(IBAdata[7:9])
  IBAdata$propEm <- IBAdata$emmigrants/(IBAdata$currentRich)
  IBAdata$propCol <- IBAdata$colonists/(IBAdata$currentRich) + 1
  
  yMedian <- median(IBAdata$propEm)
  xMedian <- median(log(IBAdata$propCol))
  yIQ1 <- quantile(IBAdata$propEm)[2] 
  yIQ3 <- quantile(IBAdata$propEm)[4] 
  xIQ1 <- quantile(log(IBAdata$propCol))[2] 
  xIQ3 <- quantile(log(IBAdata$propCol))[4] 
  xmax <- max(IBAdata$propCol)
  ymax <- max(IBAdata$propEm)
  
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
  IBApoints <- IBAdata[c("Site_ID", "IBA", "propEm", "propCol")]
  colnames(IBApoints) <- c("Site_ID", "IBA", "y", "x")
  IBApoints$x <- log(IBApoints$x)
  IBApoints <- as.data.frame(IBApoints)
  IBApoints <- IBApoints[c("Site_ID", "IBA", "x", "y")]
  str(IBApoints)
  head(IBApoints)
  
  #-#-# Prepaire IBA xy only dataframe #-#-#
  IBAxy <- IBApoints[c("x", "y")]
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
  IBApointsABCDE <- IBApointsABCDE[c("Site_ID", "IBA", "ClassE", "ClassD", "ClassC", "ClassB", "ClassA")]
  
  IBAdata <- merge(IBAdata_orig, IBAdata, by = "Site_ID", all.x =T)
  IBAdata <- IBAdata[c("Site_ID", "IBA.x", "country", "currentRich", "futureRich", "emmigrants", "colonists", "stablesp", "AllMoving", "propEm", "propCol")]
  colnames(IBAdata) <- c("Site_ID", "IBA", "country", "currentRich", "futureRich", "emmigrants", "colonists", "stablesp", "AllMoving", "propEm", "propCol")
  IBAclasses <- merge(IBAdata,IBApointsABCDE, by = "Site_ID", all.x=T)
  IBAclasses <- IBAclasses[c("Site_ID", "IBA.x", "country", "currentRich", "futureRich", "emmigrants", "colonists", "stablesp", "AllMoving",
                             "propEm", "propCol", "ClassE", "ClassD", "ClassC", "ClassB", "ClassA")]
  colnames(IBAclasses) <- c("Site_ID", "IBA", "country", "currentRich", "futureRich", "emmigrants", "colonists", "stablesp", "AllMoving",
                            "propEm", "propCol", "ClassE", "ClassD", "ClassC", "ClassB", "ClassA") 
  
  IBAclasses$Category <- 0
  IBAclasses$Category[IBAclasses$ClassE == 1] <- "Increasing diversification"
  IBAclasses$Category[IBAclasses$ClassD == 1] <- "Increasing specialization"
  IBAclasses$Category[IBAclasses$ClassC == 1] <- "High turnover"
  IBAclasses$Category[IBAclasses$ClassB == 1] <- "Increasing value"
  IBAclasses$Category[IBAclasses$ClassA == 1] <- "High persistence"
  
  write.csv(IBAclasses,paste0(outpath,"Management_classes_", Scenario_name))
})


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
## These are needed to plot the uncertainty around the allocated management class based on SDM and GCM types
#-#-# Summarize the Mgt class information for ensembles #-#-# 
Mgt_classes <- intersect(list.files(outpath, pattern = "IBA_trigger"), list.files(outpath, pattern = "_MaxKap.csv"))
#Mgt_classes <- Mgt_classes[4:39]

summarized_outpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_management_classes_summarized/"
RCPs <- c("rcp26", "rcp45", "rcp85")

Summarize <- lapply(RCPs, function(r){
  
  All_files <- grep(Mgt_classes, pattern = r)
  All_files <- Mgt_classes[c(All_files)]
  
  RCP <- r
  
  print(All_files[1])
  EnsembleFile <- read.csv(paste0(outpath,All_files[1]))
  
  One_rcp <- lapply(All_files[2:length(All_files)], function(o){

    Name <- o
    Scenario <- paste0(strsplit(Name, split = "_")[[1]][7],"_",strsplit(Name, split = "_")[[1]][8],"_",strsplit(Name, split = "_")[[1]][9])
    print(Scenario)
    File <- read.csv(paste0(outpath, Name))
    print(nrow(File))

    File$MgtCat[File$ClassA == 1] <- "A"
    File$MgtCat[File$ClassB == 1] <- "B"
    File$MgtCat[File$ClassC == 1] <- "C"
    File$MgtCat[File$ClassD == 1] <- "D"
    File$MgtCat[File$ClassE == 1] <- "E"
    
    File <- File[c("IBA", "MgtCat")] 
    colnames(File) <- c("IBA", Scenario) 
    print(nrow(File))
    return(File)
  })
  
  #RCP_scenario <- Reduce(function(x,y) merge(x,y,by="IBA",all=TRUE) ,One_rcp)
  RCP_scenario <- do.call(cbind, One_rcp)
  
  IBA_names <- as.data.frame(RCP_scenario[1])
  RCP_scenarioes <- RCP_scenario[c(paste0("GAM_CCSM4_", RCP), paste0("GAM_GFDLCM3_", RCP), paste0("GAM_HadGEM2ES_", RCP),
                                   paste0("GBM_CCSM4_", RCP), paste0("GBM_GFDLCM3_", RCP), paste0("GBM_HadGEM2ES_", RCP),
                                   paste0("GLM_CCSM4_", RCP), paste0("GLM_GFDLCM3_", RCP), paste0("GLM_HadGEM2ES_", RCP),
                                   paste0("RF_CCSM4_", RCP), paste0("RF_GFDLCM3_", RCP), paste0("RF_HadGEM2ES_", RCP))]
  RCP_scenario <- cbind(IBA_names, RCP_scenarioes)
  
    #-#-# Add max number of same mgt class per row #-#-#
    Variance_mgt <- lapply(1:nrow(RCP_scenario), function(mgt){
     
      One_IBA <- RCP_scenario[mgt,]
      Classes <- as.data.frame(t(One_IBA[2:ncol(One_IBA)]))
      colnames(Classes) <- c("MGT")
      A <- nrow(na.omit(subset(Classes, MGT == "A")))
      B <- nrow(na.omit(subset(Classes, MGT == "B")))
      C <- nrow(na.omit(subset(Classes, MGT == "C")))
      D <- nrow(na.omit(subset(Classes, MGT == "D")))
      E <- nrow(na.omit(subset(Classes, MGT == "E")))
      
      MGT_frame <- data.frame(matrix(ncol = , nrow = 5))
      MGT_frame <- as.data.frame(rbind(A,B,C,D,E))
      
      Mgt_cat <- rownames(MGT_frame)
      rownames(MGT_frame) <- NULL
      Mgt_data <- cbind(Mgt_cat,MGT_frame)
      
      colnames(Mgt_data) <- c("Mgt_cat","MGT")
      Main_class_count <- max(Mgt_data$MGT)
      #Main_class <- subset(Mgt_data, MGT == Main_class_count)
      #Main_class <- Main_class$Mgt_cat[1] # Could opt to adjust this here to select the most likely class in case of a 50/50 dist
      
      Summed_data <- as.data.frame(Main_class_count)
      colnames(Summed_data) <- c("Main_class_count")
      return(Summed_data)
    })
  
  Final_count<- do.call(rbind,Variance_mgt)
  Combined_scenario <- cbind(EnsembleFile,Final_count)
  
  write.csv(Combined_scenario,paste0(summarized_outpath, "Mgt_classes_summarized_","trigger_",RCP,"_MaxKap.csv"))
  
})


