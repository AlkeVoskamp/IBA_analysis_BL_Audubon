rm(list=ls())

#-#-# Load libraries
library(fields)
library(RColorBrewer)
library(colorRamps)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(matrixStats)
library(tidyverse)

#-#-# Set filepaths #-#-#
matrixpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Data/BL_matrixes/"
datapath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Data/"


#-#-# Proccess the input data - summarizing species matrixes into SR dataframes #-#-#
All_matrixes <- list.files(matrixpath, pattern = "_TSS.RData")


#-#-# Get the trigger species list if subsetting by trigger species #-#-#
Trigger <- get(load(paste0(datapath,"IBA trigger species.Rdata")))


#-#-# Get the SR values #-#-#
GetSR <- lapply(All_matrixes,function(s){
  
  ## File name
  Name <- strsplit(s,".RData")[[1]][1]
  print(Name)
  
  ## Read the data in
  One_matrix <- get(load(paste0(matrixpath,s)))
  
  ## Subset by trigger species if needed
  # One_matrix_trigger <- names(One_matrix)[(names(One_matrix) %in% Trigger)] # Check which trigger species were modelled
  # Trigger.List <- as.vector(c("x","y",One_matrix_trigger)) # Make a list of the modelled trigger species
  # One_matrix <- One_matrix[, Trigger.List]
  
  ## Extract cell wise SR
  One_matrix$SR <-rowSums(One_matrix[3:ncol(One_matrix)], na.rm=T)
  One_matrix <- One_matrix[c("x","y","SR")]
  colnames(One_matrix) <- c("x","y",Name)
  return(One_matrix)
})

SR_matrix <- Reduce(function(...) merge(..., all=T), GetSR)
head(SR_matrix)

write.csv(SR_matrix, file = paste0(matrixpath, "Species_richness_all_TSS.csv"))


#-#-# Read matrixes back in for plotting #-#-#
SR_data <- read.csv(paste0(matrixpath, "Species_richness_all_TSS.csv"))
Coordinates <- read.csv(paste0(datapath, "terrestrial_coords.csv"))
head(Coordinates)
SR_data <- merge(Coordinates, SR_data, by = c("x", "y"), all.x = T)
head(SR_data)

## Baseline projections
Base <- SR_data[c("x","y","GAM_baseline_TSS", "GBM_baseline_TSS", "GLM_baseline_TSS", "RF_baseline_TSS")]  
Base$Sd <- rowSds(as.matrix(Base[3:ncol(Base)]))
head(Base)

levelplot(Sd~x*y,data=Base)
levelplot(GAM_baseline_TSS~x*y,data=Base)
levelplot(GLM_baseline_TSS~x*y,data=Base)
levelplot(GBM_baseline_TSS~x*y,data=Base)
levelplot(RF_baseline_TSS~x*y,data=Base)

## Future projections
Future_rcp26 <- SR_data[c("x", "y", "GAM_CCSM4_rcp26_50_TSS", "GAM_GFDLCM3_rcp26_50_TSS", "GAM_HadGEM2ES_rcp26_50_TSS",
                          "GBM_CCSM4_rcp26_50_TSS", "GBM_GFDLCM3_rcp26_50_TSS", "GBM_HadGEM2ES_rcp26_50_TSS",
                          "GLM_CCSM4_rcp26_50_TSS", "GLM_GFDLCM3_rcp26_50_TSS", "GLM_HadGEM2ES_rcp26_50_TSS",
                          "RF_CCSM4_rcp26_50_TSS", "RF_GFDLCM3_rcp26_50_TSS", "RF_HadGEM2ES_rcp26_50_TSS")] 
Future_rcp26$Sd <- rowSds(as.matrix(Future_rcp26[3:ncol(Future_rcp26)]))
head(Future_rcp26)

Future_rcp45 <- SR_data[c("x", "y", "GAM_CCSM4_rcp45_50_TSS", "GAM_GFDLCM3_rcp45_50_TSS", "GAM_HadGEM2ES_rcp45_50_TSS",
                          "GBM_CCSM4_rcp45_50_TSS", "GBM_GFDLCM3_rcp45_50_TSS", "GBM_HadGEM2ES_rcp45_50_TSS",
                          "GLM_CCSM4_rcp45_50_TSS", "GLM_GFDLCM3_rcp45_50_TSS", "GLM_HadGEM2ES_rcp45_50_TSS",
                          "RF_CCSM4_rcp26_50_TSS", "RF_GFDLCM3_rcp26_50_TSS", "RF_HadGEM2ES_rcp26_50_TSS")] # "RF_CCSM4_rcp26_50_TSS","RF_GFDLCM3_rcp26_50_TSS","RF_HadGEM2ES_rcp26_50_TSS"
Future_rcp45$Sd <- rowSds(as.matrix(Future_rcp45[3:ncol(Future_rcp45)]))
head(Future_rcp45)

Future_rcp85 <- SR_data[c("x", "y", "GAM_CCSM4_rcp85_50_TSS", "GAM_GFDLCM3_rcp85_50_TSS", "GAM_HadGEM2ES_rcp85_50_TSS",
                          "GBM_CCSM4_rcp85_50_TSS", "GBM_GFDLCM3_rcp85_50_TSS", "GBM_HadGEM2ES_rcp85_50_TSS",
                          "GLM_CCSM4_rcp85_50_TSS", "GLM_GFDLCM3_rcp85_50_TSS", "GLM_HadGEM2ES_rcp85_50_TSS",
                          "RF_CCSM4_rcp26_50_TSS", "RF_GFDLCM3_rcp26_50_TSS", "RF_HadGEM2ES_rcp26_50_TSS")] # "RF_CCSM4_rcp26_50_TSS","RF_GFDLCM3_rcp26_50_TSS","RF_HadGEM2ES_rcp26_50_TSS"
Future_rcp85$Sd <- rowSds(as.matrix(Future_rcp85[3:ncol(Future_rcp85)]))
head(Future_rcp85)

#-#-# Plot the data #-#-#
GAM_baseline_TSS <- as.data.frame(table(Base$GAM_baseline_TSS))
colnames(GAM_baseline_TSS) <- c("Species_richness", "GAM_baseline_TSS")
head(GAM_baseline_TSS)
GLM_baseline_TSS <- as.data.frame(table(Base$GLM_baseline_TSS))
colnames(GLM_baseline_TSS) <- c("Species_richness", "GLM_baseline_TSS")
head(GLM_baseline_TSS)
GBM_baseline_TSS <- as.data.frame(table(Base$GBM_baseline_TSS))
colnames(GBM_baseline_TSS) <- c("Species_richness", "GBM_baseline_TSS")
head(GBM_baseline_TSS)
RF_baseline_TSS <- as.data.frame(table(Base$RF_baseline_TSS))
colnames(RF_baseline_TSS) <- c("Species_richness", "RF_baseline_TSS")
head(RF_baseline_TSS)


## Combine frames keeping all SR values
BaseList <- c(GAM_baseline_TSS, GLM_baseline_TSS, GBM_baseline_TSS, RF_baseline_TSS)
All <- merge(GAM_baseline_TSS, GLM_baseline_TSS, by = "Species_richness", all.x = T, all.y =T)
All <- merge(All, GBM_baseline_TSS, by = "Species_richness", all.x = T, all.y =T)
All <- merge(All, RF_baseline_TSS, by = "Species_richness", all.x = T, all.y =T)
head(All)
  
ggplot(All, aes(x = Species_richness, y = GAM_baseline_TSS)) +
  geom_line(group = 1, colour = "red") +
  geom_line(aes(x=Species_richness, y=GBM_baseline_TSS), group = 1, colour = "blue") +
  geom_line(aes(x=Species_richness, y=GLM_baseline_TSS), group = 1, colour = "green") +
  geom_line(aes(x=Species_richness, y=RF_baseline_TSS), group = 1, colour = "black") +
  scale_x_discrete(breaks = c("100","200","300","400","500","600","702","800"),
                   labels = c("100","200","300","400","500","600","700","800"))
  




#-#-# Plot the data #-#-#
colPal <- rev(brewer.pal(11,"Spectral"))[-c(3)]
hist(na.omit(CurrentData$SR))
min(na.omit(CurrentData$SR))
max(na.omit(CurrentData$SR))
hist(na.omit(FutureData$SR))
min(na.omit(FutureData$SR))
max(na.omit(FutureData$SR)) #149

## Current SR
SRplot<- ggplot(data=CurrentData, aes(y=y, x=x)) +
  geom_raster(aes(fill =  SR), stat = "identity", position = "identity")+
  scale_fill_gradientn("Species\nrichness",colours=colPal,limits=c(1,600),na.value = "white")+ # Insert colour and set range #-72,100
  borders("world",  xlim = c(-180, -20), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=0,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = c(0.1, 0.2))+ # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("a)")+ 
  theme(plot.title = element_text(size = 14,face="bold",hjust = 0))

print(SRplot)

CurrentData <- subset(CurrentData,SR >0) # if cur dataframe has seacells
## Current SR histogram
Curhist <- ggplot(CurrentData, aes(x=SR)) +
  geom_histogram(position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=c("white","black", "grey40", "grey83")) +
  scale_colour_manual(values=c("black","black","black","black")) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16))+ # Change font size legend
  theme(axis.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="# of species", y="# of grid cells", title="")+ # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("(c)")+ 
  theme(plot.title = element_text(size = 14,face="bold",hjust = 0))
Curhist

FutureData <- subset(FutureData,SR >0)
## Future SR
SRplotFut<- ggplot(data=FutureData, aes(y=y, x=x)) +
  geom_raster(aes(fill =  SR), stat = "identity", position = "identity")+
  scale_fill_gradientn("Species\n%richness",colours=colPal,limits=c(1,600),na.value = "white")+ # Insert colour and set range #-72,100
  borders("world",  xlim = c(-180, -20), ylim = c(-60,90), lwd=0.3, colour ="black")+
  geom_segment(aes(x=-180,xend=10,y=0,yend=0),colour="black",linetype="dashed")+ #Add shortened equator line 
  theme(legend.position = "none") + # Positioning the legend 
  theme(axis.title=element_text(size=28))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="", title="")+ # Remove axis titles
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # Get rid of axis ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  ggtitle("b)")+ 
  theme(plot.title = element_text(size = 14,face="bold",hjust = 0))

print(SRplotFut)


## Future SR histogram
Futhist <- ggplot(FutureData, aes(x=SR)) +
  geom_histogram(position = position_dodge2(width = 10, preserve = "single")) +
  scale_fill_manual(values=c("white","black", "grey40", "grey83")) +
  scale_colour_manual(values=c("black","black","black","black")) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16))+ # Change font size legend
  theme(axis.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="# of species", y="# of grid cells", title="") +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("(d)")+ 
  theme(plot.title = element_text(size = 14,face="bold",hjust = 0))
Futhist


#-#-# Combine maps #-#-#
CombMap <- arrangeGrob(SRplot,SRplotFut,Curhist,Futhist,
                       widths = c(2,2),
                       heights = c(8,4),
                       ncol = 2,
                       nrow = 2)



plot(CombMap)

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Manuscript drafts/Submission/Final figures/")
ggsave("Species richness maps RCP 45 habitat.tiff",CombMap,width=8, height=6, unit="in", dpi=300, bg="transparent")

