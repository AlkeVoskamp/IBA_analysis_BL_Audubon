#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#            Summarize species richness values across           #
#         all scenarioes from presence/absence matrixes         #
#             Plot species richness values FIGURE 2             #
#                          October 2020                         #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls())


#-#-# Load libraries
#library(fields)
library(RColorBrewer)
library(colorRamps)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(matrixStats)
library(tidyverse)


#-#-# Set filepaths #-#-#
matrixpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/BL_matrixes/"
datapath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Data/"
splistpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/"
outpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/Plot_files/SR_matrixes/"


#-#-# Proccess the input data - summarizing species matrixes into SR dataframes #-#-#
All_matrixes <- list.files(matrixpath, pattern = "_MaxKap.RData")


#-#-# Get the species lists #-#-#
Good_mods <- read.csv(paste0(splistpath, "SDMs with high AUC all species.csv"))
Good_mods <- as.vector(Good_mods$Species)
Trigger <- get(load(paste0(datapath,"IBA trigger species.Rdata")))


#-#-# Get the SR values #-#-#
GetSR <- lapply(All_matrixes,function(s){

  ## File name
  Name <- strsplit(s,".RData")[[1]][1]
  print(Name)

  Model <- strsplit(Name, "_")[[1]][1]
  SDM_name <- paste0("_", Model)

  ## Read the data in
  One_matrix <- get(load(paste0(matrixpath,s)))
  
  ## Subset to species with high performing models
  basenames <- colnames(One_matrix[3:ncol(One_matrix)])
  basenames <- lapply(basenames, function(n){
    name <- strsplit(n, split = SDM_name)[[1]][1]
    print(name)
    return(name)
  })
  Base.names <- do.call(rbind,basenames)
  Base.names <- Base.names[(Base.names %in% Good_mods)] 

  ## Subset by trigger species if needed
  # Trigger.names <- Base.names[(Base.names %in% Trigger)]
  # Trigger_model <- lapply(Trigger.names, function(x){ #Change here for trigger or all species
  #   Trigger_name <- paste0(x, "_", Model)
  # return(Trigger_name)})
  # Trigger_model <- do.call(rbind, Trigger_model)
  # Trigger.List <- as.vector(c("x","y",Trigger_model)) # Make a list of the modelled trigger species
  # One_matrix <- One_matrix[, Trigger.List]
  
  ## Or run for all species 
  Base_model <- lapply(Base.names, function(x){ #Change here for trigger or all species
    Base_name <- paste0(x, "_", Model)
    return(Base_name)})
  Base_model <- do.call(rbind, Base_model)
  Base.List <- as.vector(c("x","y",Base_model))
  One_matrix <- One_matrix[, Base.List]

  ## Extract cell wise SR
  One_matrix$SR <-rowSums(One_matrix[3:ncol(One_matrix)], na.rm=T)
  One_matrix <- One_matrix[c("x","y","SR")]
  colnames(One_matrix) <- c("x","y",Name)
  return(One_matrix)
})

SR_matrix <- Reduce(function(...) merge(..., all=T), GetSR)
head(SR_matrix)

write.csv(SR_matrix, file = paste0(matrixpath, "Species_richness_all_MaxKap.csv"))


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#


#-#-# Plot species richness figures #-#-#
#-#-# Set filepaths #-#-#
SRpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/Plot_files/SR_matrixes/"


#-#-# TSS plots #-#-#
#-#-# Read matrixes back in for plotting #-#-#
SR_data <- read.csv(paste0(SRpath, "Species_richness_trigger_TSS.csv"))
Coordinates <- read.csv(paste0(datapath, "terrestrial_coords.csv"))
Coordinates <- subset(Coordinates, x <= -20)
head(Coordinates)
SR_data <- merge(Coordinates, SR_data, by = c("x", "y"), all.x = T)
head(SR_data)

## Baseline projections
Base <- SR_data[c("x","y","GAM_baseline_TSS", "GBM_baseline_TSS", "GLM_baseline_TSS", "RF_baseline_TSS")]  
Base$Sd <- rowSds(as.matrix(Base[3:ncol(Base)]))
Base$Ensemble <- rowMeans(Base[c("GAM_baseline_TSS", "GBM_baseline_TSS", "GLM_baseline_TSS", "RF_baseline_TSS")])
head(Base)

## Future projections
Future_rcp26 <- SR_data[c("x", "y", "GAM_CCSM4_rcp26_50_TSS", "GAM_GFDLCM3_rcp26_50_TSS", "GAM_HadGEM2ES_rcp26_50_TSS",
                          "GBM_CCSM4_rcp26_50_TSS", "GBM_GFDLCM3_rcp26_50_TSS", "GBM_HadGEM2ES_rcp26_50_TSS",
                          "GLM_CCSM4_rcp26_50_TSS", "GLM_GFDLCM3_rcp26_50_TSS", "GLM_HadGEM2ES_rcp26_50_TSS",
                          "RF_CCSM4_rcp26_50_TSS", "RF_GFDLCM3_rcp26_50_TSS", "RF_HadGEM2ES_rcp26_50_TSS")] 
Future_rcp26$Sd <- rowSds(as.matrix(Future_rcp26[3:ncol(Future_rcp26)]))
Future_rcp26$Ensemble <- rowMeans(Future_rcp26[c("GAM_CCSM4_rcp26_50_TSS", "GAM_GFDLCM3_rcp26_50_TSS", "GAM_HadGEM2ES_rcp26_50_TSS",
                                                 "GBM_CCSM4_rcp26_50_TSS", "GBM_GFDLCM3_rcp26_50_TSS", "GBM_HadGEM2ES_rcp26_50_TSS",
                                                 "GLM_CCSM4_rcp26_50_TSS", "GLM_GFDLCM3_rcp26_50_TSS", "GLM_HadGEM2ES_rcp26_50_TSS",
                                                 "RF_CCSM4_rcp26_50_TSS", "RF_GFDLCM3_rcp26_50_TSS", "RF_HadGEM2ES_rcp26_50_TSS")])

head(Future_rcp26)

Future_rcp45 <- SR_data[c("x", "y", "GAM_CCSM4_rcp45_50_TSS", "GAM_GFDLCM3_rcp45_50_TSS", "GAM_HadGEM2ES_rcp45_50_TSS",
                          "GBM_CCSM4_rcp45_50_TSS", "GBM_GFDLCM3_rcp45_50_TSS", "GBM_HadGEM2ES_rcp45_50_TSS",
                          "GLM_CCSM4_rcp45_50_TSS", "GLM_GFDLCM3_rcp45_50_TSS", "GLM_HadGEM2ES_rcp45_50_TSS",
                          "RF_CCSM4_rcp45_50_TSS", "RF_GFDLCM3_rcp45_50_TSS", "RF_HadGEM2ES_rcp45_50_TSS")] 
Future_rcp45$Sd <- rowSds(as.matrix(Future_rcp45[3:ncol(Future_rcp45)]))
Future_rcp45$Ensemble <- rowMeans(Future_rcp45[c("GAM_CCSM4_rcp45_50_TSS", "GAM_GFDLCM3_rcp45_50_TSS", "GAM_HadGEM2ES_rcp45_50_TSS",
                                                 "GBM_CCSM4_rcp45_50_TSS", "GBM_GFDLCM3_rcp45_50_TSS", "GBM_HadGEM2ES_rcp45_50_TSS",
                                                 "GLM_CCSM4_rcp45_50_TSS", "GLM_GFDLCM3_rcp45_50_TSS", "GLM_HadGEM2ES_rcp45_50_TSS",
                                                 "RF_CCSM4_rcp45_50_TSS", "RF_GFDLCM3_rcp45_50_TSS", "RF_HadGEM2ES_rcp45_50_TSS")])

head(Future_rcp45)

Future_rcp85 <- SR_data[c("x", "y", "GAM_CCSM4_rcp85_50_TSS", "GAM_GFDLCM3_rcp85_50_TSS", "GAM_HadGEM2ES_rcp85_50_TSS",
                          "GBM_CCSM4_rcp85_50_TSS", "GBM_GFDLCM3_rcp85_50_TSS", "GBM_HadGEM2ES_rcp85_50_TSS",
                          "GLM_CCSM4_rcp85_50_TSS", "GLM_GFDLCM3_rcp85_50_TSS", "GLM_HadGEM2ES_rcp85_50_TSS",
                          "RF_CCSM4_rcp85_50_TSS", "RF_GFDLCM3_rcp85_50_TSS", "RF_HadGEM2ES_rcp85_50_TSS")]
Future_rcp85$Sd <- rowSds(as.matrix(Future_rcp85[3:ncol(Future_rcp85)]))
Future_rcp85$Ensemble <- rowMeans(Future_rcp85[c("GAM_CCSM4_rcp85_50_TSS", "GAM_GFDLCM3_rcp85_50_TSS", "GAM_HadGEM2ES_rcp85_50_TSS",
                                                 "GBM_CCSM4_rcp85_50_TSS", "GBM_GFDLCM3_rcp85_50_TSS", "GBM_HadGEM2ES_rcp85_50_TSS",
                                                 "GLM_CCSM4_rcp85_50_TSS", "GLM_GFDLCM3_rcp85_50_TSS", "GLM_HadGEM2ES_rcp85_50_TSS",
                                                 "RF_CCSM4_rcp85_50_TSS", "RF_GFDLCM3_rcp85_50_TSS", "RF_HadGEM2ES_rcp85_50_TSS")])

head(Future_rcp85)

# #-# Plot the data #-#-#
colPal <- rev(brewer.pal(11,"Spectral"))[-c(3)]
min(na.omit(Base$Ensemble))
Curmax <- round(max(na.omit(Base$Ensemble)),0)
hist(na.omit(Future_rcp45$Ensemble))
min(na.omit(Future_rcp45$Ensemble))
Futmax <- round(max(na.omit(Future_rcp45$Ensemble)),0)

## Current SR
SRplot<- ggplot(data=Base, aes(y=y, x=x)) +
  geom_raster(aes(fill =  Ensemble), stat = "identity", position = "identity")+
  scale_fill_gradientn("Species\nrichness",colours=colPal,limits=c(1,Curmax),na.value = "white")+ # Insert colour and set range #-72,100
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
  
## Current SR histogram
Curhist <- ggplot(Base) +
  geom_freqpoly(aes(x=GAM_baseline_TSS), colour = "royalblue3", show.legend = T) +
  geom_freqpoly(aes(x=GBM_baseline_TSS), colour = "green3") +
  geom_freqpoly(aes(x=GLM_baseline_TSS), colour = "orangered3") +
  geom_freqpoly(aes(x=RF_baseline_TSS), colour = "gray27") +
  theme(axis.title=element_text(size=16)) + # Change font size legend
  theme(axis.text=element_text(size=12)) + # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) + # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white")) + # Remove the background
  labs(x="# of species", y="# of grid cells", title="") + # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("(c)") + 
  theme(plot.title = element_text(size = 14,face="bold",hjust = 0))
plot(Curhist)

## Future SR
SRplotFut<- ggplot(data=Future_rcp45, aes(y=y, x=x)) +
  geom_raster(aes(fill =  Ensemble), stat = "identity", position = "identity")+
  scale_fill_gradientn("Species\n%richness",colours=colPal,limits=c(1,Curmax),na.value = "white")+ # Insert colour and set range #-72,100
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
Futhist <- ggplot(Future_rcp45) +
  geom_freqpoly(aes(x=GAM_CCSM4_rcp45_50_TSS), colour = "royalblue") +
  geom_freqpoly(aes(x=GAM_GFDLCM3_rcp45_50_TSS), colour = "royalblue3") +
  geom_freqpoly(aes(x=GAM_HadGEM2ES_rcp45_50_TSS), colour = "blue4") +
  
  geom_freqpoly(aes(x=GBM_CCSM4_rcp45_50_TSS), colour = "chartreuse3") +
  geom_freqpoly(aes(x=GBM_GFDLCM3_rcp45_50_TSS), colour = "green3") +
  geom_freqpoly(aes(x=GBM_HadGEM2ES_rcp45_50_TSS), colour = "darkgreen") +
  
  geom_freqpoly(aes(x=GLM_CCSM4_rcp45_50_TSS), colour = "indianred1") +
  geom_freqpoly(aes(x=GLM_GFDLCM3_rcp45_50_TSS), colour = "orangered3") +
  geom_freqpoly(aes(x=GLM_HadGEM2ES_rcp45_50_TSS), colour = "orangered4") +
  
  geom_freqpoly(aes(x=RF_CCSM4_rcp45_50_TSS), colour = "gray51") +
  geom_freqpoly(aes(x=RF_GFDLCM3_rcp45_50_TSS), colour = "gray27") +
  geom_freqpoly(aes(x=RF_HadGEM2ES_rcp45_50_TSS), colour = "black") +

  theme(axis.title=element_text(size=16)) + # Change font size legend
  theme(axis.text=element_text(size=12)) + # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) + # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white")) + # Remove the background
  labs(x="# of species", y="# of grid cells", title="") + # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("(d)") + 
  theme(plot.title = element_text(size = 14,face="bold",hjust = 0))
plot(Futhist)

#-#-# Combine maps #-#-#
## Set the legend
L_df <- data.frame(matrix(ncol = 2, nrow = 4))
  n <- c("Modeltype", "Value")
  colnames(L_df) <- n
  L_df$Modeltype <- c("GAM", "GBM", "GLM", "RF")
  L_df$Value <- rep(1,4)

Legend_plot <- ggplot(L_df, aes(x = Modeltype, y = Value)) +
  geom_point(aes(colour = factor(Modeltype))) +
  scale_colour_manual("Model type", values = c("royalblue3", "green3", "orangered3", "gray27")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))
plot(Legend_plot)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
Legend_hist <- g_legend(Legend_plot)
  
Comb_map <- arrangeGrob(SRplot,SRplotFut,
                       widths = c(2,2),
                       heights = c(6),
                       ncol = 2,
                       nrow = 1)
plot(Comb_map)

Comb_hist <- arrangeGrob(Curhist,Legend_hist,Futhist,
                       widths = c(1.5,0.5,1.5),
                       heights = c(2),
                       ncol = 3,
                       nrow = 1)
plot(Comb_hist)

Comb_plot<- arrangeGrob(Comb_map,Comb_hist,
                         widths = c(3),
                         heights = c(2,1),
                         ncol = 1,
                         nrow = 2)
plot(Comb_plot)

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/")
ggsave("Fig S7 Species richness trigger TSS RCP 45.jpeg",Comb_plot,width=8, height=6, unit="in", dpi=600, bg="transparent")


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#


#-#-# Max Kappa plots #-#-#
#-#-# Read matrixes back in for plotting #-#-#
SR_data <- read.csv(paste0(SRpath, "Species_richness_trigger_MaxKap.csv"))
Coordinates <- read.csv(paste0(datapath, "terrestrial_coords.csv"))
Coordinates <- subset(Coordinates, x <= -20)
head(Coordinates)
SR_data <- merge(Coordinates, SR_data, by = c("x", "y"), all.x = T)
head(SR_data)

## Baseline projections
Base <- SR_data[c("x","y","GAM_baseline_MaxKap", "GBM_baseline_MaxKap", "GLM_baseline_MaxKap", "RF_baseline_MaxKap")]  
Base$Sd <- rowSds(as.matrix(Base[3:ncol(Base)]))
Base$Ensemble <- rowMeans(Base[c("GAM_baseline_MaxKap", "GBM_baseline_MaxKap", "GLM_baseline_MaxKap", "RF_baseline_MaxKap")])
head(Base)

## Future projections
Future_rcp26 <- SR_data[c("x", "y", "GAM_CCSM4_rcp26_50_MaxKap", "GAM_GFDLCM3_rcp26_50_MaxKap", "GAM_HadGEM2ES_rcp26_50_MaxKap",
                          "GBM_CCSM4_rcp26_50_MaxKap", "GBM_GFDLCM3_rcp26_50_MaxKap", "GBM_HadGEM2ES_rcp26_50_MaxKap",
                          "GLM_CCSM4_rcp26_50_MaxKap", "GLM_GFDLCM3_rcp26_50_MaxKap", "GLM_HadGEM2ES_rcp26_50_MaxKap",
                          "RF_CCSM4_rcp26_50_MaxKap", "RF_GFDLCM3_rcp26_50_MaxKap", "RF_HadGEM2ES_rcp26_50_MaxKap")] 
Future_rcp26$Sd <- rowSds(as.matrix(Future_rcp26[3:ncol(Future_rcp26)]))
Future_rcp26$Ensemble <- rowMeans(Future_rcp26[c("GAM_CCSM4_rcp26_50_MaxKap", "GAM_GFDLCM3_rcp26_50_MaxKap", "GAM_HadGEM2ES_rcp26_50_MaxKap",
                                                 "GBM_CCSM4_rcp26_50_MaxKap", "GBM_GFDLCM3_rcp26_50_MaxKap", "GBM_HadGEM2ES_rcp26_50_MaxKap",
                                                 "GLM_CCSM4_rcp26_50_MaxKap", "GLM_GFDLCM3_rcp26_50_MaxKap", "GLM_HadGEM2ES_rcp26_50_MaxKap",
                                                 "RF_CCSM4_rcp26_50_MaxKap", "RF_GFDLCM3_rcp26_50_MaxKap", "RF_HadGEM2ES_rcp26_50_MaxKap")])

head(Future_rcp26)

Future_rcp45 <- SR_data[c("x", "y", "GAM_CCSM4_rcp45_50_MaxKap", "GAM_GFDLCM3_rcp45_50_MaxKap", "GAM_HadGEM2ES_rcp45_50_MaxKap",
                          "GBM_CCSM4_rcp45_50_MaxKap", "GBM_GFDLCM3_rcp45_50_MaxKap", "GBM_HadGEM2ES_rcp45_50_MaxKap",
                          "GLM_CCSM4_rcp45_50_MaxKap", "GLM_GFDLCM3_rcp45_50_MaxKap", "GLM_HadGEM2ES_rcp45_50_MaxKap",
                          "RF_CCSM4_rcp45_50_MaxKap", "RF_GFDLCM3_rcp45_50_MaxKap", "RF_HadGEM2ES_rcp45_50_MaxKap")] 
Future_rcp45$Sd <- rowSds(as.matrix(Future_rcp45[3:ncol(Future_rcp45)]))
Future_rcp45$Ensemble <- rowMeans(Future_rcp45[c("GAM_CCSM4_rcp45_50_MaxKap", "GAM_GFDLCM3_rcp45_50_MaxKap", "GAM_HadGEM2ES_rcp45_50_MaxKap",
                                                 "GBM_CCSM4_rcp45_50_MaxKap", "GBM_GFDLCM3_rcp45_50_MaxKap", "GBM_HadGEM2ES_rcp45_50_MaxKap",
                                                 "GLM_CCSM4_rcp45_50_MaxKap", "GLM_GFDLCM3_rcp45_50_MaxKap", "GLM_HadGEM2ES_rcp45_50_MaxKap",
                                                 "RF_CCSM4_rcp45_50_MaxKap", "RF_GFDLCM3_rcp45_50_MaxKap", "RF_HadGEM2ES_rcp45_50_MaxKap")])

head(Future_rcp45)

Future_rcp85 <- SR_data[c("x", "y", "GAM_CCSM4_rcp85_50_MaxKap", "GAM_GFDLCM3_rcp85_50_MaxKap", "GAM_HadGEM2ES_rcp85_50_MaxKap",
                          "GBM_CCSM4_rcp85_50_MaxKap", "GBM_GFDLCM3_rcp85_50_MaxKap", "GBM_HadGEM2ES_rcp85_50_MaxKap",
                          "GLM_CCSM4_rcp85_50_MaxKap", "GLM_GFDLCM3_rcp85_50_MaxKap", "GLM_HadGEM2ES_rcp85_50_MaxKap",
                          "RF_CCSM4_rcp85_50_MaxKap", "RF_GFDLCM3_rcp85_50_MaxKap", "RF_HadGEM2ES_rcp85_50_MaxKap")]
Future_rcp85$Sd <- rowSds(as.matrix(Future_rcp85[3:ncol(Future_rcp85)]))
Future_rcp85$Ensemble <- rowMeans(Future_rcp85[c("GAM_CCSM4_rcp85_50_MaxKap", "GAM_GFDLCM3_rcp85_50_MaxKap", "GAM_HadGEM2ES_rcp85_50_MaxKap",
                                                 "GBM_CCSM4_rcp85_50_MaxKap", "GBM_GFDLCM3_rcp85_50_MaxKap", "GBM_HadGEM2ES_rcp85_50_MaxKap",
                                                 "GLM_CCSM4_rcp85_50_MaxKap", "GLM_GFDLCM3_rcp85_50_MaxKap", "GLM_HadGEM2ES_rcp85_50_MaxKap",
                                                 "RF_CCSM4_rcp85_50_MaxKap", "RF_GFDLCM3_rcp85_50_MaxKap", "RF_HadGEM2ES_rcp85_50_MaxKap")])

head(Future_rcp85)

# #-#-# Plot the data #-#-#
colPal <- rev(brewer.pal(11,"Spectral"))[-c(3)]
min(na.omit(Base$Ensemble))
Curmax <- round(max(na.omit(Base$Ensemble)),0)
hist(na.omit(Future_rcp45$Ensemble))
min(na.omit(Future_rcp45$Ensemble))
Futmax <- round(max(na.omit(Future_rcp45$Ensemble)),0)

## Current SR
SRplot<- ggplot(data=Base, aes(y=y, x=x)) +
  geom_raster(aes(fill =  Ensemble), stat = "identity", position = "identity")+
  scale_fill_gradientn("Species\nrichness",colours=colPal,limits=c(1,Curmax),na.value = "white")+ # Insert colour and set range #-72,100
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

## Current SR histogram
Curhist <- ggplot(Base) +
  geom_freqpoly(aes(x=GAM_baseline_MaxKap), colour = "royalblue3", show.legend = T) +
  geom_freqpoly(aes(x=GBM_baseline_MaxKap), colour = "green3") +
  geom_freqpoly(aes(x=GLM_baseline_MaxKap), colour = "orangered3") +
  geom_freqpoly(aes(x=RF_baseline_MaxKap), colour = "gray27") +
  theme(axis.title=element_text(size=16)) + # Change font size legend
  theme(axis.text=element_text(size=12)) + # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) + # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white")) + # Remove the background
  labs(x="# of species", y="# of grid cells", title="") + # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("(c)") + 
  theme(plot.title = element_text(size = 14,face="bold",hjust = 0))
plot(Curhist)

## Future SR
SRplotFut<- ggplot(data=Future_rcp45, aes(y=y, x=x)) +
  geom_raster(aes(fill =  Ensemble), stat = "identity", position = "identity")+
  scale_fill_gradientn("Species\n%richness",colours=colPal,limits=c(1,Curmax),na.value = "white")+ # Insert colour and set range #-72,100
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
Futhist <- ggplot(Future_rcp45) +
  geom_freqpoly(aes(x=GAM_CCSM4_rcp45_50_MaxKap), colour = "royalblue") +
  geom_freqpoly(aes(x=GAM_GFDLCM3_rcp45_50_MaxKap), colour = "royalblue3") +
  geom_freqpoly(aes(x=GAM_HadGEM2ES_rcp45_50_MaxKap), colour = "blue4") +
  
  geom_freqpoly(aes(x=GBM_CCSM4_rcp45_50_MaxKap), colour = "chartreuse3") +
  geom_freqpoly(aes(x=GBM_GFDLCM3_rcp45_50_MaxKap), colour = "green3") +
  geom_freqpoly(aes(x=GBM_HadGEM2ES_rcp45_50_MaxKap), colour = "darkgreen") +
  
  geom_freqpoly(aes(x=GLM_CCSM4_rcp45_50_MaxKap), colour = "indianred1") +
  geom_freqpoly(aes(x=GLM_GFDLCM3_rcp45_50_MaxKap), colour = "orangered3") +
  geom_freqpoly(aes(x=GLM_HadGEM2ES_rcp45_50_MaxKap), colour = "orangered4") +
  
  geom_freqpoly(aes(x=RF_CCSM4_rcp45_50_MaxKap), colour = "gray51") +
  geom_freqpoly(aes(x=RF_GFDLCM3_rcp45_50_MaxKap), colour = "gray27") +
  geom_freqpoly(aes(x=RF_HadGEM2ES_rcp45_50_MaxKap), colour = "black") +
  
  theme(axis.title=element_text(size=16)) + # Change font size legend
  theme(axis.text=element_text(size=12)) + # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) + # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white")) + # Remove the background
  labs(x="# of species", y="# of grid cells", title="") + # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("(d)") + 
  theme(plot.title = element_text(size = 14,face="bold",hjust = 0))
plot(Futhist)

#-#-# Combine maps #-#-#
## Set the legend
L_df <- data.frame(matrix(ncol = 2, nrow = 4))
n <- c("Modeltype", "Value")
colnames(L_df) <- n
L_df$Modeltype <- c("GAM", "GBM", "GLM", "RF")
L_df$Value <- rep(1,4)

Legend_plot <- ggplot(L_df, aes(x = Modeltype, y = Value)) +
  geom_point(aes(colour = factor(Modeltype))) +
  scale_colour_manual("Model type", values = c("royalblue3", "green3", "orangered3", "gray27")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))
plot(Legend_plot)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
Legend_hist <- g_legend(Legend_plot)

Comb_map <- arrangeGrob(SRplot,SRplotFut,
                        widths = c(2,2),
                        heights = c(6),
                        ncol = 2,
                        nrow = 1)
plot(Comb_map)

Comb_hist <- arrangeGrob(Curhist,Legend_hist,Futhist,
                         widths = c(1.5,0.5,1.5),
                         heights = c(2),
                         ncol = 3,
                         nrow = 1)
plot(Comb_hist)

Comb_plot<- arrangeGrob(Comb_map,Comb_hist,
                        widths = c(3),
                        heights = c(2,1),
                        ncol = 1,
                        nrow = 2)
plot(Comb_plot)

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/")
ggsave("Fig 2 Species richness trigger MaxKap RCP 45.jpeg",Comb_plot,width=8, height=6, unit="in", dpi=600, bg="transparent")

