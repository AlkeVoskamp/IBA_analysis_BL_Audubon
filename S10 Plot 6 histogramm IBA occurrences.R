#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#       Changes in species occurrences across the       #
#             IBA network South America                 #
#            Percentage change per species              #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 


#-#-# Load libraries #-#-#
library(ggplot2)
library(dplyr)
library(viridis)
library(grid)
library(gridExtra)


#-#-# Set fole paths #-#-#
triggerpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/"
splistpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/"
filepath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBAs_suitable_per_species_trigger/"

#-#-# Get the trigger species list if subsetting by trigger species #-#-#
Good_mods <- read.csv(paste0(splistpath, "SDMs with high AUC all species.csv"))
Good_mods <- as.vector(Good_mods$Species)
Trigger <- get(load(paste0(triggerpath,"IBA trigger species.Rdata")))


#-#-# Set working directionary and get the files
AllSpecies <- list.files(filepath, pattern = "habitat")


#-#-# Lists to go through sites, SDMs and GCMs #-#-#
RCP <- c("rcp26","rcp45","rcp85")
Thres <- c("TSS","MaxKap")


## Summarize the occurrence data 
Summarize <- lapply(AllSpecies,function(n){
  OneSp <- get(load(paste0(filepath ,n)))
  Species <- paste0(strsplit(n,split="_")[[1]][1],"_",strsplit(n,split="_")[[1]][2])
  print(Species)
  
  if(Species %in% Good_mods){
  if(Species %in% Trigger){ # If subsetting the plot to trigger species only
    
  All_scenarioes <- colnames(OneSp[2:ncol(OneSp)])  
  
  Thres_type <- lapply(Thres, function(t){
    print(t)
    Thres <- t
    
    Thres_sub <- grep(All_scenarioes, pattern = Thres)
    Thres_scenarioes <- All_scenarioes[Thres_sub]
    
    ##Loop through the different RCPs
    RCP_type <- lapply(RCP, function(r){
      
      print(r)
      RCP <- r
      
      RCP_sub <- grep(Thres_scenarioes, pattern = RCP)
      RCP_scenarioes <- Thres_scenarioes[RCP_sub]
      
      ##All IBAs
      All <- nrow(OneSp)
      
      ##Extract current number of IBAs
      Current_sub <- grep(RCP_scenarioes, pattern = "current_")
      Current_scenarioes <- RCP_scenarioes[Current_sub]
      OneSp_current <- OneSp[Current_scenarioes]
      OneSp_current$mean <- rowMeans(OneSp_current)
      Current <- sum(as.numeric(as.character(OneSp_current$mean)))
      
      ##Extract future number of IBAs
      Future_sub <- grep(RCP_scenarioes, pattern = "future_")
      Future_scenarioes <- RCP_scenarioes[Future_sub]
      OneSp_future <- OneSp[Future_scenarioes]
      OneSp_future$mean <- rowMeans(OneSp_future)
      Future <- sum(as.numeric(as.character(OneSp_future$mean)))
      
      ##Extract both times covered
      Both_sub <- grep(RCP_scenarioes, pattern = "both_")
      Both_scenarioes <- RCP_scenarioes[Both_sub]
      OneSp_both <- OneSp[Both_scenarioes]
      OneSp_both$mean <- rowMeans(OneSp_both)
      Both <- sum(as.numeric(as.character(OneSp_both$mean)))

      Result <- as.data.frame(cbind(RCP,Species,All,Current,Future,Both))
      return(Result) 
      
    }) #Close RCP
    
    Result_thres <- do.call(rbind, RCP_type)
    Result_thres$Thres <- Thres
    Result_thres <- Result_thres[c("Thres", "RCP", "Species", "All", "Current", "Future", "Both")]
    return(Result_thres)
       
  }) #Close threshold  
      
  Result_final <- do.call(rbind, Thres_type)
  return(Result_final)
  } #Trigger species
 } #Good AUC
})
    
OccChangeTable <- do.call(rbind, Summarize)
OccChangeTable[4:ncol(OccChangeTable)] <- lapply(OccChangeTable[4:ncol(OccChangeTable)], function(x) as.numeric(as.character(x)))
head(OccChangeTable)
nrow(OccChangeTable)
str(OccChangeTable)

#-#-# Add percentage changes per species #-#-#
## How many IBAs are new to the species
OccChangeTable$New <- OccChangeTable$Future - OccChangeTable$Both
OccChangeTable$Loss <- OccChangeTable$Current - OccChangeTable$Both
head(OccChangeTable)

## Add percentages
OccChangeTable$Change <- (OccChangeTable$Future - OccChangeTable$Current) / (OccChangeTable$Current /100)
OccChangeTable$PercSame <- OccChangeTable$Both / (OccChangeTable$Current / 100)
OccChangeTable$PercNewTo <- OccChangeTable$New / (OccChangeTable$Current / 100)
OccChangeTable$PercLostFrom <- OccChangeTable$Loss / (OccChangeTable$Current / 100)

OccChangeTable[13,]
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/Species_occurrence_changes/") ## Check Inf and NAs
write.csv(OccChangeTable,"Change_in_species_occurrence_trigger_habitat.csv")


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#-#-# Plot the data #-#-#


#-#-# Set the file paths #-#-#
filepath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/Species_occurrence_changes/"
plotpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/Main manuscript/"


#-#-# Get the data #-#-#
Change_data <- read.csv(paste0(filepath,"Change_in_species_occurrence_trigger_habitat.csv"))


#-#-# Make Plot tables #-#-#
## Set the threshold
Threshold <- "MaxKap" # "TSS" 

## RCP26
OccChangeTable26 <- subset(Change_data, RCP == "rcp26") #Select rcp
OccChangeTable26 <- subset(OccChangeTable26, Thres == Threshold) #Select threshold

Current_summary <- OccChangeTable26 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Current)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Current)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Current))/sqrt(n()))

Future_summary <- OccChangeTable26 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Future)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Future)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Future))/sqrt(n()))

Both_summary <- OccChangeTable26 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Both)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Both)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Both))/sqrt(n()))

New_summary <- OccChangeTable26 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(New)),  # calculates the mean of each group
            sd_PL = sd(na.omit(New)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(New))/sqrt(n()))

Loss_summary <- OccChangeTable26 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Loss)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Loss)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Loss))/sqrt(n()))

ChangeData <- rbind(Current_summary,Future_summary,Both_summary)
RCP <- rbind("current conditions","RCP 2.6","RCP 2.6")
ChangeCategory <- rbind("Currently","2050","Both")
PlotData26 <- cbind(ChangeCategory,RCP,ChangeData)

PlotData26$ChangeCategory <- factor(PlotData26$ChangeCategory,levels = c("Currently","2050","Both"))

## RCP45
OccChangeTable45 <- subset(Change_data, RCP == "rcp45") #Select rcp
OccChangeTable45 <- subset(OccChangeTable45, Thres == Threshold) #Select threshold
Current_summary <- OccChangeTable45 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Current)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Current)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Current))/sqrt(n()))

Future_summary <- OccChangeTable45 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Future)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Future)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Future))/sqrt(n()))

Both_summary <- OccChangeTable45 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Both)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Both)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Both))/sqrt(n()))

New_summary <- OccChangeTable45 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(New)),  # calculates the mean of each group
            sd_PL = sd(na.omit(New)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(New))/sqrt(n()))

Loss_summary <- OccChangeTable45 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Loss)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Loss)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Loss))/sqrt(n()))

ChangeData <- rbind(Future_summary,Both_summary)
RCP <- rbind("RCP 4.5","RCP 4.5")
ChangeCategory <- rbind("2050","Both")
PlotData45 <- cbind(ChangeCategory,RCP,ChangeData)

PlotData45$ChangeCategory <- factor(PlotData45$ChangeCategory,levels = c("2050","Both"))


## RCP85
OccChangeTable85 <- subset(Change_data, RCP == "rcp85") #Select rcp
OccChangeTable85 <- subset(OccChangeTable85, Thres == Threshold) #Select threshold
Current_summary <- OccChangeTable85 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Current)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Current)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Current))/sqrt(n()))

Future_summary <- OccChangeTable85 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Future)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Future)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Future))/sqrt(n()))

Both_summary <- OccChangeTable85 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Both)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Both)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Both))/sqrt(n()))

New_summary <- OccChangeTable85 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(New)),  # calculates the mean of each group
            sd_PL = sd(na.omit(New)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(New))/sqrt(n()))

Loss_summary <- OccChangeTable85 %>% # the names of the new data frame and the data frame to be summarised
  dplyr::summarise(mean_PL = mean(na.omit(Loss)),  # calculates the mean of each group
            sd_PL = sd(na.omit(Loss)), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(na.omit(Loss))/sqrt(n()))

ChangeData <- rbind(Future_summary,Both_summary)
RCP <- rbind("RCP 8.5","RCP 8.5")
ChangeCategory <- rbind("2050","Both")
PlotData85 <- cbind(ChangeCategory,RCP,ChangeData)

PlotData85$ChangeCategory <- factor(PlotData85$ChangeCategory,levels = c("2050","Both"))

Combined <- rbind(PlotData26,PlotData45,PlotData85)

#-#-# Plot average number of IBAs remaining #-#-#
Combinedplot <- ggplot(Combined, aes(x=ChangeCategory, y=mean_PL, fill=RCP, colour=RCP)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=c("white","grey20", "grey40", "grey83")) +
  scale_colour_manual(values=c("black","black","black","black")) +
  geom_errorbar(aes(ymin=mean_PL-SE_PL, ymax=mean_PL+SE_PL), width=.2,
                position=position_dodge(.9)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16))+ # Change font size legend
  theme(axis.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="Mean # of IBAs", title="")+ # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("(a)")+ 
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0))
plot(Combinedplot)


#---#---#---#---#---#---# Second plot #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
## RCP26
OccChangeTable26 <- subset(Change_data, RCP == "rcp26") #Select rcp
OccChangeTable26 <- subset(OccChangeTable26, Thres == Threshold) #Select threshold
OccChangeTable26$PercLeft <- OccChangeTable26$Future/(OccChangeTable26$Current/100)
MeanChangeTab <- (OccChangeTable26[is.finite(OccChangeTable26$PercLeft), ])
Mean_perc_left <- mean(MeanChangeTab$PercLeft)
Mean_perc_SD <- sd(MeanChangeTab$PercLeft)
RCP <- "RCP 2.6"
ChangeCategory <- "2050"
PlotData26 <- as.data.frame(cbind(ChangeCategory,RCP,Mean_perc_left,Mean_perc_SD))

## RCP45
OccChangeTable45 <- subset(Change_data, RCP == "rcp45") #Select rcp
OccChangeTable45 <- subset(OccChangeTable45, Thres == Threshold) #Select threshold
OccChangeTable45$PercLeft <- OccChangeTable45$Future/(OccChangeTable45$Current/100)
MeanChangeTab <- (OccChangeTable45[is.finite(OccChangeTable45$PercLeft), ])
Mean_perc_left <- mean(MeanChangeTab$PercLeft)
Mean_perc_SD <- sd(MeanChangeTab$PercLeft)
RCP <- "RCP 4.5"
ChangeCategory <- "2050"
PlotData45 <- as.data.frame(cbind(ChangeCategory,RCP,Mean_perc_left,Mean_perc_SD))

## RCP85
OccChangeTable85 <- subset(Change_data, RCP == "rcp85") #Select rcp
OccChangeTable85 <- subset(OccChangeTable85, Thres == Threshold) #Select threshold
OccChangeTable85$PercLeft <- OccChangeTable85$Future/(OccChangeTable85$Current/100)
MeanChangeTab <- (OccChangeTable85[is.finite(OccChangeTable85$PercLeft), ])
Mean_perc_left <- mean(MeanChangeTab$PercLeft)
Mean_perc_SD <- sd(MeanChangeTab$PercLeft)
RCP <- "RCP 8.5"
ChangeCategory <- "2050"
PlotData85 <- as.data.frame(cbind(ChangeCategory,RCP,Mean_perc_left,Mean_perc_SD))

## Combine dataframes
n <- as.data.frame(c(1,2,3))
colnames(n) <- "n"
CombinedData <- as.data.frame(rbind(PlotData26,PlotData45,PlotData85))
CombinedData <- cbind(CombinedData,n)

CombinedData$RCP <- factor(CombinedData$RCP,levels = c("RCP 2.6","RCP 4.5","RCP 8.5"))
CombinedData$ChangeCategory <- factor(CombinedData$ChangeCategory,levels = c("2050"))

#-#-# Plot average percentage IBAs remaining #-#-#
Countplot <- ggplot(CombinedData, aes(x=ChangeCategory, y=as.numeric(as.character(Mean_perc_left)), fill=RCP,colour=RCP, group=n)) + #, colour=RCP
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=c("grey20", "grey40", "grey83")) +
  scale_colour_manual(values=c("black","black","black")) +
  scale_y_continuous(c(0,20,40,60,80,100),name = "% of IBAs retained") +
  geom_errorbar(aes(ymin=as.numeric(as.character(Mean_perc_left)) - as.numeric(as.character(Mean_perc_SD)),
                    ymax=as.numeric(as.character(Mean_perc_left)) + as.numeric(as.character(Mean_perc_SD))), width=.2,
                    position=position_dodge(.9)) +
  theme(axis.title=element_text(size=16))+ # Change font size legend
  theme(axis.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white")) + # Remove the background
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  labs(y="", x = "") + 
  ggtitle("(b)")+ 
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0))
plot(Countplot)

CombBar <- arrangeGrob(Combinedplot,Countplot,
                       widths = c(8,8),
                       heights = c(0.5),
                       ncol = 2,
                       nrow = 1)

plot(CombBar)

setwd(plotpath)
ggsave("Fig S18 Combined Barchart number of IBAs a species occurs in RCP 45 all MaxKap habitat.tiff",CombBar,width=12, height=4, unit="in", dpi=300, bg="transparent")


#-#-# Extract number of species that retain xx% of the IBAs they currently occur #-#-#
occ_path <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/Species_occurrence_changes/"
all_occ <- list.files(occ_path, pattern = "all")
occ <- read.csv(paste0(occ_path,all_occ[1]))
nrow(occ)

## Get trigger species list
triggerpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/"
Trigger <- get(load(paste0(triggerpath,"IBA trigger species.Rdata")))

## Set the threshold and rcp
Threshold <-  "MaxKap" #"MaxKap"
rcp <- "rcp85"

## Select data
OccChangeTable <- subset(occ, RCP == rcp) #Select rcp
OccChangeTable <- subset(OccChangeTable, Thres == Threshold) #Select threshold
head(OccChangeTable)
nrow(OccChangeTable)

OccChangeTable <- OccChangeTable[c("Species", "PercSame")]

## Subset by trigger species
SubTable <- lapply(Trigger, function(x){
  print(x)
  data <- subset(OccChangeTable, Species == x)
  return(data)
})

SubOccChangeTable <- do.call(rbind, SubTable)
head(SubOccChangeTable)
nrow(SubOccChangeTable)

FinalTable <- OccChangeTable ## Change here all species or trigger subset #SubOccChangeTable

FiftyPerc <- subset(FinalTable,PercSame >= 50)
nrow(FiftyPerc)
nrow(FiftyPerc)/(nrow(FinalTable)/100)

twentyfivePerc <- subset(FinalTable,PercSame >= 25)
nrow(twentyfivePerc)
nrow(twentyfivePerc)/(nrow(FinalTable)/100)

FinalTable[is.na(FinalTable)] <- 0
Lessthan20Perc <- subset(FinalTable,PercSame < 10 )
nrow(Lessthan20Perc)
nrow(Lessthan20Perc)/(nrow(FinalTable)/100)

