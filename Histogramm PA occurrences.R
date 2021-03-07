#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#   Changes in species occurrences across the network   #
#             KBA network South America                 #
#            Percentage change per species              #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 

library(ggplot2)
library(dplyr)
library(viridis)
library(grid)
library(gridExtra)

#-#-# Get the trigger species list if subsetting by trigger species #-#-#
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data")
Trigger <- get(load("IBA trigger species.Rdata"))


#-#-# Set working directionary and get the files
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/With habitat/PA occurrences per species rcp26 50 habitat/")
AllSpecies <- list.files()


## Summarize the occurrence data 
Summarize <- lapply(AllSpecies,function(n){
  OneSp <- get(load(n))
  Species <- paste0(strsplit(n,split="_")[[1]][1],"_",strsplit(n,split="_")[[1]][2])
  print(Species)
  if(Species %in%  Trigger){ # If subsetting the plot to trigger species only
  All <- nrow(OneSp)
  Current <- sum(as.numeric(as.character(OneSp$current)))
  Future <- sum(as.numeric(as.character(OneSp$future)))
  Both <- sum(as.numeric(as.character(OneSp$both)))
  Result <- as.data.frame(cbind(Species,All,Current,Future,Both))
  return(Result) 
  }
})

OccChangeTable <- do.call(rbind,Summarize)
OccChangeTable[2:ncol(OccChangeTable)] <- lapply(OccChangeTable[2:ncol(OccChangeTable)], function(x) as.numeric(as.character(x)))
head(OccChangeTable)
nrow(OccChangeTable)
str(OccChangeTable)

#-#-# Add percentage changes per species #-#-#
## How many PAs are new to the species
OccChangeTable$New <- OccChangeTable$Future - OccChangeTable$Both
OccChangeTable$Loss <- OccChangeTable$Current - OccChangeTable$Both
head(OccChangeTable)

## Add percentages
OccChangeTable$Change <- (OccChangeTable$Future - OccChangeTable$Current) / (OccChangeTable$Current /100)
OccChangeTable$PercSame <- OccChangeTable$Both / (OccChangeTable$Current / 100)
OccChangeTable$PercNewTo <- OccChangeTable$New / (OccChangeTable$Current / 100)
OccChangeTable$PercLostFrom <- OccChangeTable$Loss / (OccChangeTable$Current / 100)

OccChangeTable[13,]
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/") ## Check Inf and NAs
write.csv(OccChangeTable,"Change_in_species_occurrence_rcp_26_2050 habitat.csv")


#-#-# Plot the data #-#-#

## Make Plot tables
## RCP26
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")
OccChangeTable26 <- read.csv("Change_in_species_occurrence_rcp_45_2050.csv")
head(OccChangeTable26)
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

#ChangeData<- rbind(Current_summary,Future_summary,Both_summary,New_summary,Loss_summary)
#ChangeCategory <- rbind("Currently","In future","Both","Gained","Lost")
ChangeData <- rbind(Current_summary,Future_summary,Both_summary)
RCP <- rbind("current conditions","RCP 2.6","RCP 2.6")
ChangeCategory <- rbind("Currently","2050","Both")
PlotData26 <- cbind(ChangeCategory,RCP,ChangeData)

#PlotData26$ChangeCategory <- factor(PlotData$ChangeCategory,levels = c("Currently","In future","Both","Lost","Gained"))
PlotData26$ChangeCategory <- factor(PlotData26$ChangeCategory,levels = c("Currently","2050","Both"))

## RCP45
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")
OccChangeTable45 <- read.csv("Change_in_species_occurrence_rcp_45_2050.csv")
head(OccChangeTable45)
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

#ChangeData <- rbind(Current_summary,Future_summary,Both_summary,New_summary,Loss_summary)
#ChangeCategory <- rbind("Currently","In future","Both","Gained","Lost")
ChangeData <- rbind(Future_summary,Both_summary)
RCP <- rbind("RCP 4.5","RCP 4.5")
ChangeCategory <- rbind("2050","Both")
PlotData45 <- cbind(ChangeCategory,RCP,ChangeData)

#PlotData45$ChangeCategory <- factor(PlotData$ChangeCategory,levels = c("Currently","In future","Both","Lost","Gained"))
PlotData45$ChangeCategory <- factor(PlotData45$ChangeCategory,levels = c("2050","Both"))


## RCP85
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")
OccChangeTable85 <- read.csv("Change_in_species_occurrence_rcp_85_2050.csv")
head(OccChangeTable85)
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

#ChangeData <- rbind(Current_summary,Future_summary,Both_summary,New_summary,Loss_summary)
#ChangeCategory <- rbind("Currently","In future","Both","Gained","Lost")
ChangeData <- rbind(Future_summary,Both_summary)
RCP <- rbind("RCP 8.5","RCP 8.5")
ChangeCategory <- rbind("2050","Both")
PlotData85 <- cbind(ChangeCategory,RCP,ChangeData)

#PlotData85$ChangeCategory <- factor(PlotData$ChangeCategory,levels = c("Currently","In future","Both","Lost","Gained"))
PlotData85$ChangeCategory <- factor(PlotData85$ChangeCategory,levels = c("2050","Both"))

Combined <- rbind(PlotData26,PlotData45,PlotData85)

Combinedplot <-ggplot(Combined, aes(x=ChangeCategory, y=mean_PL, fill=RCP, colour=RCP)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=c("white","black", "grey40", "grey83")) +
  scale_colour_manual(values=c("black","black","black","black")) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=16))+ # Change font size legend
  theme(axis.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="", y="Mean # of KBAs", title="")+ # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  #geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), 
                #width=0.9,position = position_dodge2(width = 1, preserve = "single"),color="black") +
  ggtitle("(a)")+ 
  theme(plot.title = element_text(size = 18,face="bold",hjust = 0))
Combinedplot

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Manuscript plots/")
#ggsave("Barchart number of KBAs a species occurs in_trigger.tiff",Combinedplot,width=5, height=4, unit="in", dpi=300, bg="transparent")


#-#-# Second plot # of species with one occurrence #-#-#
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")
OccChangeTable26 <- read.csv("Change_in_species_occurrence_rcp_26_2050.csv")
head(OccChangeTable26)
Current <- nrow(subset(OccChangeTable26,Current > 0))
Future <- nrow(subset(OccChangeTable26,Future > 0))
data <- rbind(Current,Future)
RCP <- rbind("current conditions","RCP 2.6")
ChangeCategory <- rbind("Current","2050")
PlotData26 <- as.data.frame(cbind(ChangeCategory,RCP,data))
colnames(PlotData26) <- c("ChangeCategory","RCP","Species")

OccChangeTable45 <- read.csv("Change_in_species_occurrence_rcp_45_2050.csv")
Future <- nrow(subset(OccChangeTable45,Future > 0))
RCP <- "RCP 4.5"
ChangeCategory <- "2050"
PlotData45 <- cbind(ChangeCategory,RCP,Future)
colnames(PlotData45) <- c("ChangeCategory","RCP","Species")

OccChangeTable85 <- read.csv("Change_in_species_occurrence_rcp_85_2050.csv")
Future <- nrow(subset(OccChangeTable85,Future > 0))
RCP <- "RCP 8.5"
ChangeCategory <- "2050"
PlotData85 <- cbind(ChangeCategory,RCP,Future)
colnames(PlotData85) <- c("ChangeCategory","RCP","Species")

n <- as.data.frame(c(1,2,3,4))
colnames(n) <- "n"
CombinedData <- as.data.frame(rbind(PlotData26,PlotData45,PlotData85))
CombinedData <- cbind(CombinedData,n)

CombinedData$RCP <- factor(CombinedData$RCP,levels = c("current conditions","RCP 2.6","RCP 4.5","RCP 8.5"))
CombinedData$ChangeCategory <- factor(CombinedData$ChangeCategory,levels = c("Current","2050"))
#CombinedData$ChangeCategory <- factor(CombinedData$ChangeCategory, levels = CombinedData$n)

CombinedData$Species <- as.numeric(as.character(CombinedData$Species))/(941/100) # 941 Total number of species included

Countplot <-ggplot(CombinedData, aes(x=ChangeCategory, y=as.numeric(as.character(Species)), fill=RCP,colour=RCP, group=n)) + #, colour=RCP
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=c("white","black", "grey40", "grey83")) +
  scale_colour_manual(values=c("black","black","black","black")) +
  scale_y_continuous(c(0,20,40,60,80,100),name = "% of species") +
  theme(axis.title=element_text(size=16))+ # Change font size legend
  theme(axis.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white")) + # Remove the background
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  labs(y="% of species", x = "") + 
  ggtitle("(b)")+ 
  theme(plot.title = element_text(size = 16,face="bold",hjust = 0))
Countplot

CombBar <- arrangeGrob(Combinedplot,Countplot,
                       widths = c(8,8),
                       heights = c(0.5),
                       ncol = 2,
                       nrow = 1)



plot(CombBar)

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Manuscript drafts/Submission/Final figures/")
ggsave("Combined Barchart number of KBAs a species occurs in rcp 45.tiff",CombBar,width=12, height=4, unit="in", dpi=300, bg="transparent")


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
## Plots
ChangePlotSE26 <- ggplot(PlotData26, aes(ChangeCategory,mean_PL)) + 
  geom_col(show.legend = F) +  
  scale_fill_manual("legend", values = c("Currently" = "black", "In future" = "black", "Both" = "orange", "Lost" = "blue", "Gained" = "black"))+
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2) +
  labs(y="Mean # of KBAs ± s.e.", x = "") + theme_classic()
plot(ChangePlotSE26)

ChangePlotSE45 <- ggplot(PlotData45, aes(ChangeCategory,mean_PL)) + 
  geom_col(show.legend = F) +  
  scale_fill_manual("legend", values = c("Currently" = "black", "In future" = "black", "Both" = "orange", "Lost" = "blue", "Gained" = "black"))+
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2) +
  labs(y="", x = "") + theme_classic()
plot(ChangePlotSE45)

ChangePlotSE85 <- ggplot(PlotData85, aes(ChangeCategory,mean_PL)) + 
  geom_col(show.legend = F) +  
  scale_fill_manual("legend", values = c("Currently" = "black", "In future" = "black", "Both" = "orange", "Lost" = "blue", "Gained" = "black"))+
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2) +
  labs(y="", x = "") + theme_classic()
plot(ChangePlotSE85)

CombBar <- arrangeGrob(ChangePlotSE26,ChangePlotSE45,ChangePlotSE85,
                        widths = c(8,8,8),
                        heights = c(0.5),
                        ncol = 3,
                        nrow = 1)



plot(CombBar)

setwd("/Users/alkevoskamp/Documents/PD manuscript/Manuscript Figures/")
ggsave("Barchart number of KBAs a species occurs in.tiff",CombNull,width=35, height=13, unit="in", dpi=300, bg="transparent")


#-#-# Derive numbers for results text #-#-#
#-#-# Percentage of species covered 
## RCP26
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")
OccChangeTable26 <- read.csv("Change_in_species_occurrence_rcp_26_2050_trigger.csv")
head(OccChangeTable26)
NoCurCov <- nrow(subset(OccChangeTable26,Current >= 1))
NoFutCov <- nrow(subset(OccChangeTable26,Future >= 1))
PercSpCov26 <- NoFutCov/(NoCurCov/100)
NoFutCov
PercSpCov26

## RCP45
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")
OccChangeTable45 <- read.csv("Change_in_species_occurrence_rcp_45_2050_trigger.csv")
head(OccChangeTable45)
NoCurCov <- nrow(subset(OccChangeTable45,Current >= 1))
NoFutCov <- nrow(subset(OccChangeTable45,Future >= 1))
PercSpCov45 <- NoFutCov/(NoCurCov/100)
NoFutCov
PercSpCov45

## RCP85
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")
OccChangeTable85 <- read.csv("Change_in_species_occurrence_rcp_85_2050_trigger.csv")
head(OccChangeTable85)
NoCurCov <- nrow(subset(OccChangeTable85,Current >= 1))
NoFutCov <- nrow(subset(OccChangeTable85,Future >= 1))
PercSpCov85 <- NoFutCov/(NoCurCov/100)
NoFutCov
PercSpCov85

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/No habitat/PA occurrence changes rcp85 50 trigger/")
AllFiles <- list.files()

PAdata <- lapply(AllFiles,function(x){
  name <- strsplit(x,split="_")[[1]][1]
  print(name)
  data <- as.data.frame(get(load(x)))
  Same <- data$currentlist[data$currentlist %in% data$futurelist]
  Same <- length(Same)
  Current <- length(na.omit(data$currentlist))
  PercSpLeft <- Same/(Current/100)
  Final <- cbind(name,PercSpLeft)
})

Summary <- as.data.frame(do.call(rbind,PAdata))
Summary$PercSpLeft <- as.numeric(as.character(Summary$PercSpLeft))
head(Summary)
data <- subset(Summary,PercSpLeft >= 90)
nrow(data)
nrow(data)/(1597/100)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
ChangePlotSD <- ggplot(PlotData, aes(ChangeCategory,mean_PL)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2) +
  labs(y="Percentage change ± s.d.", x = "KBAs the species occurs in") + theme_classic()
plot(ChangePlotSD)


## Calculate percentage change
# Change_summary <- OccChangeTable %>% # the names of the new data frame and the data frame to be summarised
#   summarise(mean_PL = mean(na.omit(Change)),  # calculates the mean of each group
#             sd_PL = sd(na.omit(Change)), # calculates the standard deviation of each group
#             n_PL = n(),  # calculates the sample size per group
#             SE_PL = sd(na.omit(Change))/sqrt(n()))
# 
# PercSame_summary <- OccChangeTable %>% # the names of the new data frame and the data frame to be summarised
#   summarise(mean_PL = mean(na.omit(PercSame)),  # calculates the mean of each group
#             sd_PL = sd(na.omit(PercSame)), # calculates the standard deviation of each group
#             n_PL = n(),  # calculates the sample size per group
#             SE_PL = sd(na.omit(PercSame))/sqrt(n()))
# 
# PercNewTo_summary <- OccChangeTable %>% # the names of the new data frame and the data frame to be summarised
#   summarise(mean_PL = mean(na.omit(PercNewTo)),  # calculates the mean of each group
#             sd_PL = sd(na.omit(PercNewTo)), # calculates the standard deviation of each group
#             n_PL = n(),  # calculates the sample size per group
#             SE_PL = sd(na.omit(PercNewTo))/sqrt(n()))
# 
# PercLostFrom_summary <- OccChangeTable %>% # the names of the new data frame and the data frame to be summarised
#   summarise(mean_PL = mean(na.omit(PercLostFrom)),  # calculates the mean of each group
#             sd_PL = sd(na.omit(PercLostFrom)), # calculates the standard deviation of each group
#             n_PL = n(),  # calculates the sample size per group
#             SE_PL = sd(na.omit(PercLostFrom))/sqrt(n()))



