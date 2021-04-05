#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#       Changes in species occurrences across the       #
#             IBA network South America                 #
#            Percentage change per species              #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
## 968 trigger species 939 modelled and occuring in the included IBAs

#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 

library(ggplot2)
library(dplyr)
library(viridis)
library(grid)
library(gridExtra)


#-#-# Set the file paths #-#-#
filepath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/Species_occurrence_changes/"
plotpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/Main manuscript/"


#-#-# Get the data #-#-#
## Set the threshold
Threshold <- "MaxKap"
Change_data <- read.csv(paste0(filepath,"Change_in_species_occurrence_trigger_habitat.csv"))
OccChangeTable <- subset(Change_data, RCP == "rcp45") #Select rcp
OccChangeTable <- subset(OccChangeTable, Thres == Threshold) #Select threshold
nrow(OccChangeTable)

#-#-# Calculate change #-#-#
OccChangeTable$PropLeft <- OccChangeTable$Both/(OccChangeTable$Current/100)
head(OccChangeTable)
nrow(OccChangeTable)

#-#-# Extract numbers for result section #-#-#
## Count species that remain in more than 50% of the IBAs they currently occur in
fiftyPercLeft <- subset(OccChangeTable,PropLeft >= 50)
nrow(fiftyPercLeft)
nrow(fiftyPercLeft)/(nrow(OccChangeTable)/100)

## Count species that do not occur in a IBA currently
OccChangeTable <- subset(OccChangeTable,Current > 0)
nrow(OccChangeTable)
# 279 species are not currently occuring in a KBA off all species 85 of the trigger species

## Mean and median proportion left
mean(na.omit(as.numeric(as.character(OccChangeTable$PropLeft))))
median(na.omit(as.numeric(as.character(OccChangeTable$PropLeft))))

hist(as.numeric(as.character(OccChangeTable$PropLeft)))

col <- rep("lightgrey",11)
colII <- rep("black",11)

## Histogram distribution proportion of IBAs remaining
IBApSp <- ggplot(OccChangeTable, aes(x=as.numeric(as.character(PropLeft)))) +
  geom_histogram(binwidth=10, fill = col, colour = colII) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=12))+ # Change font size legend
  theme(axis.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="% of IBAs remaining climatically suitable by 2050", y="# of species", title="")+ # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("a)")+ 
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0))
plot(IBApSp)


#---#---#---#---# Second plot #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#-#-# Size of IBA vs proportion of species left #-#-#
#-#-# Load the IBA polygons and set to South American focal countries #-#-#
countryList <- c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","French Guiana","Jamaica","Puerto Rico (to USA)",
                 "Guyana","Paraguay","Peru","Mexico","Uruguay","Venezuela","Belize","Costa Rica","Haiti","Cuba","Bahamas",
                 "El Salvador","Guatemala","Honduras","Nicaragua","Panama","Bahamas","Suriname","Dominican Republic")

ibaGridded <- get(load("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/IBA_Global_2016_01/IBA_Gridded_0.5_Global_newMex.Rdata"))
ibaGriddedAmList <- sapply(ibaGridded,function(x){x$country})
ibaGriddedAm <- ibaGridded[grep(ibaGriddedAmList,pattern = paste(countryList,collapse="|"))]
polygonlist <- ibaGriddedAm 

numberlist <- c(1:1653)

Area <- lapply(numberlist,function(x){
  print(x)
  polyname <- polygonlist[[x]]$name
  polyname <- strsplit(polyname,split="/",fixed=T)[[1]][1]
  polycountry <-polygonlist[[x]]$country
  polyarea <-polygonlist[[x]]$ibaArea
  if(length(polyarea)==0) {polyarea <- NA}
  data <- cbind(polyname,polycountry,polyarea)
  colnames(data) <- c("IBA","Country","Area")
  return(data)
})

AreaData <- as.data.frame(do.call(rbind,Area))
head(AreaData)


#-#-# Calculate the proportion of species left per KBA #-#-#
#-#-# Set data paths #-#-#
plotpath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/Main manuscript/"
datapath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_species_changes_lists/"


#-#-# Load IBA change data and calculate proportion of IBAs left #-#-#
Plot_file <- read.csv(paste0(datapath,"IBA_all_species_changes_Ensemble_rcp45_2050_MaxKap_habitat.csv"))
Plot_file <- Plot_file[c("IBA", "currentRich", "stablesp")]
Plot_file <- Plot_file %>% group_by(IBA) %>% summarise_all("mean")
Plot_file <- as.data.frame(Plot_file)

Plot_file$PropLeft <- Plot_file$stablesp/(Plot_file$currentRich/100)


#-#-# Merge the area and proportion dataframes #-#-#
PlotData <- merge(AreaData,Plot_file,by=c("IBA"))
PlotData$Area <- as.numeric(as.character(PlotData$Area))
head(PlotData)
str(PlotData)

## Scatterplot IBA area vs peoportion species remaining
scatter <- ggplot(PlotData, aes(x=log(Area), y=PropLeft)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(legend.position = c(0.8, 0.9)) + # Positioning the legend   
  theme(legend.text=element_text(size=10))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) + # Change size of legend key
  theme(panel.background=element_rect(fill='#FFFFFF',colour="white"))+ # Remove the background
  theme(axis.title.x=element_text(vjust=-1.5, size = 12)) +
  theme(axis.title.y=element_text(angle=90, vjust= 2, size = 12)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  labs(x="log(Size of the IBA)", y="% of species left", title="",vjust= -0.5)+ # Remove axis titles
  ggtitle("b) ") + 
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0))

plot(scatter)


#-#-# Combine the two plots #-#-#
CombPlot <- arrangeGrob(IBApSp,scatter,
                       widths = c(4,4),
                       heights = c(1),
                       ncol = 2,
                       nrow = 1)
plot(CombPlot)

setwd(plotpath)
ggsave("Fig S19 Percentage of IBAs left per species and IBA area RCP 45 trigger MaxKap habitat.tiff",CombPlot,width=12, height=4, unit="in", dpi=300, bg="transparent")


#-#-# Extract summary numbers per manuscript (from ensemble files generated in script S7) #-#-#
summarypath <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_species_changes_lists/"
Plot_files <- list.files(summarypath, pattern = "_Ensemble")
Plot_file <- read.csv(paste0(summarypath,Plot_files[3]))
head(Plot_file)
nrow(Plot_file)

Plot_file <- Plot_file[c("IBA", "currentRich", "stablesp")]
Plot_file$PropLeft <- Plot_file$stablesp/(Plot_file$currentRich/100)
head(Plot_file)

## Number of IBAs that are projected to currently contain suitable habitat for trigger species
Suit_file <- subset(Plot_file, currentRich > 0)
nrow(Suit_file)
nrow(subset(Plot_file, currentRich == 0))

## IBAs retain xx% of their species
PropLeft_file <- subset(Suit_file, PropLeft >= 90)
nrow(PropLeft_file)

nrow(PropLeft_file)/(nrow(Suit_file)/100)

## Check correlation area IBA and proportion species remaining 
Testdata <- PlotData[c("Area","PropLeft")]
Testdata$LogArea <- log(Testdata$Area)
cor <- cor.test(x = Testdata$LogArea, y = Testdata$PropLeft,  method = c("spearman"))
cor

