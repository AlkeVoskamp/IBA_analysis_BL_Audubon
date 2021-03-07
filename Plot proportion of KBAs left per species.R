
## 968 trigger species

#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 

library(ggplot2)
library(dplyr)
library(viridis)
library(grid)
library(gridExtra)


#-#-# Set working directionary and get the files
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/With habitat/PA occurrences per species rcp45 50 habitat/")
AllSpecies <- list.files()


GetProp <- lapply(AllSpecies,function(x){
    Species <- paste0(strsplit(x,split="_")[[1]][1],"_",strsplit(x,split="_")[[1]][2])
    print(Species)
    Onesp <- get(load(x))
    All <- sum(as.numeric(as.character(Onesp$current)))
    Both <- sum(as.numeric(as.character(Onesp$both)))
    PropLeft <- Both/(All/100)
    Result <- as.data.frame(cbind(Species,All,Both,PropLeft))
    return(Result)
})

PropTable <- do.call(rbind,GetProp)
head(PropTable)
str(PropTable)

nrow(PropTable)
PropTable$PropLeft <- as.numeric(as.character(PropTable$PropLeft))
fiftyPercLeft <- subset(PropTable,PropLeft >= 50)
nrow(fiftyPercLeft)
nrow(fiftyPercLeft)/(nrow(PropTable)/100)

## Remove those species that do not occur in a KBA currently
PropTable[2:4] <- sapply(PropTable[2:4],as.character)
PropTable[2:4] <- sapply(PropTable[2:4],as.numeric)
head(PropTable)

PropTable <- subset(PropTable,All > 0)
nrow(PropTable)
# 279 species are not currently occuring in a KBA off all species 85 of the trigger species


hist(as.numeric(as.character(PropTable$PropLeft)))

mean(na.omit(as.numeric(as.character(PropTable$PropLeft))))
median(na.omit(as.numeric(as.character(PropTable$PropLeft))))

col <- rep("lightgrey",11)
colII <- rep("black",11)


KBApSp <- ggplot(PropTable, aes(x=as.numeric(as.character(PropLeft)))) +
  geom_histogram(binwidth=10, fill = col, colour = colII) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme(legend.position = "none") +
  theme(axis.title=element_text(size=12))+ # Change font size legend
  theme(axis.text=element_text(size=12))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
  theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
  labs(x="% of climatically suitable KBAs left2050", y="# of species", title="")+ # Remove axis titles
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ggtitle("a)    RCP45")+ 
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0))
KBApSp


#-#-# Size of KBA vs proportion of species left #-#-#
#-#-# Load the KBA polygons and set to South American focal countries #-#-#
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
  colnames(data) <- c("KBA","Country","Area")
  return(data)
})

AreaData <- as.data.frame(do.call(rbind,Area))
head(AreaData)


#-#-# Calculate the proportion of species left per KBA #-#-#
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/With habitat/PA occurrence changes rcp45 50 habitat/")
AllFiles <- list.files()

PropLeft <- lapply(AllFiles,function(x){
    print(x)
    KBA <- strsplit(x,split="_")[[1]][1]
    data <- get(load(x))
    data <- as.data.frame(data)
    current <- data$currentlist
    future <- data$futurelist
    StillThere <- length(intersect(current, future))
    PropLeft <- StillThere/(length(current)/100)
    data <- cbind(KBA,PropLeft)
    return(data)
})

PropLeft <- as.data.frame(do.call(rbind,PropLeft))
head(PropLeft)

PlotData <- merge(AreaData,PropLeft,by=c("KBA"))
head(PlotData)
str(PlotData)

PlotData[3:4] <- sapply(PlotData[3:4],as.character)
PlotData[3:4] <- sapply(PlotData[3:4],as.numeric)
head(PlotData)

colPal <- c("purple","blue","red","green","yellow")

plot(PropLeft~Area,data=PlotData)
abline(lm(PropLeft~Area,data=PlotData))
summary(lm(PropLeft~Area,data=PlotData))

scatter <- ggplot(PlotData, aes(x=log(Area), y=PropLeft)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(legend.position = c(0.8, 0.9)) + # Positioning the legend   
  theme(legend.text=element_text(size=10))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm")) + # Change size of legend key
  theme(panel.background=element_rect(fill='#FFFFFF',colour="white"))+ # Remove the background
  theme(axis.title.x=element_text(vjust=-1.5, size = 10)) +
  theme(axis.title.y=element_text(angle=90, vjust= 2, size = 10)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  labs(x="log(Size of the KBA)", y="% of species left", title="",vjust= -0.5)+ # Remove axis titles
  ggtitle("b) ") + 
  theme(plot.title = element_text(size = 12,face="bold",hjust = 0))

plot(scatter)


#-#-# Combine the two plots #-#-#
CombPlot <- arrangeGrob(KBApSp,scatter,
                       widths = c(4,4),
                       heights = c(1),
                       ncol = 2,
                       nrow = 1)



plot(CombPlot)

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Manuscript drafts/Submission/Final figures/")
ggsave("Percentage of KBAs left per species and KBA area rcp 45 50 habitat.tiff",CombPlot,width=12, height=6, unit="in", dpi=300, bg="transparent")

