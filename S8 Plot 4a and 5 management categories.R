#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#             Plot the management category results            #
#              Scatterplot emmigrants vs colonists            #
#           Barchart management categories per country        #
#                       September 2020                        #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#-#-# Clear memory #-#-#
rm(list=ls(all=TRUE)) 


#-#-# Load the libraries #-#-#
library(rgdal)
library(sp)
library(maptools)
library(spatialEco)
library(rgeos)
library(sf)
library(ggplot2)
library(grid)
library(gridExtra)


#-#-# Function to extract legend from plot #-#-#
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


#-#-# Set the filepaths #-#-#
File_path <- "/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Revision/Data/IBA_management_classes/"
Col_em_file <- list.files(File_path,pattern = "trigger_species_changes_Ensemble_rcp45_2050_MaxKap")


#-#-# Load the plot data #-#-#
Sc_plot_data <- read.csv(paste0(File_path, Col_em_file))
Sc_plot_data$propCol <- Sc_plot_data$propCol 
Sc_plot_data$propEm <- Sc_plot_data$propEm
Sc_plot_data <- na.omit(Sc_plot_data)


#-#-# Set the colour scheme #-#-#
colPal <- c("purple","blue","red","green","yellow")


#-#-# Scatter plot mgt categories #-#-#
scatter <- ggplot(Sc_plot_data, aes(x=log(propCol), y=propEm))+
  geom_point(aes(fill=Category),colour="black",pch=21, size=1)+
  scale_fill_manual(values=c("purple3","firebrick3","gold","yellowgreen","dodgerblue3"))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position = "none")+ # Positioning the legend   
  theme(panel.background=element_rect(fill='#FFFFFF',colour="white"))+ # Remove the background
  theme(axis.title.x=element_text(vjust=-1.5, size = 14)) +
  theme(axis.title.y=element_text(angle=90, vjust= 2, size = 14)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  labs(x="Proportion of projected colonists", y="Proportion of projected emmigrants", title="",vjust= -0.5)+ # Remove axis titles
  ggtitle("a) RCP 45") + 
  theme(plot.title = element_text(size = 15,face="bold",hjust = 0))
plot(scatter)


#-#-# Prepaire the legend#-#-#
leg1 <- ggplot(Sc_plot_data, aes(x=log(propCol), y=propEm))+
  geom_point(aes(fill=Category),colour="black",pch=21, size=1.5)+
  scale_fill_manual(values=c("purple3","firebrick3","gold","yellowgreen","dodgerblue3"))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position = c(0.5, 0.5))+ # Positioning the legend   
  theme(legend.text=element_text(size=10))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))
legScat <- g_legend(leg1)
plot(legScat)


#-#-# Combine final plot #-#-#
CombPlot <- arrangeGrob(scatter,legScat,
                         widths = c(2,1),
                         heights = c(1),
                         ncol = 2,
                         nrow = 1)
plot(CombPlot)

#-#-# Save the result plots #-#-#
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/")
ggsave("Scatterplot_trigger_cat_RCP_45_50_MaxKap.jpeg",CombPlot,width=6, height=4, unit="in", dpi=600, bg="transparent")


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#-#-# Proportion per country plot #-#-#
mgt <- read.csv(paste0(File_path,"Management_classes_IBA_trigger_species_changes_Ensemble_rcp45_2050_MaxKap.csv"))

mgt$Category <- 0
mgt$Category[mgt$ClassE == 1] <- "Increasing diversification"
mgt$Category[mgt$ClassD == 1] <- "Increasing specialization"
mgt$Category[mgt$ClassC == 1] <- "High turnover"
mgt$Category[mgt$ClassB == 1] <- "Increasing value"
mgt$Category[mgt$ClassA == 1] <- "High persistence"
mgt <- na.omit(mgt)
head(mgt)

MgtSum <- mgt[c("country","Category")]
head(MgtSum)

## Summarize data
MgtTable <- MgtSum %>%
            dplyr::group_by(country,Category) %>%
            dplyr::summarize(n())

MgtTable <- as.data.frame(MgtTable)
colnames(MgtTable) <- c("country","Category","count")
head(MgtTable)

## Add proportions
countries <- unique(MgtTable$country)

PropTab <- lapply(countries,function(x){
  print(x)
  oneC <- subset(MgtTable,country == x)
  OneP <- sum(oneC$count)/100
  oneC$Prop <- oneC$count/OneP
  oneC$Total <- sum(oneC$count)
  return(oneC)
})

PropTab <- do.call(rbind,PropTab)
head(PropTab)

CountryMgt <- ggplot(PropTab, aes(fill=Category,colour=Category, y=Prop, x=country)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(values=c("purple3","firebrick3","gold","yellowgreen","dodgerblue3")) +
    scale_colour_manual(values=c("black","black","black","black","black")) +
    theme(axis.title=element_text(size=14))+ # Change font size legend
    theme(axis.text.x=element_text(size=12,angle=60,vjust = 1, hjust=1))+ # Change font size legend
    theme(axis.text.y=element_text(size=12))+ # Change font size legend
    theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+ # Change size of legend key
    theme(legend.text=element_text(size=12)) + # Change legend text size
    theme(panel.background=element_rect(fill='white',colour="white"))+ # Remove the background
    labs(x="", y="Proportion of KBAs in CCAS category", title="")+ # Remove axis titles
    theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
    geom_text(aes(x = country, y = 105, label = Total), vjust = 1,show.legend=F) +
    ggtitle("(a)") + 
    theme(plot.title = element_text(size = 18,face="bold",hjust = 0)) 
 
plot(CountryMgt)

setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/Main manuscript/")
#setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/Spp/")
ggsave("Mgt proportion per country trigger RCP 45 MaxKap.jpeg",CountryMgt,width=14, height=6, unit="in", dpi=600, bg="transparent")

