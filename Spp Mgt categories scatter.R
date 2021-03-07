#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#           Supplementary plot management categories          #
#  Scatterplot management categories split into individual    #
#                         scenarioes                          #
#                       Februray 2021                         #
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
Col_em_file <- list.files(File_path, pattern = "Ensemble")


#-#-# Create tile plots for supplement scatterplot mgt classes #-#-#
#-#-# Set the colour scheme #-#-#
colPal <- c("purple","blue","red","green","yellow")

Plot_function <- lapply(Col_em_file, function(x){
  
  print(x)

  #-#-# Load the plot data #-#-#
  Sc_plot_data <- read.csv(paste0(File_path, x))
  Sc_plot_data$propCol <- log(Sc_plot_data$propCol)
  Sc_plot_data$propEm <- log(Sc_plot_data$propEm)


  #-#-# Change name for management categories #-#-#
  Sc_plot_data$Category <- 0
  Sc_plot_data$Category[Sc_plot_data$ClassE == 1] <- "Increasing diversification"
  Sc_plot_data$Category[Sc_plot_data$ClassD == 1] <- "Increasing specialization"
  Sc_plot_data$Category[Sc_plot_data$ClassC == 1] <- "High turnover"
  Sc_plot_data$Category[Sc_plot_data$ClassB == 1] <- "Increasing value"
  Sc_plot_data$Category[Sc_plot_data$ClassA == 1] <- "High persistence"
  Sc_plot_data <- na.omit(Sc_plot_data)

  #-#-# Set plot heading #-#-#
  RCP <- strsplit(x, split = "_")[[1]][8]
  Thres <- strsplit(x, split = "_")[[1]][10]
  Thres <- strsplit(Thres, split = ".csv")
  
  Name <- paste0(RCP, " ", Thres)
  
    #-#-# Scatter plot mgt categories #-#-#
    scatter <- ggplot(Sc_plot_data, aes(x=log(propCol), y=propEm))+
        geom_point(aes(fill=Category),colour="black",pch=21, size=1)+
        scale_fill_manual(values=c("purple3","firebrick3","gold","yellowgreen","dodgerblue3"))+
        theme(plot.margin = unit(c(1,1,1,1), "cm"))+
        theme(legend.position = "none")+ # Positioning the legend   
        theme(panel.background=element_rect(fill='#FFFFFF',colour="white"))+ # Remove the background
        scale_x_continuous(breaks = c(-6,-4,-2,0), labels = c(-6,-4,-2,0)) +
        theme(axis.line.x = element_line(color="black", size = 0.5),
            axis.line.y = element_line(color="black", size = 0.5)) +
        theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm")) +
        labs(x="", y="", title="",vjust= -0.5)+ # Remove axis titles
        ggtitle(Name) + 
        theme(plot.title = element_text(size = 12,hjust = 0))
    plot(scatter)

  return(scatter)
})


#-#-# Prepaire the legend #-#-# 
Sc_plot_data <- read.csv(paste0(File_path, Col_em_file[1]))

#-#-# Change name for management categories #-#-#
Sc_plot_data$Category <- 0
Sc_plot_data$Category[Sc_plot_data$ClassE == 1] <- "Increasing diversification"
Sc_plot_data$Category[Sc_plot_data$ClassD == 1] <- "Increasing specialization"
Sc_plot_data$Category[Sc_plot_data$ClassC == 1] <- "High turnover"
Sc_plot_data$Category[Sc_plot_data$ClassB == 1] <- "Increasing value"
Sc_plot_data$Category[Sc_plot_data$ClassA == 1] <- "High persistence"
Sc_plot_data <- na.omit(Sc_plot_data)

leg1 <- ggplot(Sc_plot_data, aes(x=log(propCol), y=propEm))+
  geom_point(aes(fill=Category),colour="black",pch=21, size=1)+
  scale_fill_manual(values=c("purple3","firebrick3","gold","yellowgreen","dodgerblue3"))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
  theme(legend.position = c(0.5, 0.5))+ # Positioning the legend   
  theme(legend.text=element_text(size=10))+ # Change font size legend
  theme(legend.key.size = unit(0.5, "cm"),legend.key.height = unit(0.5, "cm"))+
  theme(plot.margin=unit(c(2,2,2,2), "cm"))
legScat <- g_legend(leg1)
plot(legScat)


#-#-# Combine final plot #-#-#
CombPlot <- grid.arrange(arrangeGrob(Plot_function[[1]],Plot_function[[2]],Plot_function[[3]],Plot_function[[4]],Plot_function[[5]],Plot_function[[6]],
                        Plot_function[[7]],Plot_function[[8]],legScat,Plot_function[[10]],Plot_function[[11]],Plot_function[[12]],
                        widths = c(1,1,1,1,1,1),
                        heights = c(0.5,0.5),
                        ncol = 6,
                        nrow = 2,
                        bottom = textGrob("Proportion of projected colonists", rot = 0, vjust = 0.5),
                        left = textGrob("Proportion of projected emmigrants", rot = 90, vjust = 0.5)))
plot(CombPlot)

#-#-# Saver the result plots #-#-#
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/IBA_analysis_BL_Audubon/Main_manuscript_plots_final/")
ggsave("Supplement_scatterplot_all_scenarioes.jpeg",CombPlot,width=10, height=4, unit="in", dpi=600, bg="transparent")


