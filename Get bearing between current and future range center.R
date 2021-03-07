#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#            Calculate bearing between range centroids              #
#               Current and future range centroids                  #
#                 Climate change rcp26 and rcp85                    #
#                          November 2017                            #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

rm(list=ls())

library(geosphere)
library(circular)
library(plyr)
library(CircStats)

#-#-# Bearing between range centers #-#-#
setwd("/Users/alkevoskamp/Documents/BirdLife/South America manuscript/Data/Result files/")

Coords <- read.csv("Range centroids and range extent_km and perc_RCP85_50_habitat.csv")
Coords <- Coords[c(3,4,5,7,8)]
head(Coords)
Coords <- na.omit(Coords)
nrow(Coords)
#8454

n <- Coords[384,]
  
DegData <- data.frame(name= character(0), bearing= numeric(0))
head(DegData)

for(i in 1:nrow(Coords)){
  
  print(i)
  
  n <- Coords[i,]
  name <- n[1]
  
  cur  <- cbind(n[2],n[3])
  colnames(cur) <- c("x","y")

  fut <- cbind(n[4],n[5])
  colnames(fut) <- c("x","y")
  
  bear <- bearing(cur,fut)
  print(bear)
  
  alldata <- (c(name=name,bearing=bear))
  alldata <- as.data.frame(alldata)
  DegData <- rbind(DegData,alldata)

}

CDdata <- DegData

# Add directions
CDdata$Direction <- 1
CDdata$Direction[CDdata$bearing > 0 & CDdata$bearing <= 45] <- "NNE"
CDdata$Direction[CDdata$bearing > 45 & CDdata$bearing <= 90] <- "ENE"
CDdata$Direction[CDdata$bearing > 90 & CDdata$bearing <= 135] <- "ESE"
CDdata$Direction[CDdata$bearing > 135 & CDdata$bearing <= 180] <- "SSE"

CDdata$Direction[CDdata$bearing > -180 & CDdata$bearing <= -135] <- "SSW"
CDdata$Direction[CDdata$bearing > -135 & CDdata$bearing <= -90] <- "WSW"
CDdata$Direction[CDdata$bearing > -90 & CDdata$bearing <= -45] <- "WNW"
CDdata$Direction[CDdata$bearing > -45 & CDdata$bearing <= -1] <- "NNW"

head(CDdata)
  
write.csv(DegData,"Bearing between range centers rcp85 50_habitat.csv")  

Allsp <- nrow(CDdata)
PercNNE <- sum(CDdata$Direction == "NNE")/(Allsp/100)
PercENE <- sum(CDdata$Direction == "ENE")/(Allsp/100)
PercESE <- sum(CDdata$Direction == "ESE")/(Allsp/100)
PercSSE <- sum(CDdata$Direction == "SSE")/(Allsp/100)
PercSSW <- sum(CDdata$Direction == "SSW")/(Allsp/100)
PercWSW <- sum(CDdata$Direction == "WSW")/(Allsp/100)
PercWNW <- sum(CDdata$Direction == "WNW")/(Allsp/100)
PercNNW <- sum(CDdata$Direction == "NNW")/(Allsp/100)

PercMovingNorth <- PercNNW + PercNNE
PercMovingSouth <- PercSSW + PercSSE
Allsp <- nrow(CDdata)
NoMovingNorth <- sum(CDdata$Direction == "NNW") + sum(CDdata$Direction == "NNE")
NoMovingSouth <- sum(CDdata$Direction == "SSW") + sum(CDdata$Direction == "SSE")
PercMovingNorth
PercMovingSouth
Allsp
NoMovingNorth
NoMovingSouth


