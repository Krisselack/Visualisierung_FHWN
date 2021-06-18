# R script 
# File: global.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021
# Description: 

library(shiny)
library(shinythemes)
library(leaflet)
library(plyr)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(readxl) 

palette <- colorRampPalette(colors=c("#deebf7", "#3182bd"))


#shapeData <- readOGR("../data/BEZIRKSGRENZEOGDPolygon.shp")
#shapeData <- spTransform(shapeData, CRS("+proj=longlat +datum=WGS84"))
#saveRDS(shapeData, "./shapeData_bez.RDS")

# shapeData <- spTransform(shapeData, CRS("+init=epsg:4326"))
# dats <- read_xlsx("./data/Data_mit_Bezirk.xlsx")
# BinshapeData
# changing code for data import on shiny.os 
shapeData <- readRDS("./BEZ_Shape.RDS")
dats <-  readRDS("./dats2.RDS")

# Darstellung Kaufpreis
dats$Kaufjahr <- as.integer(unlist(lapply(strsplit(as.character(dats$Erwerbsdatum), "\\."), "[", 3) ))



kaufpreis <- dats %>%
  group_by(Katastralgemeinde, Kaufjahr) %>%
  summarize( BEZNR = head(as.integer(BEZ), 1),
            Kaufpreis = mean(Kaufpreis.., na.rm=TRUE)) %>%
  filter(Kaufjahr > 1800 & Kaufjahr < 2100 & Kaufpreis > 50000 ) %>%
  na.omit() %>%
  arrange(desc(Kaufpreis)) 

kaufpreis

kaufpreis_legende <- kaufpreis %>%
  group_by(BEZNR) %>%
  summarize(Kaufpreis = mean(Kaufpreis, na.rm=TRUE))

order(kaufpreis_legende$Kaufpreis, decreasing=TRUE)
kaufpreis_legende$Kaufpreis


vergl <- match(order(kaufpreis_legende$Kaufpreis, decreasing=TRUE), shapeData@data$BEZNR ) 
# levels(shapeData@data$NAMEK_RZ)

legcols <- palette(nlevels(shapeData@data$NAMEK_RZ))[order(kaufpreis_legende$Kaufpreis, decreasing=TRUE)]
#test <- cbind(legcols, order(kaufpreis_legende$Kaufpreis, decreasing=TRUE))
#test

# legcols <- c(1:23)[order(kaufpreis_legende$Kaufpreis, decreasing=TRUE)]
# order(kaufpreis_legende$Kaufpreis, decreasing=TRUE)
colmap <-  legcols[vergl]


# palmap <-   data.frame(Bez = levels(shapeData@data$NAMEK_RZ),
 #                       color = legcols[vergl])

# colmap <-  as.character(mapvalues(shapeData@data$NAMEK_RZ, palmap$Bez, as.character(palmap$color)))



# runApp()
# library(rsconnect)
# deployApp()
