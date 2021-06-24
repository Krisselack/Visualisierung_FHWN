# R script 
# File: global.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021

library(shiny)
library(shinydashboard)
library(leaflet)
library(plyr)
library(dplyr)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(readxl) 
library(htmltools )
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
dats <- dats %>% rename(Kaufpreis = 'Kaufpreis..', Kaufpreis_rel = 'X..m..Gfl.')
dats2 <- dats %>%   filter(Kaufjahr > 1800 & Kaufjahr < 2050 & Kaufpreis > 50000 ) 


kaufpreis <- dats2 %>%
  group_by(Katastralgemeinde, Kaufjahr) %>%
  summarize( BEZNR = head(as.integer(BEZ), 1),
            Kaufpreis = mean(Kaufpreis, na.rm=TRUE)) %>%
  na.omit() %>%
  arrange(desc(Kaufpreis)) 

kaufpreis_legende <- kaufpreis %>%
  group_by(BEZNR) %>%
  summarize(Kaufpreis = mean(Kaufpreis, na.rm=TRUE)) %>%
  arrange(desc(Kaufpreis))

kaufpreis_legende$farbe <- rev(palette(nlevels(shapeData@data$NAMEK_RZ)))
  
# mapping für boxplot 
mapper = data.frame("namen" = unique(as.character(shapeData@data$NAMEK_RZ)), 
                    nummer = unique(as.character(shapeData@data$BEZNR)))
mapper$nummer <- as.integer(as.character(mapper$nummer ))

legcols <- mapvalues(shapeData@data$BEZNR, kaufpreis_legende$BEZNR, kaufpreis_legende$farbe)

shapeData@data$farbs <- legcols

#legcols <-  kaufpreis_legende$farbe[match(kaufpreis_legende$BEZNR, shapeData@data$BEZNR)]
# colmap <-  rev(legcols) 

# shapeData@data$NAMEK_RZ <- as.character(shapeData@data$NAMEK_RZ)

labels <- paste("<center><p>", shapeData@data$NAMEK_RZ,
                paste("</p><p> \u00D8 Kaufpreis -",
                      format(round(kaufpreis_legende$Kaufpreis[shapeData@data$BEZNR],  0), big.mark=".",
                             decimal.mark = ","), " \u20ac </p</center>"))

#  lapply(labels, htmltools::HTML) 

## plot(NULL, xlim=c(0,length(legcols)), ylim=c(0,1), 
##     xlab="", ylab="", xaxt="n", yaxt="n")
## rect(0:(length(legcols)-1), 0, 1:length(legcols), 1, col=legcols)
##  text(1:length(legcols)-0.5, y = 0.5, order(kaufpreis_legende$Kaufpreis, decreasing=TRUE)) 

## plot(NULL, xlim=c(0,length(colmap)), ylim=c(0,1), 
##     xlab="", ylab="", xaxt="n", yaxt="n")
## rect(0:(length(colmap)-1), 0, 1:length(colmap), 1, col=colmap)
## text(1:length(colmap)-0.5, y = 0.5, shapeData@data$BEZNR) 

# runApp()
# library(rsconnect)
# deployApp()
