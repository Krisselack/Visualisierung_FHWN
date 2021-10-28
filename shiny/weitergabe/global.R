# R script 
# File: global.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021

# Hier werden die Pakete importiert und (statische) Daten aufbereitet
# Im server-script werden die Daten dynamisch (interaktiv) gestaltet
# Im Ui-Script wird das Layout erstellt 

# Package  Imports 
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
library(htmltools)

# Farbraum 
palette <- colorRampPalette(colors=c("#deebf7", "#3182bd"))

# Daten wurden als RDS aufbereitet (für Deployment) 
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

labels <- paste("<center><p>", shapeData@data$NAMEK_RZ,
                paste("</p><p> \u00D8 Kaufpreis -",
                      format(round(kaufpreis_legende$Kaufpreis[shapeData@data$BEZNR],  0), big.mark=".",
                             decimal.mark = ","), " \u20ac </p</center>"))

# zum Laufen bringen:
# in Code-Verzeichnis gehen;
# R starten: 

# library(shiny) 
# runApp()
