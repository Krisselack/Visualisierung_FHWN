# R script 
# File: Combine_Polygon.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date: 17.06.2021
# Copyright (C) 2021
# Description: 

# von jupyter weg zum direkten R-Script
library(sf)
library(rgdal)
library(ggplot2)
library(dplyr)
library(writexl)
library(raster)

libs <- c("rgdal", "maptools", "gridExtra")
lapply(libs, require, character.only = TRUE)
# Daten einlesen 
# Daten von opendata für LV Visualisierung 
preise <- read.csv2("./data/Preise.csv", fileEncoding = "windows-1252")

# Daten für Kartendarstellung - KGS mit Bezirken 

# R JOIN 
bezs <- readOGR("./data/BEZIRKSGRENZEOGDPolygon.shp", encoding = "windows-1252" )
saveRDS(bezs, "./shiny/BEZ_Shape.RDS")

kgs <- readOGR("./data/Wien_BEV_VGD_250_LAM.shp")
kgs_trans <- spTransform(kgs, CRS("+proj=longlat +datum=WGS84"))

subs_union <- intersect(kgs_trans, bezs)
subs_union$KG.Code <-  substring(as.character(subs_union$KG_NR), first=2)
table(subs_union$BEZ) 
saveRDS(subs_union, "./shiny/shape_corr.RDS")

kgs <- read.csv("./data/Wikipedia_KG.csv")
kgs_merge <- data.frame("KG.Code" = kgs$KG.Nr., "BEZ" = substr(kgs$Bezirk, 1, 2), stringsAsFactors = FALSE)

preise_mit_Bezirk <- merge(preise, kgs_merge,  by = "KG.Code", how="left")
table(preise_mit_Bezirk$BEZ)

saveRDS(preise_mit_Bezirk, "./shiny/dats2.RDS")



## # Aufbereitung Geodaten: 
## # QGIS Join KG Wien mit Bezirksdaten Wien 
## # Anpassen Koordinatensysteme - Join Attribute by location - export
## kgs_wien <- st_read("Geodata_Output/Bezirke_KG_Wien_31256.shp")
## # Aufbereiten der KG-Codes für Merging
## kgs_wien$KG.Code <- substring(as.character(kgs_wien$KG_NR), first=2)

## kgs_wien %>%
##   filter(BEZNR == 3)

## preise_mit_Bezirk <- merge(preise, kgs_wien[, c("KG.Code", "BEZ")], by = "KG.Code", how="left")

## table(preise_mit_Bezirk$BEZ)


## write_xlsx(preise_mit_Bezirk, "./data/Data_mit_Bezirk.xlsx")
