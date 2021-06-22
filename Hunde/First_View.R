# R script 
# File: First_View.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date: 23.06.2021
# Copyright (C) 2021
# Description: 
library(dplyr) 

hundat <- read.csv2("./hunde-wien.csv", skip = 1, stringsAsFactors=FALSE, fileEncoding = "Windows-1252")
metadat <- read.csv("./Meta_Wien.csv", stringsAsFactors=FALSE)
hundat$Anzahl <- as.integer(hundat$Anzahl)

# Kamphunde 
# https://www.wien.gv.at/gesellschaft/tiere/hundefuehrschein/verpflichtend.html
kampfs <- c("Bullterrier", 
   "Staffordshire Bullterrier", 
   "American Staffordshire Terrier" ,
   "Mastino Napoletano", 
   "Mastin Espanol" , 
   "Fila Brasileiro", 
   "Mastiff", "Bullmastiff" ,
   "Tosa Inu", 
    "Pit Bull Terrier" ,
    "Rottweiler", 
  "Dogo Argentino")


kampfs %in% hundat$Dog.Breed 

sort(unique(hundat$Dog.Breed) )

kampfres <- NULL 
for(i in kampfs){
  print(i)
  test <- grepl(i, hundat$Dog.Breed, fixed=TRUE)
  kampfres <- cbind(kampfres, test)
}

hundat$Kampfhund <- ifelse(
  apply(kampfres, 1, function(x) any(x==TRUE)) ,
  1, 0)

hundat2 <- hundat %>%
  left_join(metadat, by = c("DISTRICT_CODE" =  "Code"))

head(hundat2)

kampfhunde <- hundat2 %>%
  group_by(DISTRICT_CODE) %>%
  summarize(kmpf_ew = sum(Kampfhund) /  head(Einw_2021, 1) ,
            kmpf_fl = sum(Kampfhund) /  head(Einw_km2, 1) )


plot(x = 1:23, pull(kampfhunde[,2]), ylab = "Kampfhund / Einwohner")
text(x = 1:23, pull(kampfhunde[,2]), labels = 1:23)

plot(x = 1:23, pull(kampfhunde[, 3]), ylab = "Kampfhund / km2")
text(x = 1:23, pull(kampfhunde[,3]), labels = 1:23)

