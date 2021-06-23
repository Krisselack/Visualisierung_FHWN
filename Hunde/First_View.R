# R script 
# File: First_View.R
# Author: Christian Brandst√§tter 
# Contact: bran.chri@gmail.com
# Date: 23.06.2021
# Copyright (C) 2021
# Description: 
library(dplyr) 
library(writexl)

hundat <- read.csv2("./hunde-wien.csv", skip = 1, stringsAsFactors=FALSE, fileEncoding = "Windows-1252")

metadat <- read.csv("./Meta_Wien.csv", stringsAsFactors=FALSE, , fileEncoding = "UTF-8")
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

head(hundat2)

# Aufbereitung 
hundat3 <- hundat2 %>%
  select(-NUTS1, -NUTS2, -NUTS3, - SUB_DISTRICT_CODE, - Nr) %>%
  rename("Bezirkscode" = DISTRICT_CODE, "PLZ" = Postal_CODE , "Rasse" = `Dog.Breed`,
         "Anz_Kampfhund" = Kampfhund,  "Bezirksname" = Gemeinde.bezirk,
         "Fl‰che_m2" =  Flaeche, "Besch‰ftigte" =  `Besch‰f.tigte_2016`) %>%
  mutate("Georef_PBI" = paste("Ausria", PLZ)) 

writexl::write_xlsx(hundat3, "./Hundedaten.xlsx")

head(hundat3)

summaryset <- hundat3 %>% group_by(Georef_PBI) %>%
  summarize("Hunde_Bezirk" = sum(Anzahl),
            "Kampfhunde_Bezirk" = sum(Anz_Kampfhund),
            "Bez_Name" = head(Bezirksname, 1),
            "Bez_Fl‰che" = head(Fl‰che_m2, 1),
            "Bez_Besch" = head(Besch‰ftigte, 1),
            "Bez_EINW" = head(Einw_2021, 1),
            "Bez_BEV_DICHTE" = head(Einw_km2, 1)
            ) %>%
  mutate(Hunde_pro_m2 =  Hunde_Bezirk / Bez_Fl‰che,
         Kampfhunde_pro_m2 = Kampfhunde_Bezirk / Bez_Fl‰che,
         Hund_pro_EW = Hunde_Bezirk / Bez_EINW,
         Kampfhund_pro_EW = Kampfhunde_Bezirk / Bez_EINW
         )

writexl::write_xlsx(summaryset, "./Hundedaten_Gruppe.xlsx")
