# R script 
# File: ui.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021
# Description: 

ui <- bootstrapPage(theme = shinytheme("flatly"),

  tags$head(
    HTML("<link rel='stylesheet' type='text/css' href='adjust.css'>"),
    HTML("<meta name='viewport', content='width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no' />")
  ), 
                windowTitle="Visualisierungstechniken",
                title = "Datensatz Stadt Wien - Preisentwicklung ", 
                fluidRow(
                #  column(12,"Datensatz Stadt Wien - Preisentwicklung "),
                  #                             column(3,img(src='blpLogo.jpg', align="center",width=400,
                  #                                         style="margin-bottom:20px;")))

                          column(width=2,                         
                                wellPanel( radioButtons("Bez", 
                                                        label = "Bezirk",
                                                        choices =  levels(shapeData@data$NAMEK_RZ),
                                                        selected = "I. Innere Stadt"),
                                          uiOutput("Bez")
                                          )), 
                          column(width=10, leafletOutput("mymap"), height= "150%", 
                                )
                         )
                )

## fluidRow(h2("Enteker Flächendaten"),
##          h3("Zusammenfassung (kurz)"),
##          column(3,
##                 tableOutput("fldaten_ZF"),
##                 downloadButton('download_ent',"csv-Download der Auswahl")
##             #    downloadButton("saveplot", "png-Download der Abbildung")
##                 ),
##          column(12,
##                 plotOutput("gemzahl_plot")
##                 ),
##          h3("Darstellung der ausgewählten Daten"),
##          DT::dataTableOutput("fldaten")

