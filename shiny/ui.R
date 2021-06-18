# R script 
# File: ui.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021
# Description: 

ui <- bootstrapPage(theme = shinytheme("flatly"),

  tags$head(
    includeCSS("./adjust.css"), 
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
                          column(width=6, leafletOutput("mymap"), height= "150%", 
                                )
                         ), 
                
fluidRow(
  selectInput("Kat", "Kategorie:",
                choices = levels(dats2$zuordnung)),
  ),

fluidRow(h2("Darstellung pro Katastrallgemeinde"),
         h3("Kaufpreis pro Katastralgemeinde "),
         column(12,
                plotOutput("boxplot")
                )
         ))#         h3("Darstellung der ausgewählten Daten"),
  #       DT::dataTableOutput("fldaten")

