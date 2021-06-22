# R script 
# File: ui.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021
# Description: 

ui <- dashboardPage(skin = "blue", 
                    
                    dashboardHeader(title = "Datensatz Stadt Wien - Preisentwicklung "
                                    ),
                    
                    title="Visualisierungstechniken",
                    
                    
                    dashboardSidebar(width = 450,
                                     h3("In diesem Bereich  kann man Daten filtern."),
                                       h3("Und zwar: den Bezirk, die Kategorie, \n und den Zeitrahmen der Transaktionen."), 

                                     wellPanel( radioButtons("Bez", 
                                                             label = "Bezirk",
                                                             choices =  levels(shapeData@data$NAMEK_RZ)),
                                               #   selected = "XIII. Hietzing"),
                                               
                                               style = "color: darkblue;" ,
                                               selectInput("Kat", "Kategorie - Negativ-Auswahl durch Druck auf ENTF",
                                                           multiple = TRUE, 
                                                           choices = levels(dats2$zuordnung), 
                                                           selected= unique(dats2$zuordnung)),

                                               sliderInput("Zeitraum",
                                                           "Kaufjahr von bis",
                                                           min = min(dats2$Kaufjahr, na.rm=TRUE),
                                                           max = max(dats2$Kaufjahr, na.rm=TRUE), 
                                                           value=c(min(dats2$Kaufjahr, na.rm=TRUE), max(dats2$Kaufjahr, na.rm=TRUE)),
                                                           ticks = FALSE, 
                                                           sep = "", step = 1),
                                               
                                               h3("Die Datengrundlage sind auf opendata veröffentlichte Immobilientransaktionen der Stadt Wien."),

                                               tags$a(href="https://www.data.gv.at/katalog/dataset/kaufpreissammlung-liegenschaften-wien",
                                                      "Datensatz")
                                               )

                                     ), 
                    
                    dashboardBody(
                      fluidRow(
                        tags$head(
                          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                        ), 
                        column(width = 5,
                               
                               fluidRow(       
                                 box(width=12,  leafletOutput("mymap", height = 475), height= 500), 

                                 box(width = 12,  title = "Grundstücksfläche - Darstellung pro Katastralgemeinde",
                                     height = 475, plotOutput("colplot")))), 
                        
                        column(width = 5, 
                               tabBox(width=12, height = 500, #selected= "Kaufpreis_absolut",
                                      title = "Darstellung pro Katastralgemeinde",
                                      tabPanel("Kaufpreis_absolut", plotOutput("boxplot")),
                                      tabPanel("Kaufpreis_relativ", plotOutput("boxplot2"))
                                      ),

                               box(width=12,  title = "Anzahl pro Kategorie (auf Bezirksebene)",
                                   height = 475, plotOutput("barplot"))),
                        
                        column(width=2,
                               box(width=12,
                                   h2("oben"), 
                                   h4("In der ersten Abbildungszeile wird LINKS eine  Wien-Karte nach Bezirk dargestellt. Die Click-Auswahl ist mit der linken Button-Auswahl verknüpt. Im  RECHTEN Fenster werden Box-Plots dargestellt, die den absoluten und relativen Kaufwert pro Katastralgemeinde auf Bezirksebene (in Tabs) darstellen."),
                                   br(), br(), br(),  br(),  br(), br(), br(), br(), br(), # br(), 
                                   h2("unten"), 
                                   h4("In der zweiten Reihe wird LINKS die Fläche  pro Katastralgmeinde angegeben, um den relativen Preis visuell besser nachvollziehen zu könen. Auf der RECHTEN Seite befindet sich  die Zählstatistik der einzelnen Kategorien pro Bezirk, um die Auswahl links zu erleichtern.")
                                   ))
                        
                      ))
                    )             
