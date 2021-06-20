# R script 
# File: ui.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021
# Description: 

ui <- dashboardPage(skin = "blue", #shinytheme("simplex"),
                    
                    dashboardHeader(title = "Datensatz Stadt Wien - Preisentwicklung "
                                    ),
                    
                    title="Visualisierungstechniken",
                    
                    
                    dashboardSidebar(width = 500,
                                     h3("In diesem Bereich  kann man Daten filtern."),
                                     # h3(": "), 
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
                                     #   uiOutput("Bez")      

                                     ), 
                    
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ), 
                      fluidRow(
                        h3("In der ersten Abbildungszeile wird eine  Wien-Karte nach Bezirk dargestellt. Die Click-Auswahl ist mit der linken Button-Auswahl verknüpft. Im Rechten Fenster werden Box-Plots dargestellt, die den absoluten und relativen Kaufwert pro Katastralgemeinde auf Bezirksebene (in Tabs) darstellen. "), 

                        box(width=6, leafletOutput("mymap", height = 500, ), height= 520), 

                        ## fluidRow(h2("Darstellung pro Katastrallgemeinde"),
                        ##          h3("Kaufpreis pro Katastralgemeinde "),
                        ##          column(12,
                        ##                 plotOutput("boxplot")
                        ##                 )
                        tabBox(width=6, height = 520, #selected= "Kaufpreis_absolut",
                               title = "Darstellung pro Katastralgemeinde",
                               tabPanel("Kaufpreis_absolut", plotOutput("boxplot")),
                               tabPanel("Kaufpreis_relativ", plotOutput("boxplot2"))
                               ),

                        h3("In der zweiten Reihe wird auch noch die Fläche  pro Katastralgmeinde angegeben, um den relativen Preis visuell besser nachvollziehen zu können. Auf der rechten Seite befindet sich die Zählstatistik der einzelnen Kategorien pro Bezirk, um die Auswahl links zu erleichtern."), 
                        box(width = 6, title = "Grundstücksfläche - Darstellung pro Katastralgemeinde", plotOutput("colplot")),
                        box(width = 6, title = "Anzahl pro Kategorie (per Bezirk)", plotOutput("barplot"))
                        
                      )
                    ))             

#         h3("Darstellung der ausgewählten Daten"),
#       DT::dataTableOutput("fldaten")

