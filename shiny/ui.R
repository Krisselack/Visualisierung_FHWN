# R script 
# File: ui.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021
# Description: 

ui <- dashboardPage( skin = "blue", #shinytheme("simplex"),

  dashboardHeader(title = "Datensatz Stadt Wien - Preisentwicklung "
                  ),
  
  title="Visualisierungstechniken",
  
  
  dashboardSidebar(width = 500, 
    wellPanel( radioButtons("Bez", 
                            label = "Bezirk",
                            choices =  levels(shapeData@data$NAMEK_RZ)),
                         #   selected = "XIII. Hietzing"),
              
                            style = "color: darkblue;" ,

              selectInput("Kat", "Kategorie:",
                          multiple = TRUE, 
                          choices = levels(dats2$zuordnung), 
                          selected= "Ein-, Zweifamilienhaus"),

     sliderInput("Zeitraum",
                          "Kaufjahr von bis",
                          min = min(dats$Kaufjahr, na.rm=TRUE),
                          max = max(dats$Kaufjahr, na.rm=TRUE), 
                          value=c(min(dats$Kaufjahr, na.rm=TRUE), max(dats$Kaufjahr, na.rm=TRUE)) 
#                          timeFormat="%Y-%m-%d")
            )),
               uiOutput("Bez")       
    ), 
  
dashboardBody(

  fluidRow(
    box(width=5, leafletOutput("mymap", height = 500, ), height= 520), 

                ## fluidRow(h2("Darstellung pro Katastrallgemeinde"),
                ##          h3("Kaufpreis pro Katastralgemeinde "),
                ##          column(12,
                ##                 plotOutput("boxplot")
                ##                 )
                tabBox(width=5, height = 520, #selected= "Kaufpreis_absolut",
                  title = "Darstellung pro Katastralgemeinde",
                  tabPanel("Kaufpreis_absolut", plotOutput("boxplot")),
                  tabPanel("Kaufpreis_relativ", plotOutput("boxplot2"))
                  ),

    box(width = 5, title = "Grundstücksfläche", plotOutput("colplot")),
    box(width = 5, title = "Anzahl pro Kategorie und Bezirk", plotOutput("barplot"))
    
  )
))             

#         h3("Darstellung der ausgewählten Daten"),
#       DT::dataTableOutput("fldaten")

