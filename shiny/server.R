# R script 
# File: server.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021
# Description: 

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("BasemapAT.grau") %>%
      setView(lat=48.21, lng=16.45 , zoom=10) %>%

  addPolygons(data=shapeData, weight=2, # fill="transparent",
              layerId = ~NAMEK_RZ, 
              color=colmap,
                fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 3,
                fillOpacity = 0.8,
                color = "blue",
#                opacity = 0.2,
                bringToFront = TRUE,
                sendToBack = TRUE),
              label = lapply(labels, htmltools::HTML), 
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                 direction = "auto"
                )
              ) #%>%

  ## addLegend(data = shapeData@data$NAMEK_RZ, 
  ##           colors = palmap$color,
  ##           labels= palmap$Bez,
  ##           opacity = 1.0, 
  ##           title = "Bezirke",
  ##           position = "topright")

  })

  proxy <- leafletProxy("mymap")
  
   output$Bezirk <- renderUI({
     checkboxGroupInput("Bez", "Bezirk",
                        choices = subset(shapeData,shapeData@data$NAMEK_RZ==input$NAMEK_RZ))
   })

 observe({ 

   event <- input$mymap_shape_click
  
   updateRadioButtons(session,
                          inputId = "Bez",
                          label = "Bezirk",
                          choices = levels(shapeData@data$NAMEK_RZ),
                          selected = event$id)
    })
  
    observe({
    if(!is.null(input$Bez)){
      #get the selected polygon and extract the label point 
      selected_polygon <- subset(shapeData, as.character(shapeData@data$NAMEK_RZ) %in% input$Bez)
      # polygon_labelPt <- selected_polygon@polygons[1]@labpt

      #remove any previously highlighted polygon
      proxy %>% clearGroup(group="highlighted_polygon")
      proxy %>% clearGroup(group="Bezs")
      proxy %>% addPolylines(stroke=TRUE, weight = 1,color="red",data=selected_polygon,
                             group="highlighted_polygon")
    }
  })

  ## observe({
  ##   if(!is.null(input$Bez)){
  ##     selected_polygon_bez <- subset(shapeData,as.character(shapeData@data$NAMEK_RZ) %in% input$Bez)
  ##     proxy %>% addPolylines(data=selected_polygon_bez,layerId=input$Bez,group="Bezs",
  ##                            highlight = highlightOptions(
  ##                              weight = 3,
  ##                              fillOpacity = 0,
  ##                              color = "blue",
  ##                              opacity = 1.0,
  ##                              bringToFront = TRUE
  ##                            ))
      
  ##    } else {
  ##      return()
   ##   }



    output$boxplot <- renderPlot({

      zahl <- mapvalues(input$Bez, mapper$namen, mapper$nummer, warn_missing=FALSE)      
      ausw <-  dats2   %>% filter(as.integer(BEZ) == zahl)
      ggplot(data = ausw, aes(x =Katastralgemeinde , y=Kaufpreis)) +
      geom_boxplot() 
      
    })

  
} 
