# R script 
# File: server.R
# Author: Christian Brandstätter 
# Contact: bran.chri@gmail.com
# Date:  7.06.2021
# Copyright (C) 2021
# Description: 

# Server-Script: Hier werden die Daten aufbereitet und die Plots erzeugt 

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
      addProviderTiles("BasemapAT.grau") %>%
      setView(lat=48.22, lng=16.38, zoom=11) %>%

  addPolygons(data=shapeData, weight=2, # fill="transparent",
              layerId = ~NAMEK_RZ, 
              color=shapeData@data$farbs,
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
              ) 

  })

  proxy <- leafletProxy("mymap")
  
  output$Bezirk <- renderUI({
    checkboxGroupInput("Bez", "Bezirk",
                       choices = subset(shapeData,shapeData@data$NAMEK_RZ==as.character(input$NAMEK_RZ)))
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
      selected_polygon <- subset(shapeData, as.character(shapeData@data$NAMEK_RZ) %in% as.character(input$Bez))
      # polygon_labelPt <- selected_polygon@polygons[1]@labpt

      #remove any previously highlighted polygon
      proxy %>% clearGroup(group="highlighted_polygon")
      proxy %>% clearGroup(group="Bezs")
      proxy %>% addPolylines(stroke=TRUE, weight = 1,color="red",data=selected_polygon,
                             group="highlighted_polygon")
    }
  })

  # Datenauswahl für Plot 
  data_to_plot <- reactive({
    data <- dats2
    zahl <- mapvalues(input$Bez, mapper$namen, mapper$nummer, warn_missing=FALSE)
    data4 <- data %>% 
      filter(between(Kaufjahr, input$Zeitraum[1], input$Zeitraum[2]) &
               as.integer(BEZ) == zahl & zuordnung %in%  as.character(input$Kat))
    
    return(data4)
  })

  # Datenauswahl für Kategorien 
  data_to_plot2 <- reactive({
    data <- dats2
    zahl <- mapvalues(input$Bez, mapper$namen, mapper$nummer, warn_missing=FALSE)
    data4 <- data %>% 
      filter(as.integer(BEZ) == zahl)
    return(data4)
  })


  # Plots
  
  output$boxplot <- renderPlot({

    ggplot(data = data_to_plot(), aes(x =Katastralgemeinde, y = Kaufpreis / 1000)) +
      geom_boxplot(color = "darkblue", fill = "lightblue") +
      labs(y = "Kaufpreis  [1.000 \u20AC]",
           x = "Katastralgmeinde") + theme_minimal(base_size=13)+
      scale_y_continuous(labels = function(x) format(x, big.mark = ".",decimal.mark=",",
                                                     scientific = FALSE))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$boxplot2 <- renderPlot({
    
    ggplot(data = data_to_plot(), aes(x =Katastralgemeinde, y = Kaufpreis_rel )) +
      geom_boxplot(color = "darkblue", fill = "lightblue") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ".",decimal.mark=",",
                                                     scientific = FALSE))+
      labs(y = "Kaufpreis / Grundstücksfläche [\u20AC / m2]",
           x = "Katastralgmeinde") + theme_minimal(base_size=13)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$colplot <- renderPlot({

    err <- data_to_plot() %>% group_by(Katastralgemeinde) %>% summarize(MW =mean(`Gst.Fl.`, na.rm = TRUE),
                                                                        SE = sd(`Gst.Fl.`, na.rm = TRUE) /sqrt(n()))
    
    ggplot(data = err, aes(x =Katastralgemeinde, y = MW )) +
      geom_col(color = "darkblue", fill = "lightblue") +
      geom_errorbar(aes(ymin=MW-SE, ymax=MW+SE), width=.4)+
      scale_y_continuous(labels = function(x) format(x, big.mark = ".",decimal.mark=",",
                                                     scientific = FALSE))+
      labs(title="Kategorie (alle Jahre)", y = "MW Grundstücksfläche [m2]",
           x = "Katastralgmeinde", caption= "+/- Standardfehler") + theme_minimal(base_size=13)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })

  output$barplot <- renderPlot({

    ggplot(data = data_to_plot2(), aes(x =zuordnung )) +
      geom_bar(color = "darkblue", fill = "lightblue") +
      geom_label(stat='count', aes(label=..count..))+
      scale_y_continuous(labels = function(x) format(x, big.mark = ".",decimal.mark=",",
                                                     scientific = FALSE))+
      labs(title="Kategorie", y = "Anzahl", x = "Kategorie (Zuordnung)",
           caption = "Filterung nur nach Bezirk möglich, da als Überblick gedacht (nicht nach Kategorie und Jahr). ") + theme_minimal(base_size=13)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
} 
