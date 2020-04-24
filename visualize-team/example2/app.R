library(shiny)
library(leaflet)
library(RColorBrewer)
library(sqldf)
library(tidyverse)
library(shinythemes)
library(DT)
library(ggplot2)







df <- read.csv("mycleandata.csv")








ui <- bootstrapPage(
       fluidRow(
    headerPanel("Disclaimer: This is an example. It may be out of date. / Avertissement: ceci est un exemple. Il peut être obsolète"),
    width = "100%",
    align = "center",
    style = "background-color:#ebae34; color:#ffffff; margin-bottom: 30px;"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Zero Emission Vehicles", min(df$zev), max(df$zev),
                            value= range(df$zev),
                            step = 1.0
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxGroupInput("year", "Year",
                            choices = list("2014"=1,"2015"=2,"2016"=3,"2017"=4,"2018"=5), selected = (1:5)
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

#########################################################################
#########################################################################
#########################################################################

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    df[df$zev >= input$range[1] & df$zev <= input$range[2],]
  },)
  
  
  #########################################################################
  #########################################################################
  #########################################################################
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, df$zev)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(df) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~zev*20, weight = 1, color = "#777777",
                 fillColor = ~pal(zev), fillOpacity = 0.7, popup = ~paste("Major CMA(s):",mcma)
      )
  })
  
  
  
  #########################################################################
  #########################################################################
  #########################################################################
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = df)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",title="# of ZEV's",
                          pal = pal, values = ~zev
      )
    }
  })
}

shinyApp(ui, server)






