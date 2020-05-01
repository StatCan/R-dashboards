

# load libraries
require(shinythemes)
require(shiny)
require(leaflet)
require(rgdal)
require(tidyverse)
require(spdplyr)
require(magrittr)
require(rgeos)
require(RColorBrewer)
require(httr)
require(stringi)
require(reshape2)
require(shinydashboard)
require(geosphere)
require(viridis)
require(plotly)
require(tools)
require(formattable)
require(DT)
require(scales)
require(shiny.i18n)
require(shinyWidgets)
require(LaCroixColoR)

# set working directory
setwd("~/Documents/Projects/KnownSideEffects/")
# setwd("C:/Users/jimmy/OneDrive/Documents/GitHub/KnownSideEffects")
# setwd("C:/Users/lab/Documents/GitHub/KnownSideEffects")
# setwd("C:/Users/cuddjim/Documents/KnownSideEffects")

# create provincial data
source("create_data.R")

# create input vectors
years = 2005:2018
areas = prov_map@data$NAME
indicators = c("input","output","price",'emission')
commodities = c("wood","heavy_fuel_oil","diesel","total_coal","natural_gas","uranium")
commodity_labels = toTitleCase(gsub('_', ' ', commodities))

ui <- shinyUI(
  
  fluidPage(
    
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                   padding-top:14px !important;
                   padding-bottom:4px !important;
                   height: 50px;
                 }
                 .navbar {min-height:50px !important;}')),
    tags$head(tags$style(HTML('.navbar .navbar-menu{ background-color: #00b8bd; color: #00b8bd}'))),
    tags$script("$(\"input:radio[name='selected_language'][value='fr']\").parent().css('background-color', '#FFFFFF');"),
    theme = shinytheme("flatly"),
    chooseSliderSkin("HTML5",color='#000039'),
    setBackgroundColor('white'),
    titlePanel(h1('A Story of Thermal Emissions in Canada', 
                  style = "font-family: 'Palatino', bold; font-weight: bold; line-height: 1.1; color: #000000;")),
    uiOutput('page_content')
    
  )
)

translator <- Translator$new(translation_json_path = "translation.json")

server <- shinyServer(function(input, output) {
  
  i18n <- reactive({
    
    selected <- input$selected_language
    
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    
    translator
    
  })
  
  bubble_reactive <- reactive({
    
    min_year = min(input$year)
    max_year = max(input$year)
    selected_province = input$province
    
    subject_matter_2 %>%
      filter(province == selected_province & year %in% min_year:max_year) %>%
      mutate(year_opacity = ((year-1999)^3)/6859,
             efficiency=output/input)
    
  })
  
  output$bubble_1 <- renderPlotly({
    
    bubble_reactive() %>%
      plot_ly(type = 'scatter', mode = 'markers', x = ~input, y = ~output, color = ~commodity,
              colors = c("#FF3200","#E9A17C","#E9E4A6","#1BB6AF","#0076BB","#172869"), name = ~toTitleCase(gsub('_', ' ', commodity)),
              marker = list(size = ~price, line = list(width = 1, color = '#FFFFFF')), hoverinfo = 'text', 
              text=~paste('<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
                          '<b>Cost: </b>$',format(round(price,2),big.mark=",",scientific=FALSE),'<br>',
                          '<b>Input: </b>',format(round(input,1),big.mark=",",scientific=FALSE),'TJ','<br>',
                          '<b>Electricity generated: </b>',format(round(output,1),big.mark=",",scientific=FALSE),'TJ')) %>%
      layout(title = paste0('<b>Comparing ',input$province,' Energy Types</b>'),
             xaxis = list(title = i18n()$t('<b>Inputs (TJ x 1,000)</b>'),zeroline=FALSE),
             yaxis = list(title = i18n()$t('<b>Electricity generated(TJ)</b>'),zeroline=FALSE),margin = list(t=75,b=20), 
             legend = list(orientation = 'h',y=-0.4, font = list(family='Helvetica Neue', size = 10))) %>% 
      config(displayModeBar = F)
    
  })
  
  output$bubble_2 <- renderPlotly({
    
    title_eff = i18n()$t("Efficiency")
    
    bubble_reactive() %>%
      plot_ly(type = 'scatter', mode = 'markers', x = ~efficiency, y = ~emission, color = ~commodity,
              colors = c("#FF3200","#E9A17C","#E9E4A6","#1BB6AF","#0076BB","#172869"),
              marker = list(size = ~price, line = list(width = 1, color = '#FFFFFF')), 
              hoverinfo = 'text',
              text=~paste('<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
                          '<b>Cost: </b>$',format(round(price,2),big.mark=",",scientific=FALSE),'<br>',
                          '<b>',i18n()$t('Efficiency'),': </b>',format(round(efficiency,1),big.mark=",",scientific=FALSE),'TJ','<br>',
                          '<b>Emissions: </b>',format(round(emission,1),big.mark=",",scientific=FALSE),'tonnes')) %>%
      layout(xaxis = list(title = paste0(title_eff),zeroline=FALSE),
             yaxis = list(title = i18n()$t('Emissions (tonnes)'),zeroline=FALSE),
             showlegend=FALSE) %>% 
      config(displayModeBar = F)
    
  })
  
  map_reactive <- reactive({
    
    min_year = min(input$year)
    max_year = max(input$year)
    map_commodity = input$map_commodity
    min_emissions = str_c(map_commodity,'_emission_',min_year)
    max_emissions = str_c(map_commodity,'_emission_',max_year)
    min_outputs = str_c(map_commodity,'_output_',min_year)
    max_outputs = str_c(map_commodity,'_output_',max_year)
    
    prov_map@data %<>% 
      mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
             outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0)) %>% 
      mutate(scaled_outputs = log(1+outputs)^2.5)
    
    prov_map
    
  })
  
  output$plot <- renderLeaflet({
    
    prov_popup <- paste0('<strong>',map_reactive()$NAME,', ',
                         i18n()$t(toTitleCase(gsub('_', ' ', input$map_commodity))),"</strong> <br>",
                         '<strong>',i18n()$t("Emissions"),': </strong>',formatC(round(map_reactive()$emissions,0), format = 'd', big.mark = ","), " tonnes",
                         '<br><strong>',i18n()$t("Outputs"),': </strong>',formatC(round(map_reactive()$outputs,0), format = 'd', big.mark = ","), " MWh")
    
    huey = map_reactive()$emissions
    
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 4,
                                     attributionControl=FALSE)) %>%
      setView(lng = -98.4, lat = 58.2, zoom = 4) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       options = providerTileOptions(opacity = 0.8)) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = map_reactive(),
                  fillColor = ~colorBin(c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), emissions, 5)(emissions),
                  color = "#BDBDC3",
                  fillOpacity = 0.7,
                  weight = 4) %>%
      addCircles(data=map_reactive(), lng = ~x, lat = ~y,
                 fillOpacity = 1,
                 color = 'black',
                 popup = prov_popup,
                 weight = ~scaled_outputs) %>% 
      addLegend(opacity = 0.7, title = i18n()$t("CO2e Emissions (tonnes)"),"bottomleft", 
                pal = colorBin(palette = c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), domain = map_reactive()$emissions, 5), values = huey, 
                labFormat = labelFormat(transform = function(huey) sort(huey, decreasing = FALSE)))
    
  })
  
  

  
  output$page_content <- renderUI({
    
    navbarPage('Statistics Canada',
               
               tabPanel(
                 
                 'Language',
                 
                 radioGroupButtons(
                   inputId = "selected_language",
                   choiceValues = translator$languages,
                   choiceNames = c('English','Francais'),
                   selected = input$selected_language,
                   justified = TRUE,width="200px"
                 ),
                 icon = NULL
                 
               ),
               
               tabPanel(
                 
                 i18n()$t('Comparing Provincial Thermal Emissions'),
                 
                 sidebarPanel(
                   selectizeInput("map_commodity", label = i18n()$t('Select Fuel Type'),
                                  choices = setNames(commodities,i18n()$t(commodity_labels)),
                                  selected = 'diesel'), 
                   width=2
                          ),
                 
                 mainPanel(
                   leafletOutput("plot",height=600), 
                   width=10
                   )
                 
                 ),
        
        tabPanel(
          
          i18n()$t('Comparing Thermal Energy Types'),
          
          sidebarPanel(
                   selectizeInput("province", label = i18n()$t('Select Province'),
                                  choices = areas, selected='Ontario'),
                   sliderInput("year", label = i18n()$t('Select Year'), 
                               2005, 2018, value=c(2005,2018),
                               sep = ""),
                   width=3
                   ),
          
          mainPanel(
            fluidRow(
              plotlyOutput("bubble_1"),
              plotlyOutput("bubble_2")
              ),
            width=6
            )
          
          )
        
        )
      
  })
  
})

shinyApp(ui = ui, server = server)