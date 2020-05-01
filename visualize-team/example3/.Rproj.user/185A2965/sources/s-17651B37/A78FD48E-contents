
# set working directory
#setwd("~/Documents/Projects/KnownSideEffects/")
# setwd("C:/Users/jimmy/OneDrive/Documents/GitHub/KnownSideEffects")
# setwd("C:/Users/lab/Documents/GitHub/KnownSideEffects")
# setwd("C:/Users/cuddjim/Documents/KnownSideEffects")

# create provincial data
source("create_data.R")

ui <- shinyUI(
  
  fluidPage(
    
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                   padding-top:14px !important;
                   padding-bottom:4px !important;
                   height: 50px;
    }
    .navbar-default .navbar-nav>li>a:focus, .navbar-default 
    .navbar-nav>li>a:hover { 
    color: #EEEEEE !important; background-color: #000000 !important;
    }
                 .navbar {min-height:50px !important;}')),
    tags$head(tags$style(HTML('.navbar .navbar-menu{ background-color: #00b8bd; color: #00b8bd}'))),
    tags$script("$(\"input:radio[name='selected_language'][value='fr']\").parent().css('background-color', '#FFFFFF');"),
    theme = shinytheme("flatly"),
    chooseSliderSkin("HTML5",color='#000039'),
    setBackgroundColor('white'),
    
    titlePanel(h1('Electricity generated from fossil fuels', 
                  style = "font-family: 'Palatino', bold; font-weight: bold; line-height: 1.1; color: #000000;")),
    uiOutput('page_content_1'),
    uiOutput('page_content')
    
  )
)

translator <- Translator$new(translation_json_path = "Translation.json")

server <- shinyServer(function(input, output) {
  
  # select and translate language
  tr <- reactive({
    
    selected <- input$selected_language
    
    if (length(selected) > 0 && selected == TRUE) {
      translator$set_translation_language('en')
    }
    
    else {
      
      translator$set_translation_language('fr')
      
    }
    
    translator
    
  })
  
  # create bubble plot
  bubble_reactive <- reactive({
    
    selected_province = input$province
    min_year = min(input$bubble_year); max_year = max(input$bubble_year)
    x_indicator = input$x_indicator;  y_indicator = input$y_indicator; b_indicator = input$b_indicator
    
    subject_matter_2 %>%
      filter(province == selected_province & year %in% min_year:max_year & commodity != 'uranium') %>% 
      mutate(year_opacity = ((year-1999)^3)/6859,
             efficiency=output/input) %>%
      mutate(selected_x_axis = as.numeric(paste0(!!sym(x_indicator))),
             selected_y_axis = as.numeric(paste0(!!sym(y_indicator))),
             selected_b_axis = as.numeric(paste0(!!sym(b_indicator)))) %>%
      mutate(mean_b_axis = median(selected_b_axis,na.rm=TRUE)) %>%
      mutate(selected_b_axis=1000*selected_b_axis/mean_b_axis)
    
    
    
  })
  
  output$bubble <- renderPlotly({
    
    bubble_reactive() %>%
      plot_ly(
        type = 'scatter', mode = 'markers', size = ~selected_b_axis, 
        marker = list(sizemode='diameter',opacity = 0.5, line = list(width = 1, color = '#FFFFFF')), 
        x = ~selected_x_axis, y = ~selected_y_axis, 
        color = ~commodity, colors = c("#FF3200","#E9A17C","#E9E4A6","#1BB6AF","#0076BB","#172869"),
        hoverinfo = 'text',
        text=~paste(
          '<b>',toTitleCase(gsub('_', ' ', commodity)),year,'</b>','<br>',
          '<b>Cost: </b>$',format(round(price,2),big.mark=",",scientific=FALSE),'<br>',
          '<b>Efficiency: </b>', format(round(100*output/input,1), big.mark=",", scientific=FALSE),' %', '<br>',
          '<b>Emissions: </b>',format(round(emission,1),big.mark=",",scientific=FALSE),'tonnes'
        )) %>%
      layout(
        #title = paste0('<b>Comparing ',input$province,' Energy Types</b>'),
        xaxis = list(
          font = list(family='Helvetica Neue', weight='bold',size = 20),
          title = paste0('<b>',tr()$t(paste0(indicator_labels[which(indicators==input$x_indicator)])),'</b>'), 
          zeroline=FALSE),
        yaxis = list(
          title = paste0('<b>',tr()$t(paste0(indicator_labels[which(indicators==input$y_indicator)])),'</b>'), 
          zeroline=FALSE),
        legend = list(orientation = 'h', y = -0.3, font = list(family='Helvetica Neue', weight='bold',size = 18))
      ) %>% 
      plotly::config(displayModeBar = F)
    
  })
  
  # create line plot
  line_reactive <- reactive({
    
    selected_province = input$line_province
    selected_commodity_1 = input$y_energy_1; selected_commodity_2 = input$y_energy_2
    selected_commodities = c(selected_commodity_1,selected_commodity_2)
    selected_indicator = input$line_indicator
    min_year = min(input$year_line); max_year = max(input$year_line)
    
    subject_matter_2 %>% 
      filter(province == selected_province,
             commodity %in% selected_commodities,
             year %in% min_year:max_year) %>%
      mutate(selected_indicator = as.numeric(paste0(!!sym(selected_indicator)))) %>%
      spread(commodity,selected_indicator) %>%
      group_by(year) %>%
      mutate(selected_commodity_1 = sum(as.numeric(paste0(!!sym(selected_commodity_1))),na.rm=TRUE),
             selected_commodity_2 = sum(as.numeric(paste0(!!sym(selected_commodity_2))),na.rm=TRUE))
    
  })
  
  output$line <- renderPlotly({
    
    plot_ly(data=line_reactive()) %>%
      add_trace(
        type = 'scatter', mode = 'lines', name= toTitleCase(gsub('_', ' ', input$y_energy_1)), 
        x= ~year, y= ~selected_commodity_1,
        fill = 'tozeroy', fillcolor = 'rgba(255, 12, 2, 0.6)', line = list(color = 'rgba(255, 12, 2, 1)', width = 2),
        hoverinfo = "text", text = ~paste(format(round(selected_commodity_1, 1), big.mark = ",", scientific = FALSE), ' TJ'),
        legendgroup = ~selected_commodity_1
      ) %>%
      add_trace(
        type = 'scatter', mode = 'none', name= toTitleCase(gsub('_', ' ', input$y_energy_2)),
        x= ~year, y= ~selected_commodity_2,
        fill = 'tozeroy', fillcolor = 'rgba(168, 216, 234, 0.7)', line = list(color = 'rgba(168, 216, 234, 1)', width = 2),
        hoverinfo = "text", text = ~paste(format(round(input,1),big.mark = ",",scientific = FALSE),' TJ'),
        legendgroup = ~selected_commodity_1
      ) %>%
      layout(
        # title = paste0('<b>Comparing ',tr()$t(paste0(indicator_labels[which(indicators==input$line_indicator)])),
        #                ' of ',toTitleCase(gsub('_', ' ', input$y_energy_1)),' to ',
        #                toTitleCase(gsub('_', ' ', input$y_energy_2)),'</b>'),
        xaxis = list(title = "",showline=FALSE, range = c(min(input$year_line),max(input$year_line))),
        yaxis = list(range=c(0,~max(c(line_reactive()$selected_commodity_1,line_reactive()$selected_commodity_2))),
                     side = 'left', title = tr()$t('Inputs (TJ)'), showgrid = FALSE, showline = FALSE)
      ) %>%
      plotly::config(displayModeBar = F)
    
  })
  
  output$storytable <- renderDataTable({
    as.datatable(tr()$t(story) %>% 
                   formattable(),options=list(dom='t'))
  })
  
  output$about <- renderText({
    paste(tr()$t("This product would allow Canadians to compare over time and across provinces electricity generation by fuel types. Users are able to analyse the amount of energy used to produce electricity (inputs), the amount of electricity generated (outputs), the efficiency ratios, the fuel cost and emissions. This data is based on the Annual Survey of Electric Thermal Generating Station Fuel Consumption and can be found in the tables: Electricity from fuels, annual generation by electric utility thermal plants, Electric power generation, annual cost of fuel consumed by electric utility thermal plants and Electric power generation, annual fuel consumed by electric utility thermal plants. This survey estimates fuel consumption to generate electricity by utilities. This data was coupled with emission factors published by Environment and Climate Change Canada (hyperlink). The emissions by fuel type were calculated using these emission factors."))
  })
  
  
  # create map
  map_reactive <- reactive({
    
    min_year = min(input$year); max_year = max(input$year)
    map_commodity = input$map_commodity
    min_emissions = str_c(map_commodity,'_emission_',min_year); max_emissions = str_c(map_commodity,'_emission_',max_year)
    min_outputs = str_c(map_commodity,'_output_',min_year); max_outputs = str_c(map_commodity,'_output_',max_year)
    
    prov_map@data %<>% 
      mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
             outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0)) %>% 
      mutate(scaled_outputs = log(1+outputs)^2.5) %>%
      mutate(mean_output = mean(scaled_outputs,na.rm=TRUE)) %>%
      mutate(outputs_1=23*scaled_outputs/mean_output)
    
    prov_map
    
  })
  
  output$plot <- renderLeaflet({
    
    prov_popup <- paste0('<strong>',map_reactive()$NAME,', ',
                         tr()$t(toTitleCase(gsub('_', ' ', input$map_commodity))),"</strong> <br>",
                         '<strong>',tr()$t("Emissions"),': </strong>',formatC(round(map_reactive()$emissions,0), format = 'd', big.mark = ","), " tonnes",
                         '<br><strong>',tr()$t("Electricity generated"),': </strong>',formatC(round(map_reactive()$outputs,0), format = 'd', big.mark = ","), " TJ")
    
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
                 weight = ~outputs_1) %>% 
      addLegend(opacity = 0.7, title = tr()$t("CO2e Emissions (tonnes)"),"bottomleft", 
                pal = colorBin(palette = c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), domain = map_reactive()$emissions, 5), values = huey, 
                labFormat = labelFormat(transform = function(huey) sort(huey, decreasing = FALSE)))
    
  })
  
  # bilingual button
  output$page_content_1 <- renderUI({
    
    fluidPage(
      
      tabPanel(
        
        'Language',
        
        column(switchInput(
          inputId = "selected_language",
          value = TRUE, #translator$languages,
          onLabel = 'Francais',
          offLabel = 'English',
          onStatus = '#000000',
          offStatus = '#FFFFFF',
          size='mini'
        ), width=2,offset=10),
        icon = NULL
        
      )
    )
    
  })
  
  # create ui
  output$page_content <- renderUI({
    
    navbarPage('',
               
               tabPanel(
                 
                 tr()$t('Map'),
                 
                 sidebarPanel(
                   selectizeInput("map_commodity", label = tr()$t('Select Fuel Type'),
                                  choices = setNames(commodities,tr()$t(commodity_labels)),
                                  selected = 'diesel'),
                   sliderInput("year", label = tr()$t('Select Year:'),2005, 2018, value=2018,sep = ""), 
                   width=2
                 ),
                 
                 mainPanel(
                   leafletOutput("plot",height=500), 
                   width=10
                 )
                 
               ),
               
               tabPanel(
                 
                 tr()$t('Bubble chart'),
                 
                 sidebarPanel(
                   selectizeInput("province", label = tr()$t('Select Province'),
                                  choices = setNames(areas,tr()$t(areas)), selected='Ontario'),
                   selectizeInput("x_indicator", label = tr()$t('Select variable (x-axis)'),
                                  choices = setNames(indicators,tr()$t(indicator_labels)), selected='input'),
                   selectizeInput("y_indicator", label = tr()$t('Select variable (y-axis)'),
                                  choices = setNames(indicators,tr()$t(indicator_labels)), selected='output'),
                   selectizeInput("b_indicator", label = tr()$t('Select variable (bubble size)'),
                                  choices = setNames(indicators,tr()$t(indicator_labels)), selected='price'),
                   sliderInput("bubble_year", label = tr()$t('Select period'), 
                               2005, 2018, value=c(2005,2018),
                               sep = ""),
                   width=3
                 ),
                 
                 mainPanel(
                   fluidRow(
                     plotlyOutput("bubble")
                   ),
                   width=8
                 )
                 
               ),
               
               tabPanel(
                 
                 tr()$t('Line chart'),
                 
                 sidebarPanel(
                   selectizeInput("line_province", label = tr()$t('Select Province'),
                                  choices = setNames(areas,tr()$t(areas)), selected='Ontario'),
                   selectizeInput("line_indicator", label = tr()$t('Select variable'),
                                  choices = setNames(indicators,tr()$t(indicator_labels)), selected='input'),
                   selectizeInput("y_energy_1", label = tr()$t('Compare'),
                                  choices = setNames(commodities,tr()$t(commodity_labels)), selected='total_coal'),
                   selectizeInput("y_energy_2", label = tr()$t('To'),
                                  choices = setNames(commodities,tr()$t(commodity_labels)), selected='natural_gas'),
                   sliderInput("year_line", label = tr()$t('Select Year'), 
                               2005, 2018, value=c(2005,2018),
                               sep = ""),
                   width=3
                 ),
                 
                 mainPanel(
                   fluidRow(
                     plotlyOutput("line")
                   ),
                   width=8
                 )
                 
               ),
               tabPanel(
                 
                 tr()$t('Data stories'),
                 
                 # sidebarPanel(
                 #   selectizeInput("province_story", label = tr()$t('Select Province'),
                 #                  choices = tr()$t(areas), selected='Ontario'), 
                 #   width=2
                 # ),
                 
                 mainPanel(
                   dataTableOutput("storytable"), 
                   width=12
                 )
                 
               ),
               tabPanel(
                 
                 tr()$t('About'),
                 mainPanel(
                   textOutput('about')
                 )

                 
               )
               
    )
    
  })
  
})

shinyApp(ui = ui, server = server)

