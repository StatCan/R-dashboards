library(formatR)
library(leaflet)
library(plotly)
library(plyr)
library(RColorBrewer)
library(rgdal)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)

source('load_data.R')


# TODO: Add report by province
# TODO: Add french translation for rest of app


# Set color ramp 
my_colors <- colorRampPalette(brewer.pal(8, 'Set2'))(15)

# Translations. Inspired by https://github.com/chrislad/multilingualShinyApp
dictionary_content <- read_csv('./data/translation/dictionary.csv')
translation <- dlply(dictionary_content ,.(key), function(s) key = as.list(s))
dictionary_variables <- read_csv('./data/processed/dictionary_variables.csv')


server <- function(input, output, session) {
    
  reactive_vars <- reactiveValues()
  
  # Translation -------------------------------------------------------------
  
  reactive_vars$language <- 'en'
  
  # Translate text given current language
  tr <- function(text){ 
    sapply(text,function(s) translation[[s]][[reactive_vars$language]], USE.NAMES=FALSE)
  }
  
  # Change language
  observeEvent(input$btn_language, {
    if (reactive_vars$language == 'en') {
      reactive_vars$language <- 'fr'
    } else {
      reactive_vars$language <- 'en'
    }
  })
  
  output$label_language <- renderText({
    reactive_vars$language
  })
  
  

  # Datasets. Updated daily ------------------------------------------------
  
  reactive_nmvr_data <- reactive({
    invalidateLater(8.64e7, session)
    nmvr_data <- load_dataset('nmvr')
    return(nmvr_data)
  })
  
  reactive_nmvs_data <- reactive({
    invalidateLater(8.64e7, session)
    nmvs_data <- load_dataset('nmvs')
    return (nmvs_data)
  })
  
  
  # # Update language of variables in dataset
  # reactive_nmvr_data <- reactive({
  #   nmvr_data <- reactive_nmvr_data_load()
  #   
  #   if (is.null(nmvr_data)) {return (nmvr_data)}
  #   
  #   fuel_types_tr <- dictionary_variables %>% filter(variable == 'fuel_type')
  #   geo_tr <- dictionary_variables %>% filter(variable == 'geo')
  #    
  #   if (reactive_vars$language == 'en') {
  #     nmvr_data <- nmvr_data %>%
  #       mutate(
  #         geo = mapvalues(geo, geo_tr$fr, geo_tr$en, warn_missing = FALSE),
  #         fuel_type = mapvalues(fuel_type, fuel_types_tr$fr, fuel_types_tr$en, warn_missing = FALSE)
  #       )
  #   }
  # 
  #   else if (reactive_vars$language == 'fr') {
  #     nmvr_data <- nmvr_data %>%
  #       mutate(
  #         geo = mapvalues(geo, geo_tr$en, geo_tr$fr, warn_missing = FALSE),
  #         fuel_type = mapvalues(fuel_type, fuel_types_tr$en, fuel_types_tr$fr, warn_missing = FALSE)
  #       )
  #   }
  #   
  #   return (nmvr_data)
  # })
  
  
  
  # NMVR Map View ----------------------------------------------------------------
  
  output$nmvr_map_view <- renderText({
    tr('nmvr_map')
  })

  # Extract some properties from datasets
  fuel_types <- reactive({
    reactive_nmvr_data()$fuel_type %>% unique()
  })
  
  ev_fuel_types <- c('Battery electric', 'Plug-in hybrid electric') 
  
  provinces <- reactive({
    reactive_nmvr_data()$geo %>% unique() %>% sort()
  })
  
  min_year <- reactive({
    reactive_nmvr_data()$year %>% min()
  })
  
  max_year <- reactive({
    reactive_nmvr_data()$year %>% max()
  })
  
  # Plotting parameters for map
  bins <- c(0, 100, 10000, 100000, 500000, Inf)
  
  max_value <- reactive({
    reactive_nmvr_data()$amount %>% max()
  })
  
  ev_pal <- reactive({
    colorBin('Blues', domain = c(0, max_value()), bins = bins)
  })
    
  
  reactive_nmvr_data_year <- reactive({
    reactive_nmvr_data() %>% 
      dplyr::filter(year == input$plot_date)
  })
   
  reactive_total_new_vehicles <- reactive({
    reactive_nmvr_data_year() %>% 
      dplyr::filter(fuel_type == 'All fuel types') %>% pull(amount) %>% sum()
  })
  
  reactive_total_new_zev <- reactive({
    reactive_nmvr_data_year() %>% 
      dplyr::filter(fuel_type %in% ev_fuel_types) %>% pull(amount) %>% sum()
  })
  
  reactive_total_new_gv <- reactive({
    reactive_total_new_vehicles() - reactive_total_new_zev()
  })
  
  
  output$reactive_total_new_vehicles <- renderText({
    paste(prettyNum(reactive_total_new_vehicles(), big.mark=','), tr('new_vehicles'))
  })
  
  output$reactive_total_new_gv <- renderText({
    paste(prettyNum(reactive_total_new_gv(), big.mark=','), tr('new_gas_vehicles'))
  })
  
  output$reactive_total_new_zev <- renderText({
    paste(prettyNum(reactive_total_new_zev(), big.mark=','), tr('new_electric_vehicles'))
  })
  
  output$slider_input_plot_date <- renderUI({
    sliderInput(
      'plot_date',
      label = tr('year'),
      value = max_year(),
      min = min_year(),
      max = max_year(),
      step = 1,
      sep = '',
      animate = animationOptions(interval = 2000, loop = FALSE)
    )    
  })
  
  
  # Basemap
  output$mymap <- renderLeaflet({
    leaflet() %>% 
        addTiles() %>% 
        addLayersControl(
          position = 'topright',
          overlayGroups = fuel_types(),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup(fuel_types()[-1]) %>% 
        setView(-95, 55, zoom = 5) %>% 
        addLegend('topright',
                  pal = ev_pal(),
                  values = c(0, max_value()),
                  layerId = 'legend',
                  title = paste0('<small>Amount of vehicles</small>'))
  })
  
  # Readd legend with language changes
  observeEvent(input$btn_language, {
    leafletProxy('mymap') %>% 
      removeControl('legend') %>% 
      addLegend('topright',
                pal = ev_pal(),
                values = c(0, max_value()),
                layerId = 'legend',
                title = paste0('<small>', tr('amount_of_vehicles'), '</small>'))
  })
  

  # Update map circle markers when date changes
  observeEvent(input$plot_date, {
    leafletProxy('mymap') %>% 
      clearMarkers() 
    
    # Add circle markers for each group to the basemap
    for (fuel_typ in fuel_types()) {
      nmvr_data_filtered <- 
        reactive_nmvr_data_year() %>% 
        dplyr::filter(fuel_type == fuel_typ)
      amount <- nmvr_data_filtered$amount
      
      leafletProxy('mymap') %>% 
        addCircleMarkers(
          data = nmvr_data_filtered, 
          lat = ~ latitude, 
          lng = ~ longitude,
          weight = 1, 
          radius = ~8*(amount)^(1/5),
          fillOpacity = 0.6, 
          fillColor = ~ev_pal()(amount),
          group = fuel_typ,
          label = sprintf("<strong>%s</strong><br/>Amount: %g", nmvr_data_filtered$geo, amount) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px", direction = "auto"))
    }
  })
  
  
  
  # NMVR Growth Tab --------------------------------------------------------------
  
  # UI Components
  output$nmvr_time_view <- renderText({
    tr('nmvr_time')
  })
  
  output$select_group_by <- renderUI({
    choices <- c('province' = 'Province', 'fuel_type' = 'Fuel type')
    names(choices) <- c(tr('province'), tr('fuel_type'))
    
    selectInput(
      'nmvr_group_select', tr('group_by'),
      choices = choices)
  })
  
  output$select_input_province <- renderUI({
    disabled(
      selectInput(
      'province_select', tr('province'),
      choices = provinces(),
      selected = provinces()[1]
      )
    )
  })
  
  output$select_input_fuel_type <- renderUI({
    selectInput(
      'fuel_type_select', tr('fuel_type'),
      choices = fuel_types()
    )
  })
  
  
  reactive_nmvr_data_fuel_type <- reactive({
    reactive_nmvr_data() %>% 
      dplyr::filter(fuel_type == input$fuel_type_select)
  })
  
  reactive_nmvr_data_province <- reactive({
    reactive_nmvr_data() %>% 
      dplyr::filter(geo == input$province_select)
  })
  
  
  # Disable/Enable select options based on Group select and set color ramp
  observeEvent(input$nmvr_group_select, {
    if (input$nmvr_group_select == 'Province') {
      shinyjs::disable('province_select')
      shinyjs::enable('fuel_type_select')
    } 
    else if (input$nmvr_group_select == 'Fuel type') {
      shinyjs::enable('province_select')
      shinyjs::disable('fuel_type_select')
    }
  })
  
  # Output plots
  # TODO: Switch to plotly proxy to update plots 
  output$time_series_plot <- renderPlotly({
    if (input$nmvr_group_select == 'Province') {
      nmvr_data_plot <- reactive_nmvr_data_fuel_type()
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~cumsum, color = ~geo, colors = my_colors, type = 'scatter', mode = 'lines+markers') %>% 
        plotly::layout(
          xaxis = list(title = tr('year')),
          yaxis = list(title = tr('number_of_vehicles'), range=c(0, 1.2 * max(nmvr_data_plot$cumsum))),
          title = paste0(tr('total_number_of_vehicles_by_province'), ' - ', input$fuel_type_select)
        )
    } 
    
    else if (input$nmvr_group_select == 'Fuel type') {
      nmvr_data_plot <- reactive_nmvr_data_province()
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~cumsum, color = ~fuel_type, type = 'scatter', mode = 'lines+markers') %>% 
        plotly::layout(
          xaxis = list(title = tr('year')),
          yaxis = list(title = tr('number_of_vehicles'), range=c(0, 1.2 * max(nmvr_data_plot$cumsum))),
          title = paste0(tr('total_number_of_vehicles_per_fuel_type'), ' - ', input$province_select)
        )
    }
  })
  
  output$bar_chart_plot <- renderPlotly({
    if (input$nmvr_group_select == 'Province') {
      nmvr_data_plot <- reactive_nmvr_data_fuel_type()
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~amount, color = ~geo, colors = my_colors, type = 'bar') %>% 
        plotly::layout(
          xaxis = list(title = tr('year')),
          yaxis = list(title = tr('number_of_new_vehicles')),
          title = paste0(tr('total_number_of_new_vehicles_by_provinces'), ' - ', input$fuel_type_select),
          barmode = 'stack')
    }
    
    else if (input$nmvr_group_select == 'Fuel type') {
      nmvr_data_plot <- reactive_nmvr_data_province() %>% 
        dplyr::filter(fuel_type != 'All fuel types')
      nmvr_data_plot %>% 
        plot_ly(x = ~year, y = ~amount, color = ~fuel_type, type = 'bar') %>% 
        plotly::layout(
          xaxis = list(title = tr('year')),
          yaxis = list(title = tr('number_of_new_vehicles')),
          title = paste0(tr('total_number_of_new_vehicles_per_fuel_type'), ' - ', input$province_select),
          barmode = 'stack')
    }
  })
  
  
  # output$sunburst_plot <- renderPlotly({
  #   nmvr_data_plot <- nmvr_data %>%
  #     filter(year == 2018) %>% 
  #     filter(fuel_type != 'All fuel types', fuel_type != 'Gasoline') %>% 
  #     arrange(geo)
  #   labels <- c('Canada', provinces, nmvr_data_plot$fuel_type)
  #   parents <- c('', rep('Canada', length(provinces)), nmvr_data_plot$geo)
  #   values <- c(
  #     nmvr_data_plot %>% pull(amount) %>% sum(),
  #     nmvr_data_plot %>% group_by(geo) %>% summarise(total=sum(amount)) %>% pull(total),
  #     nmvr_data_plot %>% pull(amount)
  #   )
  #   plot_ly(labels = labels, parents = parents, values = values, type = 'sunburst')
  # })  
  
  
  
  

  # NMVS Time View ----------------------------------------------------------
  
  sale_types <- c('Dollars', 'Units')
  
  origins_manufacture <- reactive({
    reactive_nmvs_data()$origin_of_manufacture %>%
      unique() %>% 
      sort()
  })
  
  vehicle_types <- reactive({
    reactive_nmvs_data()$vehicle_type %>% unique()
  })
  
  
  # UI components
  output$nmvs_time_view <- renderText({
    tr('nmvs_time')
  })
  
  output$nmvs_select_sale_type <- renderUI({
    selectInput(
      inputId = 'nmvs_select_sale_type',
      label = tr('sales_type'),
      choices = sale_types,
      selected = sale_types[1]
    )
  })
  
  output$nmvs_select_origin_manufacture <- renderUI({
    selectInput(
      inputId = 'nmvs_select_origin_manufacture',
      label = tr('origins_of_manufacture'),
      choices = origins_manufacture(),
      selected = 'Total, country of manufacture'
    )
  })
  
  output$nmvs_select_vehicle_type <- renderUI({
    selectInput(
      inputId = 'nmvs_select_vehicle_type',
      label =  tr('vehicle_type'),
      choices = vehicle_types()
    )
  })
  
  
  # Filtered data based on select inputs
  reactive_nmvs_data_filtered <- reactive({
    reactive_nmvs_data() %>% 
      dplyr::filter(
        sales == input$nmvs_select_sale_type,
        origin_of_manufacture == input$nmvs_select_origin_manufacture,
        vehicle_type == input$nmvs_select_vehicle_type
      )
  })
  
  
  # Output plots
  output$nmvs_time_series_plot <- renderPlotly({
    reactive_nmvs_data_filtered() %>% 
      plot_ly(x = ~year, y = ~value, color = ~geo, colors = my_colors,
              type = 'scatter', mode = 'lines+markers', height=600, width=1550) %>% 
      plotly::layout(
        xaxis = list(title = tr('year')),
        yaxis = list(title = input$nmvs_select_sale_type),
        title = paste0(tr('number_of_new_vehicle_sales'), ' (', input$nmvs_select_sale_type, ')')
      )
  })
  
  
  
  
  # CMA Level view ----------------------------------------------------------
  
  # Load data
  can_cma_shapes <- get_can_cma_shapes()
  fake_cma <- read_csv('./data/raw/fake_cma.csv')

  cma_max_year <- fake_cma$year %>% max()
  cma_min_year <- fake_cma$year %>% min()
  cma_max_value <- fake_cma$value %>% max()
  

  # UI components
  output$text_fake_data_message <- renderText({
    tr('fake_data_message')
  })
  
  output$text_cma_map <- renderText({
    tr('cma_map')
  })
  
  output$slider_cma_date <- renderUI({
    sliderInput(
      'slider_cma_date',
      label = tr('year'),
      value = cma_max_year,
      min = cma_min_year,
      max = cma_max_year,
      step = 1,
      sep = '',
      animate = animationOptions(interval = 2000, loop = FALSE)
    )
  })

  # Reactive values
  reactive_cma_data<- reactive({
    # Small fix since slider value is not set until cma tab is clicked on causing some errors
    year_select <- if(is.null(input$slider_cma_date)) cma_max_year else input$slider_cma_date
    
    fake_cma %>%
      dplyr::mutate(cmapuid = as.character(cmapuid)) %>% 
      dplyr::filter(year == year_select) %>%
      right_join(can_cma_shapes@data, by = c('cmapuid' = 'CMAPUID'))
  })
  
  reactive_cma_values <- reactive({
    reactive_cma_data() %>% pull(value)
  })
  
  reactive_cma_labels <- reactive({
    reactive_cma_data() %>% 
    dplyr::mutate(label = paste0("<strong>", CMANAME, "</strong><br/><strong>", tr('yearly_sales'), ": ", value, "</strong>")) %>%
    pull(label) %>%
    lapply(htmltools::HTML)
  })
  
  
  bins_cma <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  pal_cma <- colorBin("YlOrRd", domain = c(0, max(fake_cma$value)), bins = bins_cma)
  
  # Basemap
  # TODO: highlight ui remains permanent
  output$leaflet_cma_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-95, 55, zoom = 5) %>% 
      addLegend(
        'topright',
        pal = pal_cma,
        values = c(0, cma_max_value),
        layerId = 'legend',
        title = paste0('<small>Amount of vehicles</small>'))
  })
  
  # Update legend for selected language
  observeEvent(input$btn_language, {
    leafletProxy('leaflet_cma_map') %>% 
      removeControl('legend') %>% 
      addLegend(
        'topright',
        pal = pal_cma,
        values = c(0, cma_max_value),
        layerId = 'legend',
        title = paste0('<small>', tr('yearly_sales'), '</small>'))
  })
  
  # Update map for selected year and language
  observeEvent(c(input$slider_cma_date, input$btn_language), {
    leafletProxy('leaflet_cma_map') %>%
      clearShapes() %>%
      addPolygons(
        data = can_cma_shapes,
        fillColor = ~pal_cma(reactive_cma_values()),
        weight = 2,
        fillOpacity = 0.7,
        label = reactive_cma_labels(),
        highlight = highlightOptions(
          fillColor = 'blue',
          bringToFront = TRUE)
      )
  })
  
  
  

  # Generating Report -------------------------------------------------------
  
  # UI components
  output$report <- renderText({
    tr('report')
  })
  
  output$generate_report <- renderText({
    tr('generate_report')
  })
  
  output$select_report_province <- renderUI({
    selectInput(
      inputId = 'select_report_province',
      label = tr('province'),
      choices = provinces(),
      selected = provinces()[1]
    )
  })
  
  
  output$btn_download_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
        paste0(input$select_report_province, '-report.html')
      },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(nmvr_data = reactive_nmvr_data(), 
                     nmvs_data = reactive_nmvs_data(),
                     province = input$select_report_province)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}
