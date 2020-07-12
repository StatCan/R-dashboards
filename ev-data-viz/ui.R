#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Based off of https://github.com/eparker12/nCoV_tracker


# TODO: Rename some label IDs for consistency


library(leaflet)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(tidyverse)


navbarPageWithButton <- function(..., button) {
  navbar <- navbarPage(...)
  div <- tags$div(class = "navbar-form", style = 'float: right', button)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], div)
  navbar
}


ui <- bootstrapPage(
  shinyjs::useShinyjs(),
  
  navbarPageWithButton(
    theme = shinytheme('flatly'), title='VRF Data Visualization', id='nav',
             
    tabPanel(textOutput('nmvr_map_view'),
      div(class='outer', tags$head(includeCSS('./www/styles.css')),
        leafletOutput('mymap', width = '100%', height = '100%'),
        absolutePanel(
          id = 'controls', class = 'panel panel-default', top = 80, left = 20, width = 250, fixed = TRUE,
          draggable = TRUE, height = 'auto',
          
          h3(textOutput('reactive_total_new_vehicles'), align = 'right'),
          span(h4(textOutput('reactive_total_new_gv'), align = 'right'), style="color:#cc4c02"),
          span(h4(textOutput('reactive_total_new_zev'), align = 'right'), style="color:#006d2c"),
          uiOutput('slider_input_plot_date')
        ) # absolute Panel
      ) # div outer
    ), # Tab panel
             
    tabPanel(textOutput('nmvr_time_view'),
      sidebarLayout(
        sidebarPanel(
          uiOutput('select_group_by'),
          uiOutput('select_input_province'),
          uiOutput('select_input_fuel_type'),
          width = 2
        ),
        mainPanel(
          plotlyOutput('time_series_plot') %>% withSpinner() ,
          br(), br(),
          plotlyOutput('bar_chart_plot') %>% withSpinner(),
          plotlyOutput('sunburst_plot')
        )
      ) # sidebar layout
    ), # tab panel
             
    tabPanel(textOutput('nmvs_time_view'),
      sidebarLayout(
        sidebarPanel(
          uiOutput('nmvs_select_sale_type'),
          uiOutput('nmvs_select_vehicle_type'),
          uiOutput('nmvs_select_origin_manufacture'),
          width = 2
        ),
        mainPanel(
          plotlyOutput('nmvs_time_series_plot') %>% withSpinner()
        )
      ) # side bar layout
    ), #tab panel
    
    tabPanel(
      textOutput('text_cma_map'),
      div(class='outer', tags$head(includeCSS('./www/styles.css')),
         leafletOutput('leaflet_cma_map', width = '100%', height = '100%'),
         absolutePanel(
           id = 'controls', class = 'panel panel-default', top =200, left = 20, width = 250, fixed = TRUE,
           draggable = TRUE, height = 'auto',

           h3(textOutput('text_fake_data_message')),
           uiOutput('slider_cma_date')
         ) # absolute Panel
      ) # div outer
    ), #tab panel
             
    tabPanel(
      title = textOutput('report'),
      sidebarLayout(
        sidebarPanel(
          uiOutput('select_report_province'),
          downloadButton("btn_download_report", textOutput('generate_report'))
        ),
        mainPanel()
      ) # side bar layout
    ), # tab panel

    button = actionButton('btn_language', label = textOutput('label_language'))
    
  ) # navbar page
) # bootstrap page




