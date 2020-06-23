#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Based off of https://github.com/eparker12/nCoV_tracker


library(leaflet)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)


# Load data and preprocess
provinces_latlong <- read_csv('data/raw/provinces_latlong.csv') %>% 
  janitor::clean_names()

# NMVR = ev_data
ev_data <- read_csv('data/raw/ev_registrations.csv') %>% 
  janitor::clean_names() %>% 
  filter(str_detect(vehicle_type, '^Total')) %>% 
  filter(geo != 'Canada') %>% 
  select(year = ref_date, geo, fuel_type, amount = value) %>%
  replace_na(list(amount = 0)) %>% 
  mutate(geo = recode(geo, `British Columbia and the Territories` = 'British Columbia')) %>% # Fix later
  left_join(provinces_latlong, by = c('geo' = 'province')) %>% 
  group_by(geo, fuel_type) %>% 
  mutate(cumsum = cumsum(amount))

fuel_types <- ev_data$fuel_type %>% unique()
ev_fuel_types <- c('Battery electric', 'Plug-in hybrid electric') 
provinces <- ev_data$geo %>% unique() %>% sort()

min_year <- ev_data$year %>% min()
max_year <- ev_data$year %>% max()


# Plotting parameters for map
bins <- c(0, 100, 10000, 100000, 500000, Inf)
max_value = ev_data$amount %>% max()
ev_pal <- colorBin('Blues', domain = c(0, max_value), bins = bins)


# Create basemap
basemap <- leaflet() %>% 
  addTiles() %>% 
  addLayersControl(
    position = 'topright',
    overlayGroups = fuel_types,
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  hideGroup(fuel_types[-1]) %>% 
  setView(-95, 55, zoom = 5) %>% 
  addLegend('topright', pal = ev_pal, values = c(0, max_value),
            title = '<small>Amount of vehicles</small>')


ui <- bootstrapPage(
  shinyjs::useShinyjs(),
  
  navbarPage(theme = shinytheme('flatly'), collapsible = TRUE, 'VRF Data Visualisation', id='nav',
             
             tabPanel('Map View',
                      div(class='outer', tags$head(includeCSS('styles.css')),
                          leafletOutput('mymap', width = '100%', height = '100%'),
                          absolutePanel(
                            id = 'controls', class = 'panel panel-default', top = 80, left = 20, width = 250, fixed = TRUE,
                            draggable = TRUE, height = 'auto',
                            
                            h3(textOutput('reactive_total_new_vehicles'), align = 'right'),
                            span(h4(textOutput('reactive_total_new_gv'), align = 'right'), style="color:#cc4c02"),
                            span(h4(textOutput('reactive_total_new_zev'), align = 'right'), style="color:#006d2c"),
                            
                            sliderInput(
                              'plot_date',
                              label = 'Year',
                              value = max_year,
                              min = min_year,
                              max = max_year,
                              step = 1,
                              sep = '',
                              animate = animationOptions(interval = 2000, loop = FALSE)
                            )
                          ) # absolute Panel
                      ) # div outer
             ), # Tab panel
             
             tabPanel('Growth view',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            'group_select', 'Group by',
                            choices = c('Province', 'Fuel type')),
                          selectInput(
                            'province_select', 'Province',
                            choices = provinces,
                            selected = 'Ontario'
                          ),
                          selectInput(
                            'fuel_type_select', 'Fuel type',
                            choices = fuel_types
                          ),
                          width = 2
                        ),
                        mainPanel(
                          plotlyOutput('time_series_plot'),
                          plotlyOutput('bar_chart_plot'),
                          plotlyOutput('sunburst_plot')
                        )
                      ) # sidebar layout
             ) # tab panel
  ) # navbar page
)
