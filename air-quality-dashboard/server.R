#!/usr/bin/env Rscript

library(shiny)
library(tidyverse)
library(lubridate)

function(input, output, session) {
  
  left_df <- reactive(
    data[[input$left_period]] %>%
      filter(input$left_country == Country))
  
  left_plot_df <- reactive(filter(left_df(), input$left_city == City))
  
  right_df <- reactive(
    data[[input$right_period]] %>%
      filter(input$right_country == Country))
  
  right_plot_df <- reactive(filter(right_df(), input$right_city == City))
  
  observeEvent(
    eventExpr = input$left_country,
    handlerExpr = {
      choice <- left_df() %>% select(City) %>% unique()
      updateSelectInput(session, "left_city", choices = choice, selected = "Toronto")
      })

  observeEvent(
    eventExpr = input$right_country,
    handlerExpr = {
      choice <- right_df() %>% select(City) %>% unique()
      updateSelectInput(session, "right_city", choices = choice, selected = "Toronto")
      })
  
  output$left_plot_co <- renderPlot({
    colorful_plot(left_plot_df, right_plot_df, "co", "red")
  })
  output$left_plot_no2 <- renderPlot({
    colorful_plot(left_plot_df, right_plot_df, "no2", "blue")
  })
  output$left_plot_so2 <- renderPlot({
    colorful_plot(left_plot_df, right_plot_df, "so2", "purple")
  })
  
  output$right_plot_co <- renderPlot({
    colorful_plot(right_plot_df, left_plot_df, "co", "red")
  })
  output$right_plot_no2 <- renderPlot({
    colorful_plot(right_plot_df, left_plot_df, "no2", "blue")
  })
  output$right_plot_so2 <- renderPlot({
    colorful_plot(right_plot_df, left_plot_df, "so2", "purple")
  })
  
}