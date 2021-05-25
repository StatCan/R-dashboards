mob_measure_ui <- function(id) {
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(NS(id, "comp_control")),
      uiOutput(NS(id, "year_control")),
      uiOutput(NS(id, "time_control")),
      uiOutput(NS(id, "region_selection")),
      uiOutput(NS(id, "trade_control")),
      uiOutput(NS(id, "mode_control")),
      uiOutput(NS(id, "type_control")),
      uiOutput(NS(id, "unit_control")),
      uiOutput(NS(id, "text_source"))
    ), 
    
    mainPanel(
      fluidRow(
        valueBoxOutput(NS(id, "vbox_prov"), width = 8),
        valueBoxOutput(NS(id, "vbox_year"), width = 4)),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_trade"), width = 12)),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_cohort")),
        valueBoxOutput(NS(id, "vbox_medage")),
        valueBoxOutput(NS(id, "vbox_absence"))),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_net_measure")),
        valueBoxOutput(NS(id, "vbox_in_measure")),
        valueBoxOutput(NS(id, "vbox_out_measure"))),

      #tableOutput(NS(id, "outtable")),
      fillRow(
        plotlyOutput(NS(id, "outPlot"), height = "500px"),
        width = "100%"
      )
    )
  )
}

mob_measure_server <- function(id, language) {

  moduleServer(id, function(input, output, session) {
    source("../R/format_number.R")
    source("../R/valuebox.R")
    
    dictionary <- read.csv('../dictionary/dict_mobility_measures.csv') %>%
      split(.$key)
    
    # uses a reactiveVal language.
    tr <- function(key) {
      dictionary[[key]][[language()]]
    }
    
    geo_names <- reactive(tr("mem_geo"))
    grp = reactive(setNames(c(1:3,19,20), tr("mem_trade_grp")))
    rs = reactive(setNames(c(4:18), tr("mem_trade_rs")))
    trade_names <- reactive({
      c(grp(), rs()) %>% sort() %>% names()
    })
    
    # load in the data file
    # first, try to download the Rds file from GitHub
    tmp <- tempfile()
    resp <-
      GET(
        "https://github.com/parlerBinou/Shiny-RAIS-longitudinal/raw/main/data/mobility_measures.Rds",
        write_disk(tmp)
      )
    # check if the response was "successful" (200)
    if (resp$status_code == 200) {
      # then load the data from downloaded RDS file.
      full <- readRDS(tmp)
      unlink(tmp)
    } else {
      # to use pre-downloaded Rds file
      full <- readRDS("../data/mobility_measures.Rds")
    }
    
    full <- full %>%
      pivot_wider(id_cols=c(REF_DATE, dim_geo, dim_trade, dim_mode, dim_years, dim_type),
                  names_from=dim_ind, values_from=c(VALUE, STATUS)) %>%
      as.data.frame()
    
    last_years <- full %>%
      subset(!is.na(VALUE_3),
             select = c(REF_DATE, dim_years)) %>%
      group_by(dim_years) %>%
      summarize(last_year = max(REF_DATE))
    
    # time (year after certification)
    output$time_control <- renderUI({
      radioButtons(
        inputId = NS(id, "time"),
        label = NULL,
        choices = setNames(1:2, tr("mem_year")),
        selected = 1
      )
    })
    
    # the most recent year (cohort) is used to define the range of
    # cohorts in the drop down menu.
    # but it depends on the time (year after certification)
    # so define "last_yr" as reactive
    last_yr <- reactive({
      req(input$time)
      
      last_years %>%
        filter(dim_years == input$time) %>%
        pull(last_year)
    })
    
    # to make the cohort selection persistent even if input$time changed,
    # define it as a reactiveVal.
    # initialize it with the most recent available cohort.
    selected_cohort <- reactiveVal(
      max(filter(full, !is.na(VALUE_3))$REF_DATE))
    
    # get the selected cohort value and update selected_cohort.
    get_cohort <- function() {
      selected_cohort(max(input$year))
    }
    
    # reset the stored value when the selected_chort is invalid.
    reset_cohort <- function() {
      selected_cohort(last_yr())
    }
    
    # observe changes in input$year and update the stored value in selected_cohort.
    observeEvent(input$year, get_cohort())
    # observe changes in input$time
    # if the stored value in selected_cohort is invalid, reset it.
    observeEvent(input$time, {
      if (selected_cohort() > last_yr()) {reset_cohort()}
    })
    
    
    #  menu for year (cohort)
    # note last_yr() is used as it's reactive.
    # last_yr() appears first in the list, and it goes back to 2008.
    output$year_control <- renderUI({
      req(input$comp)
      if (input$comp == 3) {
        sliderTextInput(
          inputId = NS(id,"year"),
          label = tr("lab_cert_year"), 
          choices = c(2008:last_yr()),
          selected = c(2008, selected_cohort())
        )
      } else {
        pickerInput(
          inputId = NS(id, "year"),
          label = tr("lab_cert_year"),
          choices = c(last_yr():2008),
          selected = selected_cohort(),
          multiple = FALSE
        )
      }
    })
    
    # comparison dimension
    output$comp_control <- renderUI({
      radioButtons(
        inputId = NS(id, "comp"),
        label = tr("lab_comp"),
        choices = setNames(c(1:3), tr("mem_comp")),
        selected = 1
      )
    })
    
    # region
    output$region_selection <- renderUI({
      req(input$comp)
      if (input$comp == 1) {
        multi_selection <- TRUE
        default_selection <- c(1:11)
        options_set <- pickerOptions(
          actionsBox = TRUE,
          selectAllText = tr("sAll_lbl"),
          deselectAllText = tr("dsAll_lbl"),
          noneSelectedText = tr("text_no_geo"))
      } else {
        multi_selection <- FALSE
        default_selection <- 1
        options_set <- NULL
      }
      pickerInput(
        inputId = NS(id, "geo"),
        label = tr("lab_geo"),
        choices = setNames(c(1:12), tr("mem_geo")),
        multiple = multi_selection,
        selected = default_selection,
        options = options_set
      )  
    })
    
    # trade
    output$trade_control <- renderUI({
      req(input$comp)
      choice_set = list(
        grp = grp(),
        rs = rs()
      )
      names(choice_set) <- c(tr("lab_trade_grp"), tr("lab_rs"))
      
      if (input$comp == 2) {
        multi_selection <- TRUE
        default_selection <- c(1:3,19,20)
        options_set <- pickerOptions(
          actionsBox = TRUE,
          selectAllText = tr("sAll_lbl"),
          deselectAllText = tr("dsAll_lbl"),
          noneSelectedText = tr("text_no_trade"))
      } else {
        multi_selection <- FALSE
        default_selection <- 1
        options_set <- NULL
      }
      
      pickerInput(
        inputId = NS(id, "trade"),
        label = tr("lab_trade"),
        choices = choice_set,
        multiple = multi_selection,
        selected = default_selection,
        options = options_set
      ) 
    })
    
    # mode of certification
    output$mode_control <- renderUI({
      selectInput(
        inputId = NS(id, "mode"),
        label = tr("lab_mode"),
        choices = setNames(1:3, tr("mem_mode")),
        selected = 1
      )
    })
    
    # type of mobility
    output$type_control <- renderUI({
      selectInput(
        inputId = NS(id, "type"),
        label = tr("lab_type"),
        choices = setNames(1:3, tr("mem_type")),
        selected = 1
      )
    })
    
    output$unit_control <- renderUI({
      req(input$comp)
      if (input$comp == 3) {
      radioButtons(
        inputId = NS(id, "unit"),
        label = NULL,
        choices = setNames(1:2, tr("mem_unit")), 
        selected = 2)
      }
    })
    
    output$text_source <- renderUI({
      if (language() == "en") {
        url <-
          a("Table 37-10-0205-01", href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3710020501")
        tagList("Statistics Canada. ", url)
      } else {
        url <-
          a("Tableau 37-10-0205-01", href = "https://www150.statcan.gc.ca/t1/tbl1/fr/tv.action?pid=3710020501")
        tagList("Statistique Canada. ", url)
      }
    })
    
    # creating outputs
    
    df <- reactive({
      req(input$year)
      
      if (is.null(input$trade)) {
        selected_trades <- c(1:3,19,20)
      } else { 
        selected_trades <- input$trade}
      
      if (is.null(input$geo)) {
        selected_geo <- c(1:11)
      } else {
        selected_geo <- input$geo
      }
      
      df <- full %>%
        subset(
          REF_DATE %in% c(min(input$year):max(input$year)) &
            dim_geo %in% selected_geo &
            dim_trade %in% selected_trades &
            dim_mode == input$mode &
            dim_years == input$time &
            dim_type == input$type) %>%
        arrange(dim_geo, dim_trade, desc(REF_DATE)) %>%
        mutate(
          supp = c(1:max(nrow(.), 1)),
          label1 = geo_names()[dim_geo],
          label2 = trade_names()[dim_trade],
          label3 = as.character(REF_DATE)
        )
    })
    
    
    # render the table for testing / debugging
    output$outtable <- renderTable(
      df()
    )

    output$outPlot <- renderPlotly({
      
      # check if there are data points left after the filtering.
      validate(need(nrow(df()) > 0, message = tr("text_no_data")))

      # if comparing across geography or trade, make horizontal bar charts
      if (input$comp != 3) {
        tick_label <- if (input$comp == 1) {df()$label1} else {df()$label2}
        
        net_text <- format_number(df()$VALUE_11, locale = language())
        in_text <- format_number(df()$VALUE_9, locale = language())
        out_text <- format_number(df()$VALUE_10, locale = language())
        
        fig <- plot_ly(x = replace_na(df()$VALUE_9, 0), y = df()$supp,
                       name = tr("in"), type = "bar", orientation = "h",
                       text = in_text, marker = list(color = '117733'),
                       hovertemplate = "%{y}: %{text}%", source = "mm"
                       # when comparing across geography and Canada is selected, show
                       # In and Out and hide Net - always zero.
                       # otherwise, show Net only by default.
                       # visible = ifelse(
                       #   (input$comp == 2 & input$geo == 1), TRUE, "legendonly")
          ) %>%
          add_trace(x = replace_na(df()$VALUE_10, 0), name = tr("out"),
                    text = out_text, marker = list(color = '882255')
                    # visible = ifelse(
                    #   (input$comp == 2 & input$geo == 1), TRUE, "legendonly")
          ) %>%
          add_trace(x = replace_na(df()$VALUE_11, 0), name = tr("net"),
                    text = net_text, marker = list(color = '332288')
                    # visible = ifelse(
                    #   (input$comp == 2 & input$geo == 1), "legendonly", TRUE)
          ) %>%
          layout(
            yaxis = list(
              ticktext = tick_label,
              tickvals = df()$supp,
              autorange = "reversed"
            ),
            barmode = "group",
            legend=list(
              traceorder = "normal", orientation="h", yanchor="bottom",
              y=1, xanchor="left", x=0)
          ) 
      } else {
        # compare across cohorts (year of certification)
        # two possibilities - by number or by percent.
        req(input$unit)

        if (input$unit == 1) {
          net_measure <- df()$VALUE_8
          in_measure <- df()$VALUE_6
          out_measure <- df()$VALUE_7
          
          hover_template <- "%{x}: %{text}"
        } else {
          net_measure <- df()$VALUE_11
          in_measure <- df()$VALUE_9
          out_measure <- df()$VALUE_10
          
          hover_template <- "%{x}: %{text} %"
        }
        net_text <- format_number(net_measure, locale=language())
        in_text <- format_number(in_measure, locale=language())
        out_text <- format_number(out_measure, locale=language())

        fig <- 
          plot_ly(x = df()$supp, y = replace_na(in_measure, 0), name = tr("in"),
                  type = "bar", text = in_text, marker = list(color = '117733'),
                  hovertemplate = hover_template, source = "mm"
                  # visible = ifelse(
                  #     (input$geo == 1), TRUE, "legendonly")
                  ) %>%
          add_trace(y = replace_na(out_measure, 0), name = tr("out"),
                    text = out_text, marker = list(color = '882255')
                    # visible = ifelse(
                    #   (input$geo == 1), TRUE, "legendonly")
                    ) %>%
          add_trace(y = replace_na(net_measure, 0), name = tr("net"),
                    text = net_text, marker = list(color = '332288')
                    # visible = ifelse(
                    #   (input$geo == 1), "legendonly", TRUE)
                    ) %>%
          layout(
            barmode = "group",
            legend=list(
              traceorder = "normal", orientation="h", yanchor="bottom",
              y=1, xanchor="left", x=0),
            xaxis = list(
              ticktext = df()$label3,
              tickvals = df()$supp,
              autorange = "reversed")
            )

      }
    })
    
    
    # define the index of values to be shown in text boxes
    selected_supp <- reactiveVal(1)

    
    get_clicked <- function() {
      clk <- event_data("plotly_click", source = "mm")
      if (is.null(clk)) {selected_supp(1)} else {
        if (input$comp == 3) { selected_supp(clk$x) } else { selected_supp(clk$y) }
      }
    }
    
    
    reset_selection <- function() {
      selected_supp(1)
    }
    
    # if click on plotly, read the selected index
    observeEvent(event_data("plotly_click", source = "mm"), get_clicked())
    
    # if anything changes in df(), reset the selection
    observeEvent(df(), reset_selection())
    
    # render value boxes
    output$vbox_year <- renderValueBox({
      my_valueBox(
        df()$REF_DATE[df()$supp == selected_supp()], tr("lab_cert_year"),
        icon = "calendar")
    })
    
    output$vbox_prov <- renderValueBox({
      my_valueBox(
        df()$label1[df()$supp == selected_supp()], tr("lab_geo"),
        icon = "map-marker")
    })
    
    output$vbox_trade <- renderValueBox({
      my_valueBox(
        df()$label2[df()$supp == selected_supp()], tr("lab_trade"),
        icon = "toolbox")
    })
    
    value_status <- function(value, status) {
      if (is.na(value)) {
        status
      } else {
        format_number(value, locale = language())
      }
    }
    
    value_status_two <- function(val1, stat1, val2, stat2, two_in_pct=TRUE) {
      out1 <- value_status(val1, stat1)
      if (is.na(val2)) {
        out2 <- NULL
      } else {
        if (two_in_pct) {
          endian <- " %)"
        } else {
          endian <- " )"
        }
        out2 <- paste0("(", value_status(val2, stat2), endian)  
      }
      
      return(paste(out1, out2))
    }
    
    output$vbox_cohort <- renderValueBox({
      my_valueBox(
        value_status_two(
          df()$VALUE_1[df()$supp == selected_supp()],
          df()$STATUS_1[df()$supp == selected_supp()],
          df()$VALUE_3[df()$supp == selected_supp()],
          df()$STATUS_3[df()$supp == selected_supp()],
          two_in_pct = FALSE),
        paste0(tr("cohort"), " (", tr("taxfilers"), ")"),
        icon = "users", size = "small")
    })
    
    output$vbox_medage <- renderValueBox({
      my_valueBox(
        value_status(
          df()$VALUE_2[df()$supp == selected_supp()],
          df()$STATUS_2[df()$supp == selected_supp()]),
        tr("medage"), icon = "award", size = "small")
    })
    
    output$vbox_absence <- renderValueBox({
      my_valueBox(
        value_status_two(
          df()$VALUE_4[df()$supp == selected_supp()],
          df()$STATUS_4[df()$supp == selected_supp()],
          df()$VALUE_5[df()$supp == selected_supp()],
          df()$STATUS_5[df()$supp == selected_supp()]),
        tr("absence"), icon = "house-user", size = "small")
    })
    
    output$vbox_net_measure <- renderValueBox({
      my_valueBox(
        value_status_two(
          df()$VALUE_8[df()$supp == selected_supp()],
          df()$STATUS_8[df()$supp == selected_supp()],
          df()$VALUE_11[df()$supp == selected_supp()],
          df()$STATUS_11[df()$supp == selected_supp()]),
        tr("net"), icon = "exchange-alt", size = "small")
    })
    
    output$vbox_in_measure <- renderValueBox({
      my_valueBox(
        value_status_two(
          df()$VALUE_6[df()$supp == selected_supp()],
          df()$STATUS_6[df()$supp == selected_supp()],
          df()$VALUE_9[df()$supp == selected_supp()],
          df()$STATUS_9[df()$supp == selected_supp()]),
        tr("in"), icon = "sign-in-alt", size = "small")
    })
    
    output$vbox_out_measure <- renderValueBox({
      my_valueBox(
        value_status_two(
          df()$VALUE_7[df()$supp == selected_supp()],
          df()$STATUS_7[df()$supp == selected_supp()],
          df()$VALUE_10[df()$supp == selected_supp()],
          df()$STATUS_10[df()$supp == selected_supp()]),
        tr("out"), icon = "sign-out-alt", size = "small")
    })
    
  })

}

# for testing

measure_demo <- function() {
  ui <- fluidPage(
    mob_measure_ui("x"))
  server <- function(input, output, session) {
    mob_measure_server("x", "en")
  }
  shinyApp(ui, server)
}


# measure_demo()
