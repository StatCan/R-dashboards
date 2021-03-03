
server <- function(input, output, session) {
  full <- read_csv("data/mig_mat.csv") %>%
    as.data.frame()

  meta <- read_csv("data/metadata.csv")
  region = setNames(meta$region, meta$code)
  colour = setNames(meta$colour, meta$code)

  dictionary <- read_csv("dictionary/translation_fr.csv")


  observeEvent(input$selected_language, {
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })

  selected_dict <- reactive({
    req(input$selected_language)
    dict <- dictionary %>%
      pull(input$selected_language)
  })

  last_yr <- reactive({
    req(input$time)

    last_year <- full %>%
      filter(dim_years == input$time) %>%
      pull(REF_DATE) %>%
      max()
  })

  output$year_control <- renderUI({
    selectizeInput(inputId = "year",
                   label = i18n$t("Year of certification"),
                   choices = c(last_yr():2008),
                   selected = last_yr())
  })

  output$time_control <- renderUI({
    radioButtons(inputId = "time",
                 label = NULL,
                 choices = setNames(1:2, selected_dict()[16:17]),
                 selected = 1)
  })

  output$trade_control <- renderUI({
    selectInput(inputId = "trade",
                label = i18n$t("Selected trades"),
                choices = setNames(c(4, 1:3), selected_dict()[c(8, 10:12)]),
                selected = 4)
  })

  output$mode_control <- renderUI({
    selectInput(inputId = "mode",
                label = i18n$t("Modes of certification"),
                # choices = setNames(1:3, selected_dict()[13:15]),
                choices = setNames(1, selected_dict()[14:14]),
                selected = 1)
  })

  output$type_control <- renderUI({
    selectInput(inputId = "type",
                label = i18n$t("Types of mobility"),
                # choices = setNames(1:3, selected_dict()[18:20]),
                choices = setNames(1, selected_dict()[19:19]),
                selected = 1)
  })

  output$region_selection <- renderUI({
    if (!input$all_regions) {
      selectizeInput(inputId = "sel_region",
                     label = NULL,
                     choices = setNames(c("", 1:11), selected_dict()[23:34]),
                     selected = NULL)
    }
  })

  output$text1 <- renderUI({
    if (input$selected_language == "en") {
    url <- a("Table 37-10-0154-01", href="https://doi.org/10.25318/3710015401-eng")
    tagList("Statistics Canada. ", url)
    } else {
      url <- a("Tableau 37-10-0154-01", href="https://doi.org/10.25318/3710015401-fra")
      tagList("Statistique Canada. ", url)
    }
  })

  output$text2 <- renderUI({
    if (input$selected_language == "en") {
      url <- a("reference guide", href="https://www150.statcan.gc.ca/n1/en/catalogue/37200001")
      tagList("See ", url,  " for definitions.")
    } else {
      url <- a("le guide de référence", href="https://www150.statcan.gc.ca/n1/fr/catalogue/37200001")
      tagList("Voir ", url, " pour les définitions.")
    }
  })

  output$outPlot <- renderPlot({
    df <- full %>%
      subset(REF_DATE == input$year &
               dim_trad == input$trade &
               dim_mode == input$mode &
               dim_years == input$time &
               dim_type == input$type,
      select=c(from, to, VALUE))

    pr_abbr <- reactive({
      req(input$selected_language)
      if (input$selected_language == "en") {
        abbr <- meta$pr_en
      } else {
        abbr <- meta$pr_fr
      }
    })

    if (input$all_regions == FALSE) {
      req(input$sel_region, cancelOutput = TRUE)
      selected_region <- as.numeric(input$sel_region)
      df <- df %>%
        subset(from == selected_region | to == selected_region)
    }

    # check if there are data points left after the filtering.
    validate(
      need(nrow(df) > 0, message = i18n$t("No data available."))
    )

    circos.clear()
    circos.par(
      cell.padding = c(0.01, 0.01, 0.01, 0.01),
      track.margin = c(0.01, -0.01),
      start.degree = 90,
      gap.degree = 2,
      canvas.xlim = c(-0.95, 0.95),
      canvas.ylim = c(-0.95, 0.95))

    chordDiagram(x = df, group = region, order = meta$code,
                 grid.col = colour, transparency = 0.25,
                 directional = 1, direction.type = "diffHeight+arrows",
                 link.arr.type = "big.arrow", diffHeight = -0.03,
                 link.sort = TRUE, link.largest.ontop = TRUE,
                 annotationTrack = c("grid"), #c("grid", "axis"),
                 preAllocateTracks = list(track.height = 0.25))

    circos.track(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
      #s = get.cell.meta.data("sector.index")
      l = as.numeric(get.cell.meta.data("sector.index"))
      s = pr_abbr()[l]
      xx = get.cell.meta.data("xlim")
      circos.text(x = mean(xx), y=0.3,
                  labels = s, cex = 1.2, adj = c(0, 0.5),
                  facing = "clockwise", niceFacing = TRUE)
      circos.axis(h = "bottom",
                  labels.cex = 0.6,
                  labels.pos.adjust = TRUE,
                  labels.facing = "outside",
                  labels.niceFacing = TRUE)
    })


  }, height=reactive(ifelse(!is.null(input$innerSize),
                            input$innerSize, 500)))

}
