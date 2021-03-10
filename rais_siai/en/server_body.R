# the body of the server is identical in the two versions.
# define it in a separate file and source this to avoid duplication.
# not sure why, but sourcing 'server.R' itself does not work.

# load in the data file
full <- read_csv("../data/mig_mat.csv") %>%
  as.data.frame()
# load in the meta data

meta <- read_csv("../data/metadata.csv")

# define region and colour of each region as named lists.
region = setNames(meta$region, meta$code)
colour = setNames(meta$colour, meta$code)

# load in the dictionary.
# (currently) shiny.i18n does not support translation of lists
# translation of the entries of dropdown menus is handled manually
# by using this dictionary.
dictionary <- read_csv("../dictionary/translation_fr.csv")


observeEvent(input$selected_language, {
  # Here is where we update language in session
  shiny.i18n::update_lang(session, input$selected_language)
})

# the dictionary reacts to the selected language
selected_dict <- reactive({
  req(input$selected_language)
  dict <- dictionary %>%
    pull(input$selected_language)
})

# the most recent year (cohort) is used to define the range of
# cohorts in the dropdown menu.
# but it depends on the time (year after certification)
# so define "last_yr" as reactive
last_yr <- reactive({
  req(input$time)

  last_year <- full %>%
    filter(dim_years == input$time) %>%
    pull(REF_DATE) %>%
    max()
})

# dropdown menu for year (cohort)
# note last_yr() is used as it's reactive.
# last_yr() appears first in the list, and it goes back to 2008.
output$year_control <- renderUI({
  selectizeInput(
    inputId = "year",
    label = i18n$t("Year of certification"),
    choices = c(last_yr():2008),
    selected = last_yr()
  )
})

# time (year after certification)
# translation is done manually using the selected_dict().
output$time_control <- renderUI({
  radioButtons(
    inputId = "time",
    label = NULL,
    choices = setNames(1:2, selected_dict()[16:17]),
    selected = 1
  )
})

# trade
output$trade_control <- renderUI({
  selectInput(
    inputId = "trade",
    label = i18n$t("Selected trades"),
    choices = setNames(1:5, selected_dict()[c(8:12)]),
    selected = 1
  )
})

# mode of certification
output$mode_control <- renderUI({
  selectInput(
    inputId = "mode",
    label = i18n$t("Modes of certification"),
    choices = setNames(1:3, selected_dict()[13:15]),
    selected = 1
  )
})

# type of mobility
output$type_control <- renderUI({
  selectInput(
    inputId = "type",
    label = i18n$t("Types of mobility"),
    choices = setNames(1:3, selected_dict()[18:20]),
    selected = 1
  )
})

# region, only appears on the screen if 'all region' is ticked off
# no default value is provided
output$region_selection <- renderUI({
  if (!input$all_regions) {
    selectizeInput(
      inputId = "sel_region",
      label = NULL,
      choices = setNames(c("", 1:11), selected_dict()[23:34]),
      selected = NULL
    )
  }
})

# text and link to the table
# change depending on the selected language.
output$text1 <- renderUI({
  if (input$selected_language == "en") {
    url <-
      a("Table 37-10-0204-01", href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3710020401")
    tagList("Statistics Canada. ", url)
  } else {
    url <-
      a("Tableau 37-10-0204-01", href = "https://www150.statcan.gc.ca/t1/tbl1/fr/tv.action?pid=3710020401")
    tagList("Statistique Canada. ", url)
  }
})

# text and link to the TRG
# change depending on the selected language.
output$text2 <- renderUI({
  if (input$selected_language == "en") {
    url <-
      a("reference guide", href = "https://www150.statcan.gc.ca/n1/en/catalogue/37200001")
    tagList("See ", url,  " for definitions.")
  } else {
    url <-
      a("le guide de référence", href = "https://www150.statcan.gc.ca/n1/fr/catalogue/37200001")
    tagList("Consultez ", url, " pour les définitions.")
})

output$outPlot <- renderPlot({
  df <- full %>%
    subset(
      REF_DATE == input$year &
        dim_trad == input$trade &
        dim_mode == input$mode &
        dim_years == input$time &
        dim_type == input$type,
      select = c(from, to, VALUE)
    )

  # the province names also need to change depending on the selected language.
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
  validate(need(nrow(df) > 0, message = i18n$t("No data available.")))

  # main code for the chord diagram.
  # largely based on https://github.com/guyabel/migest/blob/master/demo/cfplot_reg2.
  circos.clear()
  circos.par(
    cell.padding = c(0.01, 0.01, 0.01, 0.01),
    track.margin = c(0.01,-0.01),
    start.degree = 90,
    gap.degree = 2,
    canvas.xlim = c(-0.95, 0.95),
    canvas.ylim = c(-0.95, 0.95)
  )

  # region, code, and colour are defined in metadata.
  chordDiagram(
    x = df,
    group = region,
    order = meta$code,
    grid.col = colour,
    transparency = 0.25,
    directional = 1,
    direction.type = "diffHeight+arrows",
    link.arr.type = "big.arrow",
    diffHeight = -0.03,
    link.sort = TRUE,
    link.largest.ontop = TRUE,
    annotationTrack = c("grid"),
    preAllocateTracks = list(track.height = 0.25)
  )

  circos.track(
    track.index = 1,
    bg.border = NA,
    panel.fun = function(x, y) {
      #s = get.cell.meta.data("sector.index")
      l = as.numeric(get.cell.meta.data("sector.index"))
      # replace the label by abbreviations in the selected language
      s = pr_abbr()[l]
      xx = get.cell.meta.data("xlim")
      circos.text(
        x = mean(xx),
        y = 0.3,
        labels = s,
        cex = 1.2,
        adj = c(0, 0.5),
        facing = "clockwise",
        niceFacing = TRUE
      )
      circos.axis(
        h = "bottom",
        labels.cex = 0.6,
        labels.pos.adjust = TRUE,
        labels.facing = "outside",
        labels.niceFacing = TRUE
      )
    }
  )
},
# the plot height is adjusted by the browser window size.
height = reactive(
  ifelse(
    !is.null(input$innerSize),
    input$innerSize,
    500
  )
))
