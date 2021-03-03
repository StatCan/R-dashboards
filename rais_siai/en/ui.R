
lang_set <- c(
  "English"="en",
  "Français"="fr")

ui <- fluidPage(

  # To set the main plot size in response to the browser windows size,
  # read in the current windows size.
  tags$head(
    tags$script(
      '$(document).on("shiny:connected", function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
           $(window).resize(function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
        ')),

  shiny.i18n::usei18n(i18n),
  title="Interprovincial mobility of journeypersons in Canada",

  fluidRow(
    column(10, h3(i18n$t("Interprovincial mobility of journeypersons in Canada"))),

    column(2, style="margin-top: 10px;",
           selectInput('selected_language',
                       label=NULL,
                       choices=lang_set,
                       selected="en"))
    ),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # To have choices depend upon language selection,
      # input controls are defined in server using renderUI.

      uiOutput("year_control"),

      uiOutput("time_control"),

      uiOutput("trade_control"),

      uiOutput("mode_control"),

      uiOutput("type_control"),

      checkboxInput(inputId = "all_regions",
                    label = i18n$t("All regions"),
                    value = TRUE),

      uiOutput("region_selection"),

      uiOutput("text1"),

      uiOutput("text2")
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      fillRow(
        plotOutput(outputId = "outPlot", width = "100%", height = "100%"),
        width = "100%"
      )
    )

  )
)
