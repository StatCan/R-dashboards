### valueBox without ShinyDashboard
### from https://www.r-bloggers.com/2018/06/valuebox-without-shinydashboard-2/

### developer: Tamas Markó
### email: tamas.marko@myhappydata.com

my_valueBox <- function(value, subtitle, icon=NULL, color=NULL, size="normal") {
  if (size == "small") {
    icon_size = "font-size: 100%;"
    value_style = "font-size: 125%; font-weight: bold;"
    subtitle_style = "font-size: 100%;"
  } else {
    icon_size = "font-size: 175%;"
    value_style = "font-size: 150%; font-weight: bold;"
    subtitle_style = "font-size: 100%;"
  }
  
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary", style = "margin-bottom:10px;",
          div(class = "panel-heading", style = "padding: 5px 10px;",
              style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-sm-9 text-left", style = value_style, value),
                  div(class = "col-sm-3 text-right", style = icon_size, icon(icon))),
              div(class = "row",
                  div(class = "col-sm-12 text-left", style = subtitle_style, subtitle))
          )#,
          #div(class = "panel-footer",
          #    div(class = "clearfix")
        )
    )
}
# 
# valueBox_demo <- function() {
#   ui <- fluidPage(
#     fluidRow(
#       valueBox(value = 123,
#                subtitle = "Test box with icon",
#                icon = "users"), 
#       valueBox(value = "ABCD",
#                subtitle = "Text box without icon"),
#       valueBox(value = "X",
#                subtitle = "Text box",
#                color = "#FFFFFF")
#     ),
#     fluidRow(
#       valueBox(value = 123,
#                subtitle = "Median age at certification",
#                size = "small"), 
#       valueBox(value = "Canada",
#                subtitle = "Median age at certification Median age at certification",
#                icon = "users", size = "small"),
#       valueBox(value = "2013",
#                subtitle = "Year of certification",
#                size = "small")
#     ))
#   
#   server <- function(input, output, session) {
#   }
#   shinyApp(ui, server)
# }
# 
# valueBox_demo()

