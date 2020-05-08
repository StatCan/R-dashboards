# Load the packages
library("cansim")
library("DT")
library("ggplot2")
library("lubridate")
library("plotly")
library("plyr")
library("readxl")
library("scales")
library("shiny")
library("shinycssloaders")
library("shinythemes")
library("tidyverse")
library("xlsx")

# Define UI
ui <- navbarPage(
    windowTitle = HTML("COVID-19 cases in Canada"),
    title = div("COVID-19 cases in Canada", img(src = "maple-leaf.png", style = "margin-left: 10px; margin-right: 5px; height: 20px; width: auto;")),
    theme = shinytheme("cerulean"),
    tabPanel("Home",
        sidebarPanel(
            uiOutput("snapshot"),
            uiOutput("age_group"),
            uiOutput("gender"),
            uiOutput("hospitalized"),
            uiOutput("icu"),
            uiOutput("death"),
            uiOutput("exposure"),
            uiOutput("x_axis"),
            width = 3
        ),
        mainPanel(
            tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }",
                "a:hover { text-decoration: none !important; }"
            ),
            div(tags$strong("Please use with caution: "), "this data is preliminary and subject to change. Please visit ", tags$a(href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310076601", target = "_blank", style = "color: #c27571; font-weight: bold; text-decoration: underline;", "this page"), "to learn more about the data.", style = "background-color: #f4e4e4; color: #c27571; border: 1px solid #efd5d9; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(),
            div(textOutput("case_count") %>% withSpinner(color = "#44ade9"), style = "font-weight: bold; font-size: 1.75rem; text-align: center;") %>% withSpinner(color = "#44ade9"), br(), br(),
            plotlyOutput("get_cumulative_incidence_plot") %>% withSpinner(color = "#44ade9"), br(), br(), br(),
            DTOutput("get_plot_table") %>% withSpinner(color = "#44ade9"), br(), br(),
            width = 9
        )
    ),
    tabPanel("Compare data snapshots",
        sidebarPanel(
            uiOutput("age_group2"),
            uiOutput("snapshot2"),
            uiOutput("x_axis2"),
            width = 3
        ),
        mainPanel(
            tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
            ),
            div(tags$strong("Note: "), "at least two data snapshots must be selected to make a comparison.", style = "background-color: #fcf9e7; color: #a99368; border: 1px solid #faefd4; border-radius: 3px; width: 100%; padding: 10px;"), br(), 
            plotlyOutput("get_comparison_plot") %>% withSpinner(color = "#44ade9"), br(), br(),
            width = 9
        )
    ),
    tabPanel("Data source",
        sidebarPanel(
         uiOutput("snapshot3"),
         width = 3
        ),
        mainPanel(
            tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
            ),
            div(tags$strong("Source: "), "Statistics Canada.", tags$a(href = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310076601", target = "_blank", style = "color: #a99368; font-weight: bold; text-decoration: underline;", "Table  13-10-0766-01."), " Detailed confirmed cases of coronavirus disease (COVID-19) (Preliminary data), Public Health Agency of Canada.", style = "background-color: #fcf9e7; color: #a99368; border: 1px solid #faefd4; border-radius: 3px; width: 100%; padding: 10px;"), br(), br(), 
            DTOutput("get_data_source") %>% withSpinner(color = "#44ade9"), br(), br(),
            width = 9
        )
    ),
    tabPanel("About",
        mainPanel(
            tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"
            ),
            p("This R Shiny app was developed by the Microsimulation Team within the Health Analysis Division at Statistics Canada."), br(), 
            width = 12
        )
    )
)

# Define server logic
server <- function(input, output) {
    # Cache select data structures
    cached <- reactiveValues()
    
    # Get data for the comparison of data snapshots
    get_comparison_data <- function(x) {
        if(is.null(x) | ! x %in% list.files()) {
            return(list(d = NULL, crosstab = NULL))
        } else {
            # Get data
            d <- read_xlsx(x)
            
            # Isolate date
            snapshot <- strsplit(x, "updated ")[[1]][2]
            snapshot <- gsub(".xlsx", "", snapshot)
            
            # Reformt the date
            snapshot <- format(as.Date(snapshot), "%B %d, %Y")
            
            # Add d to list
            l <- list(d = d)
            
            # Create crosstab for plot
            # Convert episode date to date object
            d$`Episode Date` <- as.Date(d$`Episode Date`, format = "%d-%b-%y")
            
            # Remove cases with no episode date
            d <- d %>% filter(! is.na(`Episode Date`))
            
            # Sort data by episode date
            d <- d[order(d$`Episode Date`),]
            
            # Create a crosstab
            crosstab <- d %>% group_by(`Episode Date`) %>% tally()
            
            # Rename the n vector
            names(crosstab)[ncol(crosstab)] <- "Incidence"
            
            # Compute cumulative incidence
            crosstab <- crosstab %>% mutate(`Cumulative Incidence` = cumsum(Incidence))
            
            # Create an age group vector
            crosstab <- crosstab %>% mutate(`Age Group` = rep("All ages", nrow(crosstab)))
            
            # Reorder columns
            crosstab <- crosstab %>% select(`Age Group`, everything())
            
            # Add day column
            crosstab <- crosstab %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = cached$day1))
            crosstab$Snapshot <- rep(snapshot, nrow(crosstab))
            
            # Create a crosstab by age group
            crosstab2 <- d %>% group_by(`Age Group`, `Episode Date`) %>% tally()
            
            # Rename the n vector
            names(crosstab2)[ncol(crosstab2)] <- "Incidence"
            
            # Restructure as tibble
            crosstab2 <- as_tibble(crosstab2)
            
            # Compute cumulative incidence
            #crosstab2 <- crosstab2 %>% group_by(`Age Group`) %>% mutate(`Cumulative Incidence` = cumsum(Incidence))
            crosstab2$`Cumulative Incidence` <- unlist(aggregate(crosstab2$Incidence, by = list(crosstab2$`Age Group`), cumsum)$x)
            
            # Restructure as tibble
            crosstab2 <- as_tibble(crosstab2)
            
            # Add day column
            crosstab2 <- crosstab2 %>% group_by(`Age Group`) %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = cached$day1))
            crosstab2$Snapshot <- rep(snapshot, nrow(crosstab2))
            
            # Restructure as tibble
            crosstab2 <- as_tibble(crosstab2)
            
            # Combine both crosstabs
            crosstab <- rbind(crosstab, crosstab2)
            
            # Ensure that the age group vector is a factor
            crosstab$`Age Group` <- factor(crosstab$`Age Group`)
            crosstab$Snapshot <- factor(crosstab$Snapshot)
            
            # Relevel the age group factor
            crosstab$`Age Group` <- relevel(crosstab$`Age Group`, ref = "All ages")
            
            # Add crosstab to list
            l$crosstab <- as.data.frame(crosstab)
            
            return(l)
        }
    }
    
    # Crosstab function for plots
    get_crosstab <- function(combo) {
        d <- cached$d
        e1 <- e2 <- e3 <- e4 <- e5 <- e6 <- c()
        if(length(grep("\\All\\b", combo$age_group)) == 0) {
            e1 <- "`Age Group` %in% combo$age_group &"
        } 
        if(length(grep("\\All\\b", combo$gender)) == 0) {
            e2 <- "Gender %in% combo$gender &"
        } 
        if(length(grep("\\All\\b", combo$hospitalized)) == 0) {
            e3 <- "Hospitalized %in% combo$hospitalized &"
        }
        if(length(grep("\\All\\b", combo$icu)) == 0) {
            e4 <- "`Intensive Care Unit` %in% combo$icu &"
        }
        if(length(grep("\\All\\b", combo$death)) == 0) {
            e5 <- "Death %in% combo$death &"
        }
        if(length(grep("\\All\\b", combo$exposure)) == 0) {
            e6 <- "`Exposure Setting` %in% combo$exposure &"
        }
        
        if(length(e1) > 0 | length(e2) == 0 | length(e3) == 0 | length(e4) == 0 | length(e5) == 0 | length(e6) == 0) {
            e <- paste0("d %>% filter(", e1, e2, e3, e4, e5, e6, ")")
            e <- gsub("&)", ")", e)
            
            #e <- paste0("d %>% filter(", e1, e2, ")", collapse = "")
            d <- eval(parse(text = e))
        } 
        
        # Convert episode date to date object
        d$`Episode Date` <- as.Date(d$`Episode Date`, format = "%d-%b-%y")
        
        # Remove cases with no episode date
        d <- d %>% filter(! is.na(`Episode Date`))
        
        # Sort data by episode date
        d <- d[order(d$`Episode Date`),]
        
        # Create a crosstab
        crosstab <- d %>% group_by(`Episode Date`) %>% tally()
        
        # Rename the n vector
        names(crosstab)[ncol(crosstab)] <- "Incidence"
        
        # Compute cumulative incidence
        crosstab <- crosstab %>% mutate(`Cumulative Incidence` = cumsum(Incidence))
        
        # Create an age group vector
        crosstab <- crosstab %>% mutate(`Age Group` = rep(combo$age_group, nrow(crosstab)), Gender = rep(combo$gender, nrow(crosstab)), Hospitalized = rep(combo$hospitalized, nrow(crosstab)), `Intensive Care Unit` = rep(combo$icu, nrow(crosstab)), Death = rep(combo$death, nrow(crosstab)), `Exposure Setting` = rep(combo$exposure, nrow(crosstab)))
        
        # Reorder columns
        crosstab <- crosstab %>% select(`Age Group`, Gender, Hospitalized, `Intensive Care Unit`, Death, `Exposure Setting`, everything())
        
        # Add day column
        crosstab <- crosstab %>% mutate(Day = get_days(unlist(`Episode Date`), day1 = cached$day1))
        
        return(crosstab)
    }
    
    # Get the data from a snapshot
    get_data <- function(snapshot) {
        if(is.null(snapshot)) {
            return()
        } else {
            if(snapshot %in% list.files()) {
                d <- read_xlsx(snapshot)
            } else {
                d <- NULL
            } 
            return(d)
        }
    }
    
    # Compute the day numbers from day 1
    get_days <- function(x, day1) {
        day1 <- day1 - 1
        day <- c()
        for(i in 1:length(x)) {
            day[i] <- as.numeric(x[i] - day1)
        }
        return(day)
    }
    
    # Build age group menu based on the number of available age groups
    output$age_group <- renderUI({
        d <- get_data(input$snapshot)
        cached$d <- d
        cached$day1 <- sort(as.Date(d$`Episode Date`, format = "%d-%b-%y"))[1]
        if(is.null(d)) { 
            return() 
        } else {
            options <- factor(c("All ages", d$`Age Group`))
            options <- relevel(options, ref = "All ages")
            options <- levels(options)
            names(options) <- levels(options)
            cached$age_options <- options
            checkboxGroupInput("age_group", label = "Age groups", choices = options, selected = options[options != "All ages" & options != "Not stated"])
        }
    })
    
    # Build age group menu based on the number of available age groups
    output$age_group2 <- renderUI({
        if(is.null(cached$age_options)) { 
            return() 
        } else {
            selectInput("age_group2", label = "Age groups", choices = cached$age_options)
        }
    })
    
    # Print the number of cases selected
    output$case_count <- renderText({
        if(is.null(cached$d) | is.null(input$age_group) | is.null(input$gender) | is.null(input$hospitalized) | is.null(input$icu) | is.null(input$death) | is.null(input$exposure)) {
            return()
        } else {
            if("All ages" %in% input$age_group) {
                age_selections <- unique(cached$d$`Age Group`)
            } else {
                age_selections <- input$age_group
            }
            if(input$gender == "All genders") {
                gender_selections <- unique(cached$d$Gender)
            } else {
                gender_selections <- input$gender
            }
            if(input$hospitalized == "All conditions") {
                hospital_selections <- unique(cached$d$Hospitalized)
            } else {
                hospital_selections <- input$hospitalized
            }
            if(input$icu == "All conditions") {
                icu_selections <- unique(cached$d$`Intensive Care Unit`)
            } else {
                icu_selections <- input$icu
            }
            if(input$death == "All conditions") {
                death_selections <- unique(cached$d$Death)
            } else {
                death_selections <- input$death
            }
            if(input$exposure == "All conditions") {
                exposure_selections <- unique(cached$d$`Exposure Setting`)
            } else {
                exposure_selections <- input$exposure
            }
            cases <- cached$d %>% filter(`Age Group` %in% age_selections & Gender %in% gender_selections & Hospitalized %in% hospital_selections & `Intensive Care Unit` %in% icu_selections & Death %in% death_selections & `Exposure Setting` %in% exposure_selections)
            prop <- nrow(cases) / nrow(cached$d) * 100
            return(paste0(format(round(prop, 1), nsmall = 1), "% of cases (", format(nrow(cases), big.mark = ","), " out of ", format(nrow(cached$d), big.mark = ","), ") in this data snapshot match the current search criteria."))
        }
    })
    
    # Build age group menu based on the number of available age groups
    output$death <- renderUI({
        d <- cached$d
        if(is.null(d)) { 
            return() 
        } else {
            options <- factor(c("All conditions", d$Death))
            options <- relevel(options, ref = "All conditions")
            options <- levels(options)
            names(options) <- levels(options)
            cached$death_options <- options
            radioButtons("death", label = "Deceased", choices = options)
        }
    })
    
    # Build age group menu based on the number of available age groups
    output$exposure <- renderUI({
        d <- cached$d
        if(is.null(d)) { 
            return() 
        } else {
            options <- factor(c("All conditions", d$`Exposure Setting`))
            options <- relevel(options, ref = "All conditions")
            options <- levels(options)
            names(options) <- levels(options)
            cached$exposure_options <- options
            radioButtons("exposure", label = "Exposure settings", choices = options)
        }
    })
    
    # Build age group menu based on the number of available age groups
    output$gender <- renderUI({
        d <- cached$d
        if(is.null(d)) { 
            return() 
        } else {
            options <- factor(c("All genders", d$Gender))
            options <- relevel(options, ref = "All genders")
            options <- levels(options)
            names(options) <- levels(options)
            cached$gender_options <- options
            radioButtons("gender", label = "Genders", choices = options)
        }
    })
    
    # Render plot data in a searchable/sortable table
    output$get_comparison_table <- renderDT(
        #cached$comparison_data %>% filter(`Age Group` %in% input$age_group2) %>% arrange(desc(Day)) %>% select(`Age Group`, `Episode Date`, Day, Snapshot, everything()),
        cached$comparison_data,
        extensions = c("Buttons", "Scroller"), 
        rownames = FALSE,
        options = list(
            columnDefs = list(list(visible = FALSE, targets = c())),
            pageLength = 50, 
            dom = "Bfrtip", 
            buttons = c("colvis", "copy", "csv", "excel", "pdf"), 
            deferRender = TRUE, 
            searchDelay = 500,
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
                "}"
            )
        )
    )
    
    # Build comparison plot
    output$get_comparison_plot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        if(is.null(input$snapshot2) | length(input$snapshot2) < 2 | is.null(input$age_group2)) {
            return()
        } else {
            d <- tibble()
            for(i in 1:length(input$snapshot2)) {
                d <- rbind(d, get_comparison_data(input$snapshot2[i])$crosstab)
            }
            cached$comparison_data <- d
            point_size <- 0.5
            element_text_size <- 12
            x_label <- ifelse(input$x_axis2 == "Day", "Day number (since first case)", "Date")
            ggplotly(ggplot(d %>% filter(`Age Group` %in% input$age_group2), aes(x = !!rlang::sym(input$x_axis2), y = `Cumulative Incidence`)) +
                geom_line(aes(color = Snapshot), size = point_size) +
                xlab(x_label) +
                ylab("Cumulative incidence") +
                scale_y_continuous(labels = comma) +
                theme_minimal() +
                theme(
                    plot.title = element_text(size = element_text_size),
                    axis.title.x = element_text(size = element_text_size),
                    axis.title.y = element_text(size = element_text_size),
                    legend.text = element_text(size = element_text_size),
                    legend.title = element_blank()
                ))
        }
    })
    
    # Build cumulative incidence plot
    output$get_cumulative_incidence_plot <- renderPlotly({
        # generate bins based on input$bins from ui.R
        combos <- expand.grid(list(age_group = input$age_group, gender = input$gender, hospitalized = input$hospitalized, icu = input$icu, death = input$death, exposure = input$exposure), KEEP.OUT.ATTRS = FALSE)
        crosstab <- data.frame()
        for(row in 1:nrow(combos)) {
            crosstab <- rbind(crosstab, get_crosstab(combos[row,]))
        }
        if(is.null(crosstab) | is.null(input$age_group) | is.null(input$gender)) {
            return()
        } else {
            cached$crosstab <- crosstab
            point_size <- 0.5
            element_text_size <- 12
            x_label <- ifelse(input$x_axis == "Day", "Day number (since first case)", "Date")
            ggplotly(ggplot(crosstab, aes(x = !!rlang::sym(input$x_axis), y = `Cumulative Incidence`)) +
            geom_line(aes(color = `Age Group`), size = point_size) +
            xlab(x_label) +
            ylab("Cumulative incidence") +
            scale_y_continuous(labels = comma) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = element_text_size),
                axis.title.x = element_text(size = element_text_size),
                axis.title.y = element_text(size = element_text_size),
                legend.text = element_text(size = element_text_size),
                legend.title = element_blank()
            ))
        }
    })
    
    # Render plot data in a searchable/sortable table
    output$get_data_source <- renderDT(
        get_data(input$snapshot3),
        extensions = c("Buttons", "Scroller"), 
        rownames = FALSE,
        options = list(
            columnDefs = list(list(visible = FALSE, targets = c())),
            pageLength = 50, 
            dom = "Bfrtip", 
            buttons = c("colvis", "copy", "csv", "excel", "pdf"), 
            deferRender = TRUE, 
            searchDelay = 500,
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
                "}"
            )
        )
    )
    
    # Render plot data in a searchable/sortable table
    output$get_plot_table <- renderDT(
        cached$crosstab %>% filter(`Age Group` %in% input$age_group & Gender %in% input$gender & Hospitalized %in% input$hospitalized) %>% arrange(desc(Day)) %>% select(`Episode Date`, Day, Incidence, `Cumulative Incidence`, `Age Group`, Gender, everything()),
        extensions = c("Buttons", "Scroller"), 
        rownames = FALSE,
        options = list(
            columnDefs = list(list(visible = FALSE, targets = c())),
            pageLength = 10, 
            dom = "Bfrtip", 
            buttons = c("colvis", "copy", "csv", "excel", "pdf"), 
            deferRender = TRUE, 
            searchDelay = 500,
            initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
                "}"
            )
        )
    )
    
    # Build age group menu based on the number of available age groups
    output$hospitalized <- renderUI({
        d <- cached$d
        if(is.null(d)) { 
            return() 
        } else {
            options <- factor(c("All conditions", d$Hospitalized))
            options <- relevel(options, ref = "All conditions")
            options <- levels(options)
            names(options) <- levels(options)
            cached$hospital_options <- options
            radioButtons("hospitalized", label = "Hospitalized", choices = options)
        }
    })
    
    # Build age group menu based on the number of available age groups
    output$icu <- renderUI({
        d <- cached$d
        if(is.null(d)) { 
            return() 
        } else {
            options <- factor(c("All conditions", d$`Intensive Care Unit`))
            options <- relevel(options, ref = "All conditions")
            options <- levels(options)
            names(options) <- levels(options)
            cached$icu_options <- options
            radioButtons("icu", label = "In intensive care", choices = options)
        }
    })
    
    # Build data snapshot menu based on the number of snapshots available
    output$snapshot <- renderUI({
        cached$files <- list.files(pattern = "^[^~|+]*.xlsx")
        if (is.null(cached$files)) {
            return()
        } else {
            # Get a list of data files that currently exist
            files <- sort(cached$files, decreasing = TRUE)
            
            # Check to see if new snapshot needs to be created
            now <- now(tzone = "UTC") - hours(4)
            if (! paste0("Table 13-10-0766-01 - updated ", format(now, "%Y-%m-%d"), ".xlsx") %in% list.files() & ! paste0("+Table 13-10-0766-01 - updated ", format(now, "%Y-%m-%d"), ".xlsx") %in% list.files() & as.integer(format(now, "%H")) >= 9) {
                # Import a new snapshot
                new_snapshot <- get_cansim("13-10-0766-01", refresh = TRUE)
                
                # Convert it to wide format
                new_snapshot <- wrangle_data(new_snapshot)
                
                # Compare it to the last snapshot
                last_snapshot <- read_xlsx(files[1])
                compare <- all.equal(new_snapshot, last_snapshot)
                
                # If it's different from the last snapshot, store it and rebuild the snapshot menu
                if(isTRUE(compare) | nrow(new_snapshot) == nrow(last_snapshot)) {
                    write.xlsx2(as.data.frame(new_snapshot), paste0("+Table 13-10-0766-01 - updated ", format(now, "%Y-%m-%d"), ".xlsx"), row.names = FALSE, showNA = FALSE)
                } else {
                    write.xlsx2(as.data.frame(new_snapshot), paste0("Table 13-10-0766-01 - updated ", format(now, "%Y-%m-%d"), ".xlsx"), row.names = FALSE, showNA = FALSE)
                    cached$files <- list.files(pattern = "^[^~]*.xlsx")
                    files <- sort(cached$files, decreasing = TRUE)
                }
            }
            
            # Isolate the date portion of the file name(s) to use in the drop down menu
            file_names <- unname(sapply(files, function(x) strsplit(x, "updated ")[[1]][2]))
            file_names <- gsub(".xlsx", "", file_names)
            
            # Reformt the date
            names(files) <- format(as.Date(file_names), "%B %d, %Y")
            cached$files2 <- files
            
            selectInput("snapshot", "Data snapshots", choices = files)
        }
    })
    
    # Build data snapshot menu based on the number of snapshots available
    output$snapshot2 <- renderUI({
        if(is.null(cached$files)) { 
            return() 
        } else {
            checkboxGroupInput("snapshot2", label = "Data snapshots", choices = cached$files2, selected = c(input$snapshot))
        }
    })
    
    # Build data snapshot menu based on the number of snapshots available
    output$snapshot3 <- renderUI({
        cached$files <- list.files(pattern = "*.xlsx")
        if (is.null(cached$files)) {
            return()
        } else {
            selectInput("snapshot3", "Data snapshots", choices = cached$files2)
        }
    })
    
    # Build x-axis menu
    output$x_axis <- renderUI({
        d <- cached$d
        if(is.null(d)) { 
            return() 
        } else {
            options <- c("Episode date" = "Episode Date", "Day number since first case" = "Day")
            cached$x_axis_options <- options
            radioButtons("x_axis", label = "Horizontal axis", choices = options)
        }
    })
    
    # Build x-axis menu
    output$x_axis2 <- renderUI({
        d <- cached$d
        if(is.null(d)) { 
            return() 
        } else {
            options <- c("Episode date" = "Episode Date", "Day number since first case" = "Day")
            cached$x_axis_options <- options
            radioButtons("x_axis2", label = "Horizontal axis", choices = options)
        }
    })
    
    # Wrangle the raw data
    wrangle_data <- function(d) {
        # Reshape data from long to wide format
        d_wide <- spread(d %>% select("Case identifier number", "Case information", VALUE, REF_DATE), "Case information", VALUE)
        
        # Add leading zeros to case identifier number
        d_wide$`Case identifier number` <- str_pad(d_wide$`Case identifier number`, width = nchar(max(as.numeric(d$`Case identifier number`))), pad = "0")
        
        # Identify select vectors
        vectors_to_factor <- c("Age group", "Gender", "Transmission", "Hospitalization", "Intensive care unit", "Death")
        
        # Restructure as factors
        d_wide[vectors_to_factor] <- lapply(d_wide[vectors_to_factor], factor)
        
        # Add semantic labels
        d_wide$`Age group` <- revalue(d_wide$`Age group`, c("1" = "0-19", "2" = "20-29", "3" = "30-39", "4" = "40-49", "5" = "50-59", "6" = "60-69", "7" = "70-79", "8" = "80+", "99" = "Not stated"))
        d_wide$Gender <- revalue(d_wide$Gender, c("1" = "Male", "2" = "Female", "3" = "Non-binary", "7" = "Non-binary", "9" = "Not stated"))
        d_wide$Transmission <- revalue(d_wide$Transmission, c("1" = "Travel exposure", "2" = "Community exposure", "3" = "Pending"))
        d_wide$Hospitalization <- revalue(d_wide$Hospitalization, c("1" = "Yes", "2" = "No", "9" = "Not stated"))
        d_wide$`Intensive care unit` <- revalue(d_wide$`Intensive care unit`, c("1" = "Yes", "2" = "No", "9" = "Not stated"))
        d_wide$Death <- revalue(d_wide$Death, c("1" = "Yes", "2" = "No", "9" = "Not stated"))
        
        # Add day, month and reference year vectors together and structure as a date object
        d_wide$`Episode date` <- as.Date(paste0(d_wide$REF_DATE, "-", str_pad(d_wide$`Episode date - month`, 2, pad = "0"), "-", str_pad(d_wide$`Episode date - day`, 2, pad = "0")), format = "%Y-%m-%d")
        
        # Change format to %d-%b-%y
        d_wide$`Episode date` <- format(d_wide$`Episode date`, format = "%d-%b-%y")
        
        # Remove unwanted vectors from data
        d_wide <- d_wide %>% select("Case identifier number", "Episode date", Gender, "Age group", Transmission, Hospitalization, "Intensive care unit", Death)
        
        # Rename vectors
        names(d_wide) <- c("CaseID", "Episode Date", "Gender", "Age Group", "Exposure Setting", "Hospitalized", "Intensive Care Unit", "Death")
        
        # Order data by case ids in ascending order
        d_wide <- d_wide %>% arrange(CaseID)
        
        # Export data to Excel
        #write.xlsx2(as.data.frame(d_wide), paste0("Table 13-10-0766-01 - updated ", format(Sys.time(), "%Y-%m-%d"), ".xlsx"), row.names = FALSE, showNA = FALSE)
        
        return(d_wide)
    }
}

# Run the application
shinyApp(ui = ui, server = server)