
#shiny::runApp("//scan01/Users/yanzh/IJE/2020/code/shiny", host = "0.0.0.0", port=1300)

library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(DT)

ui<-fluidPage(
  shinyjs::useShinyjs(),
  includeCSS('www/style.css'),
  
  tags$h1("Inter-Jurisdictional Employment (IJE) Dashboard",align="center"),
  tabsetPanel(
 
  tabPanel("Using This App",
           sidebarLayout(
             sidebarPanel(
               h4(HTML("<u>Definitions</u>")),
               p(HTML("<ul>
                        <li><b>Jurisdiction</b> refers to a Canadian province or territory</li>
                        <li><b>Inter-Jurisdictional Employees (IJE)</b> report T4 earnings in a different province or territory than their jurisdiction of residence, as reported on their T1</li>
                        <li><b>Incoming IJE</b> for a given jurisdiction report T4 earnings in that province or territory, but report a different one on their T1 as their jurisdiction of residence</li>
                        <li><b>Outgoing IJE</b> for a given jurisdiction report that province or territory on their T1 as their jurisdiction of residence, but report T4 earnings elsewhere</li>
                        <li><b>Resident employment</b> for a given jurisdiction refers to employees who only reports T4 earnings in their province or territory of residence as reported on their T1</li>
                       </ul>
                       As income can be derived from multiple sources, income can be split accordingly:
                       <ul>
                        <li><b>Income Inside the Jurisdiction</b> for a selected jurisdiction refers to T4 income earned within that jurisdiciton for both incoming and outgoing employees</li>
                        <li><b>Income Outside the Jurisdiction</b> for a selected jurisdiction refers to T4 income earned outside that jurisdiction for both incoming and outgoing employees</li>
                       </ul>"))
                      #  On the National and Jurisdiction tabs, data can be selected from one of two files:
                      # <ul>
                      # <li><b>T1 Personal Master File:</b> includes regular tax filers for the fiscal year</li>
                      # <li><b>T1 Historical File:</b> includes regular tax filers for the fiscal year, as well as late and re-assessed tax filers</li>
                      # </ul>"))
             ),
             
             mainPanel(
               fluidRow(column(8,
                               h4(HTML('<u>About</u>')),
                               p(HTML("<b>Inter-jurisdictional employees (IJE)</b> are employees who maintain a permanent residence in a province or territory, while reporting 
                    earnings/income from a different province or territory. Data on IJE are important for federal and provincial governments for a wide range 
                    of purposes related to tax policy, employment, funding for public programs, and more.")),
                               p(HTML("For any given province or territory, inter-jurisdictional labour movements have two directions: <b>incoming</b> and <b>outgoing</b>. For 
                    example, 'Incoming IJE' for the province of Alberta would refer to individuals who received T4 earnings in Alberta, but who report a 
                    different province or territory of residence on their T1 tax returns. 'Outgoing IJE' for the province of Alberta would refer to those who listed Alberta 
                    as their province of residence on their T1 tax return, but who made earnings in another province or territory in the same tax year.")),
                               h4(HTML('<u>How to Use This Application</u>')),
                               p(HTML("The IJE Dashboard contains five separate tabs from which users can produce interactive data visualizations of inter-jurisdictional employment 
                    flows. These tabs can be found at the top of the page, and display information on national level trends; jurisdictional inflows and outflows of  
                    IJE; and for each province and territory, breakdowns of inflows and outflows by province pair, industry, and age group.")),
                               # p(HTML('The IJE Dashboard contains five separate tabs from which users can produce interactive data visualizations of inter-jurisdictional employment 
                               #        flows. These tabs can be found at the top of the page, and display information on:<br>
                               #        <ul>
                               #        <li><b>National Trends:</b> A choropleth map of Canada, where provinces and territories are coloured according to the total number of IJE 
                               #        or their total combined income.</li>
                               #        <li><b>Jurisdiction:</b> Line plots over time of the total number of incoming and outgoing IJEs and their income for any province or territory.</li>
                               #        <li><b>Industry:</b> Line plots over time of the total number of IJEs and their income by industry of employment for any province or territory.</li>
                               #        <li><b>Target Jurisdiction:</b> Line plots over time of the total number of IJEs and their income by province pair for any province or territory.</li>
                               #        <li><b>Age Group:</b> Line plots over time of the total number of IJEs by their age group, and the percent change in each group over the specified 
                               #        time period.</li>
                               #        </ul>')),
                               p(HTML("Data selections can be customized in the side bar on any page. Clicking the <b>Refresh Plot</b> button renders new graphs on the page. All plots in the 
                    dashboard are fully interactive, with additional information displayed by hovering over any data point. Navigation tools for panning/zooming in on visuals can be found in 
                    the upper right corner of each plot. The data used to generate plots on each tab can be downloaded by clicking <b>Download</b> in the sidebar on the same page.")),
                               # p(HTML("Data selections can be customized in the side bar on any page. To generate the graphs corresponding with your selections, click the 
                               #        <b>Refresh Plot</b> button towards the bottom of the sidebar. All plots in the dashboard are fully interactive, and contain detailed information 
                               #        on your selections and the values corresponding to each point. Navigation tools are available in the upper right hand corner of each plot as well, 
                               #        and allow for panning and zooming. The data used in each plot can also be directly downloaded from each page by selecting the table of choice in 
                               #        the dropdown menu and clicking <b>Download</b>.")),
                               h4(HTML('<u>IJE Guide & Release Files</u>')),
                               p(HTML("IJE data developed by Social Analysis and Modelling Division (SAMD) are based on the Canadian Employer-Employee Dynamics Database (CEEDD) at Statistics 
                    Canada. Starting in 2021, SAMD has added this dashboard as a new dissemination and data exploration tool as part of its regular release of  
                    IJE data to the public and to stakeholders.")),
                               br(),
                               fluidRow(column(2,img(src='pdf-icon-png-2058.png')),
                                        column(10,p("Users who would like to know more about the IJE data used in this dashboard can consult the IJE User Guide, which can be downloaded at 
                                                   the link provided below."),
                                               p(HTML('<b>Methodological guide: </b>'), downloadLink('downloadGuide','Methodological Guide on IJEs-ENGLISH-Dec.2017.pdf')))),
                               br(),
                               fluidRow(column(2,img(src='file-zip-icon-6811.png')),
                                        column(10,p("For users who would like to download the entire IJE release, the data in its original format can be found on the left. This .zip file 
                                                   contains Excel tables formatted in the same way as files sent out to clients in releases prior to 2021."),
                                               p(HTML('<b>Excel format data: </b>'), downloadLink('downloadVintage','English_Version.zip')))),
                               br()
                    #            p(HTML("For users who would like to know more about the IJE data produced by SAMD, or for users who prefer the original Excel data format for IJE data, the 
                    # IJE User Guide and the current release of IJE in its original format can be found below:")),
                    #            br(),
                    #            fluidRow(column(6,
                    #                            fluidRow(column(4,img(src='pdf-icon-png-2058.png')),
                    #                                     column(8,p(downloadLink('downloadGuide','Click here'), "to download the IJE User Guide.")))),
                    #                     column(6,
                    #                            fluidRow(column(4,img(src='file-zip-icon-6811.png')),
                    #                                     column(8,p(downloadLink('downloadVintage', 'Click here'), "to download IJE data in its vintage format.")))))
                    #            ),
                    #     
                    #     column(4,''))

           ))))),
    
  tabPanel("National", 
           sidebarLayout(
             sidebarPanel(
               selectInput("YearInput", "Select Year",
                           choices = c(beginy:endy),
                           selected =c(endy)),
               
               radioButtons("SeriesInput", "Select Series",
                            choices = c("Employee Count","Employee Income"),
                            selected=c("Employee Count")),
               
               radioButtons("TypeInput", "Select Employment Type",
                            choices = c("Incoming","Outgoing","Resident"),
                            selected =c("Incoming")),   
               
               radioButtons("GenderInput", "Select Gender",
                         choices = c("All","Male","Female"),
                         selected =c("All")),
               
               radioButtons("IncomeSource", tags$div(HTML(paste0("Select Income Source",tags$sup('1')))),
                           choices = c("Inside the Jurisdiction","Outside the Jurisdiction"),
                           selected =c("Inside the Jurisdiction")),
               
               radioButtons("SourceInput", tags$div(HTML(paste0("Select Data Source",tags$sup('2')))),
                            choices = c("T1 Personal Master File", "T1 Historical File"),
                            selected = c("T1 Personal Master File")), 
               
               actionButton('NationalGen',HTML('<b>Refresh Plot</b>'),width='100%'),
               hr(),

               selectInput("tableN", "Choose a table to download: ",
                           choices = c("Filtered National Table","Full National Table")),
               downloadButton("downloadNtable", "Download") 
       
             ),
             mainPanel(
               
               ## render object occurs here
               # leafletOutput("PRcount",height=600),
               plotlyOutput("mainmap",height=600
                          # ,click='map_click'
                          ),
               
               # br(),
               hr(),
               # plotOutput("PRbar"),
               # br(),
               helpText("1. IJEs can make income inside/outside their jurisdiction from different income sources. Selection of income source inside/outside
                        the jurisdiction has no effect on data maps when ‘Employees’ is selected as the data series. When 'Resident' employment type is 
                        selected, income source defaults to 'Inside the Jurisdiction', as resident employees by definition do not earn income outside the 
                        jurisdiction."),
               
               helpText("2. Data from T1 Personal Master File are derived from the T1 and T4 tax files. Data from T1 Historical File are derived from 
                        the T1, T4 and the T1 historical personal master file. The T1 historical file includes late and re-assessed tax filers. 
                        Because the T1 historical file is only available up to 2015, the last two years is forecasted using a 5-year average from both 
                        the T4-T1 and the T4-T1-T1 historical file series."),
               # helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
               #          of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
               #          received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
               #          which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
               #          true when 'Outgoing' is selected."),
               helpText("3. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("4. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may also be 
                        suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to 
                        the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
             )
             
             
           )
           
       ),
  
  
  tabPanel("Jurisdiction",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProvinceInput", "Select Jurisdiction",
                           choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"),
                           
                           selected =c("Newfoundland and Labrador")),
               
               sliderInput("YRInput", "Year Range", 
                           beginy, endy, c(beginy, endy),
                           sep=''),
               
               radioButtons("GDInput", "Select Gender",
                           choices = c("All","Male","Female"),
                           selected =c("All")),
               
               radioButtons("DSInput", tags$div(HTML(paste0("Select Data Source",tags$sup('1')))),
                            choices = c("T1 Personal Master File", "T1 Historical File"),
                            selected = c("T1 Personal Master File")),
               
               actionButton('JurisdGen',HTML('<b>Refresh Plot</b>'),width='100%'),
               hr(),
               
               selectInput("tableIn", "Choose a table to download: ",
                           choices = c("Filtered Table","Full Table")
                           ),
               downloadButton("downloadPtable", "Download")       
             ),
             mainPanel(
               plotlyOutput("PRtrend",height=525),
               br(),
               # plotlyOutput("PRtrend2",height=450),
               # br(),
               plotlyOutput("PRInctrend",height=525),
               # br(),
               hr(),
               ## what does this do??
               
               ### added a line of code here too `.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover `###
             #   tags$style(HTML("
             #        .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
             #        color: #ffffff;
             #        }
             #        ### ADD THIS HERE ###
             #        .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
             # 
             #        ###To change text and background color of the `Select` box ###
             #        .dataTables_length select {
             #               color: #0E334A;
             #               background-color: #0E334A
             #               }
             # 
             #        ###To change text and background color of the `Search` box ###
             #        .dataTables_filter input {
             #                color: #0E334A;
             #                background-color: #0E334A
             #               }
             # 
             #        thead {
             #        color: #ffffff;
             #        }
             # 
             #         tbody {
             #        color: #000000;
             #        }
             # 
             #       "
             # 
             #   )
             # ),
             # 
             #   ## what does this do?
             # DT::dataTableOutput("PRtable"),
             
             #br(),
             helpText("1. Data from T1 Personal Master File are derived from the T1 and T4 tax files. Data from T1 Historical File are derived from
                      the T1, T4 and the T1 historical personal master file. The T1 historical file includes late and re-assessed tax filers. Because
                      the T1 historical file is only available up to 2015, the last two years is forecasted using a 5-year average from both the T4-T1 
                      and the T4-T1-T1 historical file series."),
             helpText("2. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
             
             # helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
             #            of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
             #            received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
             #            which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
             #            true when 'Outgoing' is selected."),
             helpText("3. Incoming IJE's earnings are defined as their T4 earnings received from the selected jurisdiction. Outgoing IJE's earnings are
                      defined as their T4 earnings received from non-residential jurisdictions (i.e., outside the selected jurisdiction)."),
             helpText("4. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may also be suppressed 
                      if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
             helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
             
             
             )
           )
      ),
  
  tabPanel("Target Jurisdiction",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProOPInput", "Select Base Jurisdiction",
                           choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"),
                           selected =c("Newfoundland and Labrador")),
               
               sliderInput("YRTP", "Year Range", 
                           beginy, endy, c(beginy, endy),
                           sep=''),
               
               radioButtons('IncOutTgtJuris',"Select Employment Type",
                            choices = c('Incoming','Outgoing'),
                            selected = c('Incoming')),
               
               checkboxGroupInput('ProTPInput',tags$div(HTML(paste0("Select Target Jurisdiction",tags$sup('1')))),
                                  choices = provList,
                                  selected = provList),
               
               checkboxInput("selectAllProv","Select/Deselect All", value=T),
               
               actionButton('TgtJurisdGen',HTML('<b>Refresh Plot</b>'),width='100%'),
               hr(),
               
               selectInput("tableTP", "Choose a table to download: ",
                           choices = c("Filtered TP Table","Full TP Table")),
               
               downloadButton("downloadTPtable", "Download") 
               
             ),
             mainPanel(
               plotlyOutput("TPcount",height=525),
               br(),
               plotlyOutput("TPincome",height=525),
               # br(),
               hr(),
               helpText("1. When an outgoing IJE has T4 earnings in more than one jurisdiction, they will be counted in the jurisdiction of the job in which 
                        the employee has the highest T4 earnings. When 'Incoming' IJEs is selected, the target jurisdiction is the person's jurisdiction of residence. When 
                        'Outgoing' IJEs is selected, the target jurisdiction refers to the persons province of employment."),
               helpText("2. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("3. When 'Incoming' IJEs are selected, incomes reflect the income made inside the selected jurisdiction. When 'Outgoing' IJEs are selected, income reflects the 
                        income made outside the selected jurisdiction for residents of the selected jurisdiction."),
               helpText("4. These estimates are derived from the T1 and T4 tax files and do not include late and re-assessed 
                        taxfilers from the T1 Historical personal master file."),
               # helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
               #          of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
               #          received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
               #          which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
               #          true when 'Outgoing' is selected."),
               helpText("5. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may 
                        also be suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings 
                        are rounded to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           
  ),
  
  tabPanel("Industry",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProIndInput", "Select Jurisdiction",
                           choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"),
                           selected =c("Newfoundland and Labrador")),
               
               sliderInput("YRInd", "Year Range", 
                           beginy, endy, c(beginy, endy),
                           sep=''),
               
               radioButtons('IncOutIndustry',"Select Employment Type",
                            choices = c('Incoming','Outgoing'),
                            selected = c('Incoming')),
               
               checkboxGroupInput("IndustryInput", tags$div(HTML(paste0("Select Industry",tags$sup('1,2')))),
                                  choices = indList,
                                  selected = indList[1:12]),
               
               checkboxInput("selectAllInd","Select/Deselect All"),
               
               actionButton('IndustryGen',HTML('<b>Refresh Plot</b>'),width='100%'),
               hr(),
               
               selectInput("tableInd", "Choose a table to download: ",
                           choices = c("Filtered Industry Table","Full Industry Table")),
               
               downloadButton("downloadItable", "Download")   
             ),
             mainPanel(
               plotlyOutput("IndCount",height=600),
               br(),
               plotlyOutput("IndIncome",height=600),
               # br(),
               hr(),
               ### add your style inline css values here
               
               ### added a line of code here too `.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover `###
               # tags$style(HTML("
               #                 .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
               #                 color: #ffffff;
               #                 }
               #                 ### ADD THIS HERE ###
               #                 .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
               #                 
               #                 ###To change text and background color of the `Select` box ###
               #                 .dataTables_length select {
               #                 color: #0E334A;
               #                 background-color: #0E334A
               #                 }
               #                 
               #                 ###To change text and background color of the `Search` box ###
               #                 .dataTables_filter input {
               #                 color: #0E334A;
               #                 background-color: #0E334A
               #                 }
               #                 
               #                 thead {
               #                 color: #ffffff;
               #                 }
               #                 
               #                 tbody {
               #                 color: #000000;
               #                 }
               #                 
               #                 "
               #                 
               # )
               # ),
               # 
               # 
               # DT::dataTableOutput("Indtable"),
               
               #br(),
               helpText("1. When an employee has T4 earnings in more than one industry, they will be counted in the industry of the job in which the 
                        employee has the highest T4 earnings."),
               helpText("2. Industry categories are derived from the North American Industry Classification System (NAICS). 'IFRM' includes: Information and cultural industries; 
                        Finance and insurance; Real estate and rental and leasing; Management of companies and enterprises. ‘Other services’ include: Administrative and support; 
                        Waste management and remediation services; Entertainment and recreation; Other services etc."),
               helpText("3. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("4. Estimates on this tab are derived from the T1 and T4 tax files and do not include late and re-assessed taxfilers from the T1 Historical personal master file. 
                        When 'Incoming' IJEs are selected, incomes reflect the income made inside the selected jurisdiction. When 'Outgoing' IJEs are selected, income reflects the 
                        income made outside the selected jurisdiction for residents of the selected jurisdiction."),
               # helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
               #          of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
               #          received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
               #          which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
               #          true when 'Outgoing' is selected."),
               
               helpText("5. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may also be 
                        suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded 
                        to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           ),
           
  tabPanel("Age Group",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProAgeInput", "Select Jurisdiction",
                           choices = c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"),
                           selected =c("Newfoundland and Labrador")),
               
               sliderInput("YRAge", "Year Range",
                           beginy, endy, c(beginy, endy),
                           sep=''),
               
               radioButtons("TAgeInput", "Select Employment Type",
                            choices = c("Incoming","Outgoing"),
                            selected =c("Incoming")),
               
               checkboxGroupInput( "AgeInput", "Select Age Groups",
                           choices = ageList,
                           selected = ageList),
               
               checkboxInput('selectAllAge','Select/Deselect All',value=T),
               
               radioButtons("GDAgeInput", "Select Gender",
                           choices = c("All","Male","Female"),
                           selected =c("All")),
               
               actionButton('AgeGen',HTML('<b>Refresh Plot</b>'),width='100%'),
               hr(),
               
               selectInput("tableAge", "Choose a table to download: ",
                           choices = c("Filtered Age Table","Full Age Table")),
               
               downloadButton("downloadAtable", "Download")  
               
             ),
             mainPanel(
               plotlyOutput("Agetrend",height=525),
               # br(),
               # plotlyOutput("Agechange",height=525),
               # br(),
               hr(),
               
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. These estimates are derived from the T1 and T4 tax files and do not include late and re-assessed 
                        taxfilers from the T1 Historical personal master file."),
               # helpText("3. Incoming IJE are individuals who received T4 earnings from the selected jurisdiction but reported a different jurisdiction 
               #          of residence on their T1 tax returns. Outgoing IJE are individuals who identified as residing in the selected jurisdiction but 
               #          received T4 earnings from other jurisdictions. When 'Incoming' is selected, the base jurisdiction is the province or territory in 
               #          which they work and the target jurisdiction is their jurisdiction of residence (where they file T1 Genral), with the opposite being 
               #          true when 'Outgoing' is selected."),
               helpText("3. Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts may also be suppressed 
                        if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           
          )#,
  
  # tabPanel("Reference",
  #          navlistPanel(
  #            tabPanel("Inter-jurisdictional Employees",
  #                     tags$head(
  #                       tags$style("label{font-family: 'Lobster', cursive;}")), 
  #                     h3( "Inter-jurisdictional employees are paid employment of individuals who maintain a permanent residence in a given province/territory while reporting
  #                         earnings from a different province/territory"))
  #            
  #                    ),
  #          navlistPanel(
  #            tabPanel("Data Source(s)",
  #                     tags$head(
  #                       tags$style("label{font-family: 'Lobster', cursive;}")), 
  #                     h3( "The data source for this project is the Canadian Employer-Employee Dynamics Database
  #                       (CEEDD)"))
  #          ),
  #          
  #          navlistPanel(
  #            tabPanel("User Guide",
  #                     tags$head(
  #                       tags$style("label{font-family: 'Lobster', cursive;}")),
  #                     h3("Click", downloadLink('downloadGuide','Here'), "to Download user Guide"
  #                       ))
  #          ),
  #          navlistPanel(
  #            tabPanel("Datasets",
  #                     tags$head(
  #                       tags$style("label{font-family: 'Lobster', cursive;}")),
  #                     h3("Datasets are avaiable for download from", downloadLink('downloadVintage','Here')
  #                     ))
  # 
  #          )
  #          
  # )
  

  ) 
)
