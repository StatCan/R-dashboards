
#shiny::runApp("//scan01/Users/yanzh/IJE/2020/code/shiny", host = "0.0.0.0", port=1300)

library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(DT)
ui<-fluidPage(
  theme=shinytheme("darkly"),
  tags$head(tags$style(HTML('* {font-family: "Arial"} !important'))),
  
  tags$h1("Inter-Jurisdictional Employment (IJE) Dashboard",align="center"),
  tabsetPanel(
 
  
  tabPanel("National", 
           sidebarLayout(
             sidebarPanel( 
               selectInput("YearInput", "Select Year",
                           choices = c(beginy:endy),
                           selected =c(endy)),  
               radioButtons("SourceInput", "Data Source",
                            choices = c("T1 Personal Master File", "T1 Historical File"),
                            selected = c("T1 Personal Master File")),
               
               selectInput("GenderInput", "Select Gender",
                         choices = c("Male","Female","Both"
                         ),
                         selected =c("Both")),
             
               selectInput("TypeInput", "Select Employment Type",
                         choices = c("Receiver","Sender"
                         ),
                         selected =c("Receiver")),   
               selectInput("IncomeSource", "Select Income Source",
                           choices = c("Inside the Jurisdiction","Outside the Jurisdiction"
                           ),
                           selected =c("Inside the Jurisdiction")),
               selectInput("tableN", "Choose a table to download: ",
                           choices = c("Filtered National Table","Full National Table")
               ),
               downloadButton("downloadNtable", "Download") 
       
             ),
             mainPanel(
               leafletOutput("PRcount"),
               br(),
               plotOutput("PRbar"),
               br(),
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. Data from T1 Personal Master File are derived from the T1 and T4 tax fiels."),
               helpText("3. Data from T1 Historical File are derived from the T1, T4 and the T1 historical personal master file. 
                       The T1 historical file includes late and re-assessed taxfilers. Because the T1 historical file is only available up to 2014, 
                        the last two years is forecasted using a 5-year average from both the T4-T1 and the T4-T1-T1 historical file series."),
               helpText("4. Receivers are individuals who received T4 earnings from the selected jurisdiction but reported a different
                        jurisdiction of residence on their T1 tax returns."),
               helpText("5. Senders are individuals who identified as residing in the selected jurisdiction but received T4 earnings from
                        other jurisdictions."),
               helpText("Notes: Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts 
                        may also be suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
             )
             
             
           )
           
       ),
  
  
  # tabPanel("Overall Comparison",
  #          sidebarLayout(
  #            sidebarPanel(
  #              selectInput("CompInput1", "Select Province",
  #                          choices = c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and labrador",
  #                                      "Northwest Territories","Nova Scotia","Nunavut","Ontario","Prince Edward Island",
  #                                      "Quebec","Saskatchewan","Yukon"
  #                          ),
  #                          selected =c("Alberta")
  #              ),
  #              selectInput("CompInput2", "Compare With",
  #                          choices = c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and labrador",
  #                                      "Northwest Territories","Nova Scotia","Nunavut","Ontario","Prince Edward Island",
  #                                      "Quebec","Saskatchewan","Yukon"
  #                          ),
  #                          selected =c("British Columbia")
  #              ), 
  #              
  #              sliderInput("YRComp", "Year Range", beginy, endy, c(beginy, endy)
  #              ),
  #              
  #              selectInput("GDComp", "Select Gender",
  #                          choices = c("Male","Female","Both"
  #                          ),
  #                          selected =c("Both")
  #              ),
  #              selectInput("TypeComp", "Select Employment Type",
  #                          choices = c("Receiver","Sender","Resident"
  #                          ),
  #                          selected =c("Receiver")
  #              ), 
  #              
  #              radioButtons("DSComp", "Data Source",
  #                           choices = c("T1 Personal Master File", "T1 Historical File"),
  #                           selected = c("T1 Personal Master File")
  #              ),
  #              selectInput("InComp", "Select Income Source",
  #                          choices = c("Inside","Outside"
  #                          ),
  #                          selected =c("Inside")
  #                          )       
  #              
  #            ),
  #            mainPanel(
  #              fluidRow(
  #                splitLayout(cellWidths=c("50%","50%"), plotOutput("PRcomp"), plotOutput("PRcomp_inc"))
  #              ),            
  #              br(),
  #              fluidRow(
  #                splitLayout(cellWidths=c("50%","50%"), dataTableOutput("CompT1"), dataTableOutput("CompT2"))
  #              )              
  #            )
  #          )
  # ),
  
  
  tabPanel("Jurisdiction",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProvinceInput", "Select Jurisdiction",
                           choices = c("Newfoundland and labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"
                             
                           ),
                             
                           #   c("Alberta","British Columbia","Manitoba","New Brunsick","Newfoundland and labrador",
                           #             "Northwest Territories","Nova Scotia","Nunavut","Ontario","Prince Edward Island",
                           #             "Quebec","Saskatchewan","Yukon"
                           # ),
                           
                           selected =c("Newfoundland and labrador")
                           ),
               sliderInput("YRInput", "Year Range", beginy, endy, c(beginy, endy)
                           ),
               selectInput("GDInput", "Select Gender",
                           choices = c("Male","Female","Both"
                           ),
                           selected =c("Both")
                           ),
               radioButtons("DSInput", "Data Source",
                            choices = c("T1 Personal Master File", "T1 Historical File"),
                            selected = c("T1 Personal Master File")
                            ),
               selectInput("tableIn", "Choose a table to download: ",
                           choices = c("Filtered Table","Full Table")
                           ),
               downloadButton("downloadPtable", "Download")       
             ),
             mainPanel(
               fluidRow(
                 splitLayout(cellWidths=c("50%","50%"),  plotOutput("PRtrend"),  plotOutput("PRInctrend"))
               ),
               br(),
               fluidRow(
                 splitLayout(cellWidths=c("50%","50%"),  plotOutput("Income_receiver"),  plotOutput("Income_sender"))
               ),
               br(),
               ### add your style inline css values here
               
               ### added a line of code here too `.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover `###
               tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: #ffffff;
                    }
                    ### ADD THIS HERE ###
                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}

                    ###To change text and background color of the `Select` box ###
                    .dataTables_length select {
                           color: #0E334A;
                           background-color: #0E334A
                           }

                    ###To change text and background color of the `Search` box ###
                    .dataTables_filter input {
                            color: #0E334A;
                            background-color: #0E334A
                           }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                    }

                   "
                               
               )
             ),
             
               
               DT::dataTableOutput("PRtable"),
             
             #br(),
             helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
             helpText("2. Data from T1 Personal Master File are derived from the T1 and T4 tax fiels."),
             helpText("3. Data from T1 Historical File are derived from the T1, T4 and the T1 historical personal master file. 
                       The T1 historical file includes late and re-assessed taxfilers. Because the T1 historical file is only available up to 2014, 
                        the last two years is forecasted using a 5-year average from both the T4-T1 and the T4-T1-T1 historical file series."),
             helpText("4. Receivers are individuals who received T4 earnings from the selected jurisdiction but reported a different
                        jurisdiction of residence on their T1 tax returns."),
             helpText("5. Senders are individuals who identified as residing in the selected jurisdiction but received T4 earnings from
                        other jurisdictions."),
             helpText("6. Receiver's earnings are defined as Receiver's T4 earnings received from the selected jurisdiction."),
             helpText("7. Sender's earnings are defined as Sender's T4 earnings received from non-residental jurisdictions."),
             helpText("Notes: Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts 
                        may also be suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
             helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
             
             
             )
           )
      ),
  
  
  
  tabPanel("Industry",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProIndInput", "Select Jurisdiction",
                           choices = c("Newfoundland and labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"
                                       
                           ),
                           selected =c("Newfoundland and labrador")
               ),
               selectInput("IndustryInput", "Select Industry",
                           choices = c("Accommodation and food services","Agriculture, forestry, fishing and hunting","Construction",
                                       "Education services, health care and social assistance","Information and cultural industries; Finance and insurance; Real estate and rental and leasing; Management of companies and enterprise",
                                       "Manufacturing","Mining and quarrying (excluding oil and gas)",
                                       "Oil and gas extraction and support activities","Other services","Professional, scientific and technical services",
                                       "Public administration", "Utilities","Wholesale and Retail trade","Transportation and warehousing",
                                       "Unknown"              
                           ),
                           selected =c("Accommodation and food services")
               ),
               sliderInput("YRInd", "Year Range", beginy, endy, c(beginy, endy)
               ),
               selectInput("tableInd", "Choose a table to download: ",
                           choices = c("Filtered Industry Table","Full Industry Table")
               ),
               downloadButton("downloadItable", "Download")   
             ),
             mainPanel(
               fluidRow(
                 splitLayout(cellWidths=c("50%","50%"),  plotOutput("Indtrend"),  plotOutput("IndInctrend"))
               ),
               br(),
               fluidRow(
                 splitLayout(cellWidths=c("50%","50%"),  plotOutput("Ind_receiver"),  plotOutput("Ind_sender"))
               ),
               br(),
               ### add your style inline css values here
               
               ### added a line of code here too `.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover `###
               tags$style(HTML("
                               .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                               color: #ffffff;
                               }
                               ### ADD THIS HERE ###
                               .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
                               
                               ###To change text and background color of the `Select` box ###
                               .dataTables_length select {
                               color: #0E334A;
                               background-color: #0E334A
                               }
                               
                               ###To change text and background color of the `Search` box ###
                               .dataTables_filter input {
                               color: #0E334A;
                               background-color: #0E334A
                               }
                               
                               thead {
                               color: #ffffff;
                               }
                               
                               tbody {
                               color: #000000;
                               }
                               
                               "
                               
               )
               ),
               
               
               DT::dataTableOutput("Indtable"),
               
               #br(),
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. These estimates are derived from the T1 and T4 tax files and do not include late and re-assessed 
                        taxfilers from the T1 Historical personal master file."),
               helpText("3. Receivers are individuals who received T4 earnings from the selected jurisdiction but reported a different
                        jurisdiction of residence on their T1 tax returns."),
               helpText("4. Senders are individuals who identified as residing in the selected jurisdiction but received T4 earnings from
                        other jurisdictions."),
               helpText("5. When an employee has T4 earnings in more than one industry, he/she will be counted in the 
                        industry of the job in which the employee has the highest T4 earnings."),
               helpText("6. Indstry categories are derived from the North American Industry Classification System (NAICS)."),
               helpText("7. Other services include: Administrative and support; Waste management and remediation services; 
                        Entertainment and recreation; Other services etc."),
               helpText("Notes: Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts 
                        may also be suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           ),
  
  tabPanel("Target Jurisdiction",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProOPInput", "Select Jurisdiction",
                           choices = c("Newfoundland and labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"
                                       
                           ),
                           selected =c("Newfoundland and labrador")
                           ),
               selectInput("ProTPInput", "Select Target Jurisdiction",
                           choices = c("Newfoundland and labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"
                                       
                           ),
                           selected =c("Prince Edward Island")
                           ),
               sliderInput("YRTP", "Year Range", beginy, endy, c(beginy, endy)
                           ),
               selectInput("tableTP", "Choose a table to download: ",
                           choices = c("Filtered TP Table","Full TP Table")
               ),
               downloadButton("downloadTPtable", "Download") 
               
             ),
             mainPanel(
               fluidRow(
                 splitLayout(cellWidths=c("50%","50%"), plotOutput("TPtrend"), plotOutput("TPInctrend"))
               ),
               br(),
               fluidRow(
                 splitLayout(cellWidths=c("50%","50%"), plotOutput("TPComp"), plotOutput("TPCompInc"))
               ),
               br(),
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. These estimates are derived from the T1 and T4 tax files and do not include late and re-assessed 
                        taxfilers from the T1 Historical personal master file."),
               helpText("3. Receivers are individuals who received T4 earnings from the selected jurisdiction but reported the target
                        jurisdiction of residence on their T1 tax returns."),
               helpText("4. Senders are individuals who identified as residing in the selected jurisdiction but received T4 earnings from
                        the target jurisdiction."),
               helpText("5. When an sender has T4 earnings in more than one jurisdiction, he/she will be counted 
                        in the jurisdiction of the job in which the sender employee has the highest T4 earnings."),
               helpText("Notes: Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts 
                        may also be suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           
           ),
           
           
  tabPanel("Age Group",
           sidebarLayout(
             sidebarPanel(
               selectInput("ProAgeInput", "Select Jurisdiction",
                           choices = c("Newfoundland and labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                       "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                       "Yukon", "Northwest Territories","Nunavut"
                                       
                           ),
                           selected =c("Alberta")
                           ),
               checkboxGroupInput( "AgeInput", "Select Aga Groups",
                           choices = c("18 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years",
                                       "55 to 64 years", "65 years and older"
                           ),
                           selected = c("18 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years",
                                        "55 to 64 years", "65 years and older")
                          ),
               sliderInput("YRAge", "Year Range", beginy, endy, c(beginy, endy)
                          ),
               selectInput("GDAgeInput", "Select Gender",
                           choices = c("Male","Female","Both"
                           ),
                           selected =c("Both")
                           ),
               selectInput("TAgeInput", "Select Employment Type",
                           choices = c("Receiver","Sender", "Resident"
                           ),
                           selected =c("Receiver")),
               selectInput("tableAge", "Choose a table to download: ",
                           choices = c("Filtered Age Table","Full Age Table")
               ),
               downloadButton("downloadAtable", "Download")  
               
             ),
             mainPanel(
               plotOutput("Agetrend"),
               br(),
               plotOutput("Agechange"),
               br(),
               helpText("1. Only employees aged 18 or older who earned more than $1,000 (in 2016 constant dollars) are included."),
               helpText("2. These estimates are derived from the T1 and T4 tax files and do not include late and re-assessed 
                        taxfilers from the T1 Historical personal master file."),
               helpText("3. Receivers are individuals who received T4 earnings from the selected jurisdiction but reported a different
                        jurisdiction of residence on their T1 tax returns."),
               helpText("4. Senders are individuals who identified as residing in the selected jurisdiction but received T4 earnings from
                        other jurisdictions."),
               helpText("Notes: Numbers may not add up to totals because of rounding. Counts less than 10 are suppressed. Additional counts 
                        may also be suppressed if the sum of all suppressed counts is less than 10. Counts are rounded to the nearest 5, and earnings are rounded to the nearest 100."),
               helpText("Source: Statistics Canada, Canadian Employer-Employee Dynamics Database.")
               
             )
           )
           
          ),
  
  # tabPanel("Downloading Table",
  #          sidebarLayout(
  #            sidebarPanel(
  #              selectInput("table", "Choose a table: ",
  #                          choices = c("Newfoundland and labrador","Prince Edward Island","Nova Scotia","New Brunswick",
  #                                      "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
  #                                      "Yukon", "Northwest Territories","Nunavut"
  #                                      
  #                          )
  #                
  #              ),
  #              downloadButton("downloadData", "Download")
  #            ),
  #            mainPanel(
  #              #tableOutput("tableD")
  #            )
  #          )
  #          
  # ),
  
  
  tabPanel("Reference",
           navlistPanel(
             tabPanel("Inter-jurisdictional Employees",
                      tags$head(
                        tags$style("label{font-family: 'Lobster', cursive;}")), 
                      h3( "Inter-jurisdictional employees are paid employment of individuals who maintain a permanent residence in a given province/territory while reporting
                          earnings from a different province/territory"))
             
                     ),
           navlistPanel(
             tabPanel("Data Source(s)",
                      tags$head(
                        tags$style("label{font-family: 'Lobster', cursive;}")), 
                      h3( "The data source for this project is the Canadian Employer-Employee Dynamics Database
                        (CEEDD)"))
           ),
           navlistPanel(
             tabPanel("User Guide",
                      tags$head(
                        tags$style("label{font-family: 'Lobster', cursive;}")), 
                      h3("Click", downloadLink('downloadGuide','Here'), "to Download user Guide" 
                        ))
                      
           ),
           navlistPanel(
             tabPanel("Datasets",
                      tags$head(
                        tags$style("label{font-family: 'Lobster', cursive;}")), 
                      h3("Datasets are avaiable for download from", downloadLink('downloadVintage','Here')
                      ))
             
           )    
           
  )
  

  ) 
)
