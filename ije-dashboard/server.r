#setwd('F:/IJE/code/shiny/')
library(shiny)
library(dplyr)
library(leaflet)
library(scales)
library(ggplot2)
#library(highcharter)
options(scipen = 999)

server<-function(input, output, session){
  
  #####
  ## NATIONAL TAB - MAP OF IJE EMPLOYMENT/INCOME
  #####
  
  ## disables income source selection when type input is equal to resident (null values)
  observeEvent(input$TypeInput,{
    if(input$TypeInput=='Resident'){
      updateRadioButtons(session=session,inputId='IncomeSource',selected='Inside the Jurisdiction')
      shinyjs::disable('IncomeSource')
    } else {
      shinyjs::enable('IncomeSource')
    }
  })

  ## reactive filtered table
  mapfilter <- eventReactive(input$NationalGen,{
    table_1_2 %>%
      filter(year%in% input$YearInput,
             source %in% input$SourceInput,
             gender %in% input$GenderInput,
             type %in% input$TypeInput,
             income_source %in% input$IncomeSource) %>%
      mutate(inp=input$SeriesInput,
             dispvar=ifelse(inp=='Employee Count',
                            format(count,big.mark=','), # if employees, format here, income below
                            paste0('$',format(round(income/1000000,1),big.mark=','),' M')),
             prec=ifelse(inp=='Employee Count','Count of','Total income of'))
  },
  ignoreNULL=F)
  
  
  output$mainmap <- renderPlotly({
    
    mapdat <- simple_pr_shapefile %>%
      inner_join(mapfilter(), by='PRUID') %>%
      
      mutate(mapformat=sf::st_transform(
        simple_pr_shapefile$geometry,
        crs= "+proj=laea +lat_0=56.1304 +lon_0=-86.3468 +ellps=WGS84 +units=m +no_defs "))
    
    ## set color scheme for concept (employees in blue, income in red)
    if(first(mapfilter()$prec) == "Count of"){
      seriesvar <- mapdat$count
      lowgrad <- '#f1eef6'
      highgrad <- '#045a8d'
      seriestitle <- 'Number of Inter-Jurisdictional Employees by Province'
      legtitle <- 'Count (log 10 scale)'
      leglabfn <- function(s){format(s,big.mark=',')} 
      
      # seriestitle <- 'Number of Inter-Jurisdictional Employees'
      # 
      pal_count_PR <- createClasses(mapdat$count , "Blues", "transparent", 5)
      # 
      # geo_labels_PR <- sprintf(
      #   "<strong>%s (Employees):  %s </strong>",
      #   mapdat$province, format(mapdat$count, big.mark = ",")) %>%
      #   lapply(htmltools::HTML) # add labels 
      
    } else if(first(mapfilter()$prec) == "Total income of"){
      seriesvar <- mapdat$income
      lowgrad <- '#fef0d9'
      highgrad <- '#b30000'
      seriestitle <- 'Total Income of Inter-Jurisdictional Employees by Province'
      legtitle <- 'Income (log 10 scale)'
      leglabfn <- function(s){paste0('$',format(round(s/1000000,1),big.mark=','),'M')}
      # seriestitle <- 'Income of Inter-Jurisdictional Employees'
      # 
      pal_count_PR <- createClasses(mapdat$income, "Reds", "transparent", 5)
      # 
      # geo_labels_PR <- sprintf(
      #   "<strong>%s (Income): %s </strong>",
      #   mapdat$province, format(mapdat$income, big.mark = ",")) %>%
      #   lapply(htmltools::HTML) # add labels  
    }
    
    mapdat <- mapdat %>%
      mutate(pcol=pal_count_PR$pal(seriesvar))
    
    ggplotly(
      ggplot(mapdat %>% mutate(geometry=mapformat)) +
        geom_sf(aes(fill=seriesvar,
                    text=sprintf("<b>%s, %s</b><br>%s %s %s IJE: %s",
                                 province,year,
                                 prec,tolower(gender),tolower(type),dispvar)),
                
                color="#444444",
                alpha=0.75) + theme_bw() +
        
        labs(title=seriestitle) +
        # scale_fill_manual(values=pcol) +
        
        scale_fill_gradient(name=legtitle,
          low=lowgrad,high=highgrad,na.value='grey.50',
                            breaks=c(min(seriesvar),
                                     median(seriesvar),
                                     max(seriesvar)),
                            labels=c(paste0('Min: ',leglabfn(min(seriesvar)),'\n(',
                                            first(filter(mapdat,seriesvar==min(seriesvar))$province),')\n\n'),
                                     paste0('Med: ',leglabfn(median(seriesvar)),'\n(',
                                            first(filter(mapdat,seriesvar==median(seriesvar))$province),')'),
                                     paste0('\n\nMax: ',leglabfn(max(seriesvar)),'\n(',
                                            first(filter(mapdat,seriesvar==max(seriesvar))$province),')')),
                            trans='log') +
        
        theme(plot.title = element_text(vjust=1, hjust=0.5, size=14, face = "bold"),
              panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
              axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()),
      
      tooltip='text') %>%
      style(hoveron='all')
    
  })
  
  #####
  ## DEPRECATED - LEAFLET STYLE TABLE
  #####
  
  # output$PRcount <- renderLeaflet({
  #   
  #   #####
  #   PRcount <- leaflet(table_1_2)#%>%
  #   # addProviderTiles(providers$OpenStreetMap.BlackAndWhite)
  #   
  #   #filter;
  #   IJE_table1_filted <-
  #     inner_join(simple_pr_shapefile, table_1_2 %>%filter(year%in% input$YearInput,
  #                                                         source %in% input$SourceInput,
  #                                                         gender %in% input$GenderInput,
  #                                                         type %in% input$TypeInput,
  #                                                         income_source %in% input$IncomeSource
  #     ),by = c("PRUID" = "PRUID")) 
  #   
  #   #color;
  #   if(input$SeriesInput == "Employees"){
  #     seriesvar <- IJE_table1_filted$count
  #     seriestitle <- 'Number of Inter-Jurisdictional Employees'
  #     
  #     pal_count_PR <- createClasses(IJE_table1_filted$count , "Blues", "transparent", 5)
  #     
  #     geo_labels_PR <- sprintf(
  #       "<strong>%s (Employees):  %s </strong>",
  #       IJE_table1_filted$province, format(IJE_table1_filted$count, big.mark = ",")) %>%
  #       lapply(htmltools::HTML) # add labels 
  #     
  #   } else if(input$SeriesInput == "Income"){
  #     
  #     seriesvar <- IJE_table1_filted$income
  #     seriestitle <- 'Income of Inter-Jurisdictional Employees'
  #     
  #     pal_count_PR <- createClasses(IJE_table1_filted$income, "Reds", "transparent", 5)
  #     
  #     geo_labels_PR <- sprintf(
  #       "<strong>%s (Income): %s </strong>",
  #       IJE_table1_filted$province, format(IJE_table1_filted$income, big.mark = ",")) %>%
  #       lapply(htmltools::HTML) # add labels  
  #   }
  #   
  #   # geo_labels_PR <- sprintf(
  #   #   "<strong>%s (Employees):  %s </strong>",
  #   #   IJE_table1_filted$province, format(IJE_table1_filted$count, big.mark = ",")) %>%
  #   #   lapply(htmltools::HTML) # add labels 
  #   # 
  #   # labels_inc <- sprintf(
  #   #   "<strong>%s (Income): %s </strong>",
  #   #   IJE_table1_filted$province, format(IJE_table1_filted$income, big.mark = ",")) %>%
  #   #   lapply(htmltools::HTML) # add labels  
  #   
  #   #####
  #   PRcount %>%
  #     addPolygons( data = IJE_table1_filted,stroke = TRUE, color = "#444444", weight = 1, smoothFactor = 0.5,
  #                  opacity = 1.0, fillOpacity = 0.5,fillColor = pal_count_PR$pal(seriesvar),group = "count",
  #                  highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                      bringToFront = FALSE),
  #                  label = geo_labels_PR,
  #                  labelOptions = labelOptions(
  #                    style = list(padding = "3px 8px"),
  #                    textsize = "13px",
  #                    direction = "auto"),              
  #                  popup =paste( '<B>', IJE_table1_filted$province, '</B>', "<br>",
  #                                "Number of Employees: ", format(IJE_table1_filted$count, big.mark = ","), "<br>",
  #                                "Income: ", format(IJE_table1_filted$income, big.mark = "," ), "<br>")
  #     )%>% 
  #     # addCircles(lng = IJE_table1_filted$lon, lat  = IJE_table1_filted$lat, weight = 2, radius = sqrt(IJE_table1_filted$income)*5,opacity = 1.0, fillOpacity = 0.5, fillColor = "transparent",
  #     #            color="red", label = labels_inc, highlightOptions = highlightOptions(color = "white", weight = 3,
  #     #                                                                                 bringToFront = TRUE), group="Income")%>%
  #     addLegend(position ="topright", pal = pal_count_PR$pal, values = seriesvar,
  #               opacity = 1, title = seriestitle, na.label = "No Data")%>%
  #     # addLayersControl( overlayGroups = c("Income"),
  #     #                   options = layersControlOptions(collapsed = TRUE, autoZIndex=TRUE)) %>%
  #     addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) 
  #   
  # })
  
  #National Level download 
  
  National_filted<- reactive({ ## this is a repetition of an earlier table - does it need to be duplicated?
    table_1_2 %>%
      filter(year%in% input$YearInput,
             source %in% input$SourceInput,
             gender %in% input$GenderInput,
             type %in% input$TypeInput,
             income_source %in% input$IncomeSource
      )})
  
  ## verify that this code works
  tableN_down <- reactive ({
    switch( input$tableN,
            "Filtered National Table" = National_filted(),
            "Full National Table" = table_1_2
    )
  })
  
  
  # Downloadable excel file of selected table
  output$downloadNtable <- downloadHandler(
    filename = function(){
      paste(input$tableN, ".csv", sep = "")
      #paste(input$table)
    },
    content = function(file){
      write.csv(tableN_down(), file, row.names = FALSE)
      
    },
    contentType = "csv"
  )
  
  ##### 
  ## JURISDICTION TAB - INCOMING/OUTGOING EMPLOYEES/INCOME BY PROVINCE
  #####
  
  ## Jurisdiction Trendplot
  
  ## if the province is changed here, it will update the selected province in all other tabs
  observeEvent(input$ProvinceInput,
               {
                 lapply(c('ProIndInput','ProOPInput','ProAgeInput'),
                        function(x) updateSelectInput(session=session, inputId=x, selected=input$ProvinceInput))
               })
  
  trend_filted <- eventReactive(input$JurisdGen,{
    table_1_2 %>%
      filter(province %in% input$ProvinceInput,
             year>= input$YRInput[1],
             year<= input$YRInput[2],
             source %in% input$DSInput,
             gender %in% input$GDInput,
             type != "Resident",
             case_when(type == "Incoming" ~ income_source == "Inside the Jurisdiction", T ~ income_source == "Outside the Jurisdiction")
      )},
    ignoreNULL=F)
  
  #count trend
  output$PRtrend <-renderPlotly({
    ggplotly(
      ggplot(trend_filted(), aes(x=year, y = count/1000, group=type, color=type,
                                 text=sprintf('<b>%s, %s</b><br>%s %s employees: %s',
                                              province,year,
                                              gender,tolower(type),
                                              format(count,big.mark=',')))) + 
        geom_line() + geom_point() +
        
        labs(title=paste0("Incoming vs. Outgoing Inter-Jurisdicitonal Employees<br>for ",first(trend_filted()$province))) +
        xlab("Year") + ylab("Employees (x1,000)") +
        
        scale_x_continuous(breaks=seq(2002,2017,2),
                           minor_breaks=seq(2002,2017,1))+
        scale_y_continuous(labels = function(c){paste0(format(c, big.mark = ","),'k')}) +
        scale_colour_manual(name='',values = c("aquamarine4", "yellow3")) +
        
        theme(plot.title = element_text(hjust=0.5,size=14, face = "bold")),
      
      tooltip='text'
    ) %>% layout(margin=list(l=60,r=50,t=75,b=65),
                 legend=list(x=100,y=0.5))
  }) 
  
  #Income trend
  output$PRInctrend <-renderPlotly({
    ggplotly(
      ggplot(trend_filted(), aes(x=year, y = income/1000000, group=type, color=type,
                                 text=sprintf('<b>%s, %s</b><br>Income of %s %s employees<br>%sside the jurisdiction: $%sM',
                                              province,year,
                                              tolower(gender),tolower(type),
                                              str_extract(tolower(type),'.*(?=coming|going)'),
                                              format(round(income/1000000,1),big.mark=',')))) + 
        geom_line() + geom_point() +
        
        labs(title=paste0("Incoming vs. Outgoing Employee Aggregate T4 Earnings<br>for ",first(trend_filted()$province))) +
        xlab("Year") + ylab("Aggregate T4 Earnings (Million $)") +

        scale_x_continuous(breaks=seq(2002,2017,2)) +
        scale_y_continuous(labels = function(c){paste0('$',format(c, big.mark = ","),'M')}) +
        scale_colour_manual(name='',values = c("cyan3", "darkorange2")) +
        
        theme(plot.title = element_text(hjust=0.5,size=14, face = "bold"),
              axis.title = element_text(size=11)),
      
      tooltip='text'
    ) %>% layout(margin=list(l=60,r=50,t=75,b=65),
                 legend=list(x=100,y=0.5))
  }) 
  
  ## DEPRECATED GRAPHS 
  
  #Income receiver and Income Sender trend
  # output$Income_receiver <-renderPlot({
  #   IJE_table1_receive <-
  #     table_1_2 %>%
  #     filter(province %in% input$ProvinceInput,
  #            year>= input$YRInput[1],
  #            year<= input$YRInput[2],
  #            source %in% input$DSInput,
  #            gender %in% input$GDInput,
  #            type == "Incoming",
  #            income_source %in% "Inside the Jurisdiction"
  #     ) 
  #   
  #   ggplot(IJE_table1_receive,aes(x = year )) +
  #     geom_bar(mapping = aes(y = count, colour = "Employee"),stat = 'identity', fill= "aquamarine4")+
  #     #add second dimension
  #     geom_line(mapping = aes( y = income/35000),group = 1, size = 2,  colour = "cyan3")+
  #     scale_x_continuous(breaks=seq(2002,2017,2))+
  #     
  #     #add second y axis
  #     scale_y_continuous(name = "Incoming IJE", 
  #                        labels = function(c){paste0(format(c, big.mark = ","))} ,
  #                        sec.axis = sec_axis(~.*35000, name="Aggregate T4 Earnings",
  #                                            labels = function(b){paste0(format(b, big.mark = ","))}))+
  #     xlab('Year') +
  #     #define colour
  #     scale_colour_manual(values = c("aquamarine4", "cyan3")) +
  #     #label location
  #     theme(legend.position = "none", 
  #           axis.title.y= element_text(color = "aquamarine4", size=12),
  #           axis.title.y.right = element_text(color = "cyan3", size=12), 
  #           plot.title = element_text(hjust=0.5, size=16, face="bold"),
  #           axis.title = element_text(size=12))+
  #     ggtitle('Incoming Inter-Jurisdictional Employees and Aggregate T4 Earnings')
  #   
  # }) 
  # 
  # 
  # output$Income_sender <-renderPlot({
  #   IJE_table1_receive <-
  #     table_1_2 %>%
  #     filter(province %in% input$ProvinceInput,
  #            year>= input$YRInput[1],
  #            year<= input$YRInput[2],
  #            source %in% input$DSInput,
  #            gender %in% input$GDInput,
  #            type == "Outgoing",
  #            income_source %in% "Outside the Jurisdiction"
  #     ) 
  #   
  #   ggplot(IJE_table1_receive,aes(x = year )) +
  #     geom_bar(mapping = aes(y = count, colour = "Employee"),stat = 'identity', fill = "yellow3")+
  #     #add second dimension
  #     geom_line(mapping = aes( y = income/35000),group = 1, size = 2,  colour = "darkorange2")+
  #     scale_x_continuous(breaks=seq(2002,2017,2))+
  #     
  #     #add second y axis
  #     scale_y_continuous(name = "Outgoing IJE", 
  #                        labels = function(c){paste0(format(c, big.mark = ","))} ,
  #                        sec.axis = sec_axis(~.*35000, name="Aggregate T4 Earnings",                                                                                 
  #                                            labels = function(b){paste0(format(b, big.mark = ","))}))+
  #     xlab('Year') +
  #     scale_colour_manual(values = c("yellow3", "darkorange2")) +
  #     #label location
  #     theme( legend.position = "none", 
  #            axis.title.y= element_text(color = "yellow3", size=12),
  #            axis.title.y.right = element_text(color = "darkorange2"), 
  #            plot.title = element_text(hjust=0.5, size=16, face="bold"),
  #            axis.title = element_text(size=12))+
  #     ggtitle('Outgoing Inter-Jurisdictional Employees and Aggregate T4 Earnings')
  #   
  # }) 
  # 
  
  #PR Trend download tables
  
  ## duplicate??
  tableP_down <- reactive ({
    switch( input$tableIn,
            "Filtered Table" = trend_filted(),
            "Full Table" = table_1_2
    )
  })
  
  # Downloadable excel file of selected table
  output$downloadPtable <- downloadHandler(
    filename = function(){
      paste(input$tableIn, ".csv", sep = "")
      #paste(input$table)
    },
    content = function(file){
      write.csv(tableP_down(), file, row.names = FALSE)
      
    },
    contentType = "csv"
  )
  
  #####
  ## TARGET PROVINCE - Income, employment for province pairs by target jurisdiction
  #####
  
  observeEvent(input$ProOPInput,
               {
                 lapply(c('ProvinceInput','ProIndInput','ProAgeInput'),
                        function(x) updateSelectInput(session=session, inputId=x, selected=input$ProOPInput))
               })
  
  ## select/deselect all button - observes if the selectAllInd option is selected or not and fills, clears selection accordingly
  observeEvent(
    eventExpr=input$selectAllProv,
    handlerExpr=if(input$selectAllProv){
      updateCheckboxGroupInput(session=session,inputId='ProTPInput',choices=provList,selected=provList)
    } else {
      updateCheckboxGroupInput(session=session,inputId='ProTPInput',choices=provList,selected=c())
    },
    
    ignoreInit=T
  )
  
  table56910_filtered <- eventReactive(input$TgtJurisdGen,{
    table_56910 %>%
      filter(province %in% input$ProOPInput,
             target_prov %in% input$ProTPInput,
             type %in% input$IncOutTgtJuris,
             year>= input$YRTP[1],
             year<= input$YRTP[2]) %>%
      mutate(target_prov=factor(target_prov,levels=c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia","New Brunswick",
                                                     "Quebec", "Ontario", "Manitoba","Saskatchewan","Alberta","British Columbia",
                                                     "Yukon", "Northwest Territories","Nunavut")),
             direction=ifelse(type=='Incoming','from','in'))
  },
  ignoreNULL=F)
  
  ## count
  output$TPcount <- renderPlotly({
    ggplotly(
      ggplot(table56910_filtered(), aes(x=year, y= count/1000, group=target_prov, color=target_prov,
                                        text=sprintf("<b>%s, %s</b><br>%s employees %s %s: %s",
                                                     province,year,
                                                     type,direction,target_prov,
                                                     format(count,big.mark=',')))) + 
        geom_line() + geom_point()+
        
        labs(title=paste("Inter-Jurisdictional Employment for<br>", first(table56910_filtered()$province))) +
        xlab('Year') + ylab('Employees (x1,000)') +
        
        scale_x_continuous(breaks=seq(2002,2017,2))+
        scale_y_continuous(labels = function(c){paste0(format(c, big.mark = ","),'k')})+
        scale_color_brewer(name='Province',palette='Paired') +
        
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title = element_text(size=11)),
      
      tooltip='text'
    ) %>% layout(margin=list(l=60,r=50,t=75,b=65),
                 legend=list(x=100,y=0.5))
  })
  
  ## income 
  output$TPincome <- renderPlotly({
    ggplotly(
      ggplot(table56910_filtered(), aes(x=year, y= income/1000000, group=target_prov, color=target_prov,
                                        text=sprintf("<b>%s, %s</b><br>Income of %s employees %s %s: %s",
                                                     province,year,
                                                     tolower(type),direction,target_prov,
                                                     paste0('$',format(round(income/1000000,1),big.mark=','),'M')))) + 
        geom_line() + geom_point()+
        
        labs(title=paste("Inter-Jurisdictional Employment Income for<br>", first(table56910_filtered()$province))) +
        xlab('Year') + ylab('Aggregate T4 Earnings (Million $)') +
        
        
        scale_x_continuous(breaks=seq(2002,2017,2))+
        scale_y_continuous(labels = function(c){paste0('$',format(c, big.mark = ","),'M')})+
        scale_color_brewer(name='Province',palette='Paired') +
        
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              axis.title = element_text(size=11)),
      
      tooltip='text'
    ) %>% layout(margin=list(l=60,r=50,t=75,b=65),
                 legend=list(x=100,y=0.5))
  })
  
  ## Download target province table
  tableTP_down <- reactive ({
    switch( input$tableTP,
            "Filtered Industry Table" = table56910_filtered(),
            "Full Table" = table_56910
    )
  })
  
  # Downloadable excel file of selected table
  output$downloadTPtable <- downloadHandler(
    filename = function(){
      paste(input$tableTP, ".csv", sep = "")
      #paste(input$table)
    },
    content = function(file){
      write.csv(tableTP_down(), file, row.names = FALSE)
      
    },
    contentType = "csv"
  )
  
  #####
  ## INDUSTRY TAB - Graphs by industry for target jurisdiction
  #####
  
  observeEvent(input$ProIndInput,
               {
                 lapply(c('ProvinceInput','ProOPInput','ProAgeInput'),
                        function(x) updateSelectInput(session=session, inputId=x, selected=input$ProIndInput))
               })
  
  ## select/deselect all button - observes if the selectAllInd option is selected or not and fills, clears selection accordingly
  observeEvent(
    eventExpr=input$selectAllInd,
    handlerExpr=if(input$selectAllInd){
      updateCheckboxGroupInput(session=session,inputId="IndustryInput",choices=indList,selected=indList)
    } else {
      updateCheckboxGroupInput(session=session,inputId="IndustryInput",choices=indList,selected=c())
    },
    
    ignoreInit=T
  )
  
  ## 
  ind_filtered <- eventReactive(input$IndustryGen,{
    table_3478 %>%
      filter(province %in% input$ProIndInput,
             industry %in% input$IndustryInput,
             type %in% input$IncOutIndustry,
             year>= input$YRInd[1],
             year<= input$YRInd[2]) %>%
      mutate(industry=factor(industry,levels=indList))
  },
  ignoreNULL=F)
  
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  ## count trend
  output$IndCount <- renderPlotly({
    ggplotly(
      ggplot(ind_filtered(), aes(x=year, y=count/1000, group=industry, color=industry,
                                 text=sprintf("<b>%s, %s</b><br>Industry: %s<br>%s employees: %s",
                                              province,year,
                                              industry,
                                              type,format(count,big.mark=',')))) +
        geom_line() + geom_point() +
        
        labs(title=paste0("Inter-Jurisdictional Employment for <br>",first(ind_filtered()$province)," by Industry")) +
        xlab("Year") + ylab("Employees (x1,000)") + 
        
        scale_x_continuous(breaks=seq(2002,2017,2)) + 
        scale_y_continuous(labels = function(c){paste0(format(c, big.mark = ","),'k')}) +
        scale_colour_manual(name='Industry',values=getPalette(length(unique(ind_filtered()$industry)))) +
        
        theme(plot.title = element_text(hjust=0.5,size=14, face = "bold"),
              axis.title = element_text(size=11)),
      
      tooltip='text'
    ) %>% layout(margin=list(l=60,r=50,t=75,b=65),
                 legend=list(x=100,y=0.5))
  })
  
  ## income trend
  output$IndIncome <- renderPlotly({
    ggplotly(
      ggplot(ind_filtered(), aes(x=year, y=income/1000000, group=industry, color=industry,
                                 text=sprintf("<b>%s, %s</b><br>Industry: %s<br>Income of %s employees %sside the jurisdiction: %s",
                                              province,year,
                                              industry,tolower(type),
                                              str_extract(tolower(type),'.*(?=coming|going)'),
                                              paste0('$',format(round(income/1000000,1),big.mark=','),'M')))) +
        geom_line() + geom_point() +
        
        labs(title=paste0("Inter-Jurisdictional Employment Income for <br>",first(ind_filtered()$province)," by Industry")) +
        xlab("Year") + ylab("Aggregate T4 Earnings (Million $)") +
        
        scale_x_continuous(breaks=seq(2002,2017,2)) + 
        scale_y_continuous(labels = function(c){paste0('$',format(c, big.mark = ","),'M')}) +
        scale_colour_manual(name='Industry',values=getPalette(length(unique(ind_filtered()$industry)))) +
        
        theme(plot.title = element_text(hjust=0.5,size=14, face = "bold"),
              axis.title = element_text(size=11)),
      
      tooltip='text'
    ) %>% layout(margin=list(l=60,r=50,t=75,b=65),
                 legend=list(x=100,y=0.5))
  })
  
  #Industry Trend download tables
  
  tableI_down <- reactive ({
    switch( input$tableInd,
            "Filtered Industry Table" = ind_filtered(),
            "Full Table" = table_3478
    )
  })
  
  # Downloadable excel file of selected table
  output$downloadItable <- downloadHandler(
    filename = function(){
      paste(input$tableInd, ".csv", sep = "")
      #paste(input$table)
    },
    content = function(file){
      write.csv(tableI_down(), file, row.names = FALSE)
    },
    contentType = "csv"
  )
  
  
  #####
  ## AGE GROUP - Age of IJE over time, relative changes
  #####
  
  observeEvent(input$ProAgeInput,
               {
                 lapply(c('ProvinceInput','ProIndInput','ProOPInput'),
                        function(x) updateSelectInput(session=session, inputId=x, selected=input$ProAgeInput))
               })
  
  ## select/deselect all button - observes if the selectAllAge option is selected or not and fills, clears selection accordingly
  observeEvent(
    eventExpr=input$selectAllAge,
    handlerExpr=if(input$selectAllAge){
      updateCheckboxGroupInput(session=session,inputId="AgeInput",choices=ageList,selected=ageList)
    } else {
      updateCheckboxGroupInput(session=session,inputId="AgeInput",choices=ageList,selected=c())
    },
    
    ignoreInit=T
  )
  
  agePalette <- paste0('#',c('F8766D','B79F00','00BA38','00BFC4','619CFF','F564E3'))
  names(agePalette) <- unique(table_11$age_group)
  
  table11_filted<- eventReactive(input$AgeGen,{
    table_11 %>%
      filter(age_group %in% input$AgeInput,
             province %in% input$ProAgeInput,
             gender %in% input$GDAgeInput,
             type %in% input$TAgeInput,
             year>= input$YRAge[1],
             year<= input$YRAge[2])
  },
  ignoreNULL=F)
  
  #Age Trend 
  output$Agetrend <-renderPlotly({
    ggplotly(
      ggplot(table11_filted(), aes(x=year, y = count/1000, group=age_group, color=age_group,
                                   text=sprintf('<b>%s, %s</b><br>%s %s employees aged %s: %s',
                                                province,year,
                                                gender,tolower(type),age_group,
                                                format(count,big.mark = ',')))) + 
        geom_line() + geom_point() +
        
        labs(title=paste0("Inter-Jurisdictional Employees by Age Group<br>for ",first(table11_filted()$province))) +
        xlab("Year") + ylab("Number of Employees (x1,000)") +
        
        scale_x_continuous(breaks=seq(2002,2017,2))+
        scale_y_continuous(labels = function(c){paste0(format(c, big.mark = ","),'k')})+
        scale_color_manual(name='Age Group',values=agePalette) +
        
        theme(plot.title = element_text(hjust=0.5, size=14, face = "bold"),
              axis.title = element_text(size=11)),
      
      tooltip='text'
    ) %>% layout(margin=list(l=60,r=50,t=75,b=65),
                 legend=list(x=100,y=0.5))
  }) 
  
  
  #Age chanages
  table11_changes <- eventReactive(input$AgeGen,{
    table_11 %>%
      filter(age_group %in% input$AgeInput,
             province %in% input$ProAgeInput,
             gender %in% input$GDAgeInput,
             type %in% input$TAgeInput,
             year %in% c(input$YRAge[1], input$YRAge[2])) %>% 
      group_by(age_group) %>%
      mutate(pct_change = (lead(count)/count-1)) %>%
      ungroup() %>%
      mutate(yearmin=min(year),yearmax=max(year)) %>%
      filter(year %in% input$YRAge[1])
  },
  ignoreNULL=F)
  
  output$Agechange <-renderPlotly({
    ggplotly(
      ggplot(table11_changes(), aes(x=age_group, color=age_group, fill=age_group,
                                    text=sprintf('<b>%s</b><br>Percent change in %s %s employees<br>from %s to %s: %s',
                                                 province,tolower(gender),tolower(type),
                                                 yearmin,yearmax,
                                                 paste(round(100*(pct_change),1),'%')))) + 
        geom_histogram(mapping = aes(y = pct_change), position = "dodge",stat = 'identity')+
        
        labs(title=paste("Percentage Changes of Employment by Age Group from", first(table11_changes()$yearmin), "to", first(table11_changes()$yearmax),"<br>for",first(table11_filted()$province))) +
        xlab('Age Group') + ylab('Change in Employment (%)') +
        
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_color_manual(name='Age Group',values=agePalette) +
        scale_fill_manual(name='Age Group',values=agePalette) +
        
        theme(plot.title = element_text(hjust=0.5, size=14, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1)),
      
      tooltip='text'
    ) %>% layout(margin=list(l=60,r=50,t=75,b=65),
                 legend=list(x=100,y=0.5))
  })  
  
  
  #Age Group Trend download tables
  tableA_down <- reactive ({
    switch( input$tableAge,
            "Filtered Age Table" = table11_filted(),
            "Full Age Table" = table_11
    )
  })
  
  
  # Downloadable excel file of selected table
  output$downloadAtable <- downloadHandler(
    filename = function(){
      paste(input$tableAge, ".csv", sep = "")
      #paste(input$table)
    },
    content = function(file){
      write.csv(tableA_down(), file, row.names = FALSE)
      
    },
    contentType = "csv"
  )
  
  ## ADDITIONAL DOWNLOADS?
  
  # tableInput <- reactive ({
  #   switch( input$table,
  #           "Alberta" = table_AB,
  #           "British Columbia" = table_BC,
  #           "Manitoba" = table_MB,
  #           "New Brunswick" = table_NB,
  #           "Newfoundland and labrador" = table_NL,
  #           "Northwest Territories" = table_NT,
  #           "Nova Scotia" = table_NS,
  #           "Nunavut" = table_NU,
  #           "Ontario" = table_ON,
  #           "Prince Edward Island" = table_PE,
  #           "Quebec" = table_QC,
  #           "Saskatchewan" = table_SK,
  #           "Yukon" = table_YT
  #   )
  # })
  # 
  # 
  # # display the selected table
  # output$tableD <- renderTable({
  #   tableInput()
  # })
  # 
  # # Downloadable excel file of selected table
  # output$downloadData <- downloadHandler(
  #   filename = function(){
  #     paste(input$table, ".csv", sep = "")
  #   },
  #   content = function(file){
  #     write.csv(tableInput(), file, row.names = FALSE)
  #     
  #   }
  # )
  
  #####
  ## DOWNLOAD TABS
  #####
  
  
  
  output$downloadGuide <- downloadHandler(
    filename = 'Methodological Guide on IJEs-ENGLISH-Dec.2017.pdf',
    
    content = function(infile){
      file.copy('./data/downloadables/Methodological Guide on IJEs-ENGLISH-Dec.2017.pdf',infile)
      # save_object('david-wavrock/ije/Methodological Guide on IJEs-ENGLISH-Dec.2017.pdf',bucket=minio_filist,use_https=F,region='',file=infile)
    }
  )
  
  output$downloadVintage <- downloadHandler(
    filename = 'English_Version.zip',
    
    content = function(infile){
      # zip(zipfile=infile,files=list.files('./data/downloadables/English_Version'),flags='-j')
      file.copy('./data/downloadables/English_Version.zip',infile)
      # save_object('david-wavrock/ije/English_Version.zip',bucket=minio_filist,use_https=F,region='',file=infile)
    },
    contentType = 'application/zip'
    
  )
  
}
