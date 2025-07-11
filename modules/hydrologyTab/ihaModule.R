#' Continuous Data Exploration / Hydrology / IHA (user interface side)
#'
#' @param id 
#'
IHAModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("IHA_input_1")),
              uiOutput(ns("IHA_input_2")),
              uiOutput(ns("display_IHA_button")),
              hr(),
              uiOutput(ns("display_save_IHA_button"))
          ) # end of panel body
      ) # end of panel
    ),
    mainPanel(
      width = 9,
      column(
        width = 12,
        #uiOutput(ns("display_help_text_IHA")),
        
        shinydashboard::box(id=ns("display_help_text_IHA"), width=12, class="well",
                            h4("Hydrology-IHA"),
                            br(),
                            div(style="width:100%;","Indicators of Hydrologic Alteration (IHA) software was developed by scientists at The Nature Conservancy to facilitate hydrologic analysis in an ecologically-meaningful manner (Richter et al. 1996)."),
                            br(),
                            div(style="width:100%;","The output from this Shiny app is derived from the IHA R package on GitHub (",
                            a('https://github.com/jasonelaw/iha', href='https://github.com/jasonelaw/iha', target='_blank'),
                            "). It produces five groups of statistical outputs, with results grouped by calendar year:"),
                            tags$ol(
                             tags$li("Monthly mean"),
                             tags$li("Twelve magnitude/duration statistics (1-, 3-, 7-, 30- and 90-day min/max, zero flow days, base index)"),
                             tags$li("Julian day of annual minimum and maximum discharge"),
                             tags$li("Number and length of high and low pulses"),
                             tags$li("Rate and frequency of water condition changes (rise rate, fall rate, reversals)"),
                            ),
                            br(),
                            div(style="width:100%", "The statistics are intended to be used with discharge data but we are experimenting with also using the statistics with sensor depth and water level data (since discharge data aren’t available for some of the RMN sites)."),
                            br(),
                            div(style="width:100%; font-weight:bold;", "Citation:"),
                            div(style="width:100%","Richter, B.D., Baumgartner, J.V., Powell, J., Braun, D.P.. 1996. A Method for Assessing Hydrologic Alteration within Ecosystems. Conservation Biology 10 (4): 1163-1174." ,
                                a('https://www.waterboards.ca.gov/waterrights/water_issues/programs/bay_delta/docs/cmnt081712/sldmwa/richteretal1996.pdf', href=' https://www.waterboards.ca.gov/waterrights/water_issues/programs/bay_delta/docs/cmnt081712/sldmwa/richteretal1996.pdf', target='_blank')),
                            br(),
                            div(style="width:100%;font-weight:bold;","Additional R packages that may be of interest for working with hydrologic data: "),
                            div(style="width:100%","Environmental Flow Components (EFC) statistics (the IHA R package does not provide these) - ",
                                a('https://github.com/mkoohafkan/flowregime/', href=' https://github.com/mkoohafkan/flowregime/', target='_blank')),
                            br(),
                            div(style="width:100%","Low flow statistics ( ",
                                a('https://pubs.usgs.gov/sir/2008/5126/section3.html', href='https://pubs.usgs.gov/sir/2008/5126/section3.html', target='_blank'),
                                " ) ",
                                a('https://github.com/cran/lfstat', href='https://github.com/cran/lfstat', target='_blank')),
                            br(),
                            div(style="width:100%", "USGS EGRET package ( ",
                                a('https://pubs.usgs.gov/tm/04/a10/', href='https://pubs.usgs.gov/tm/04/a10/', target='_blank'),
                                " ) ",
                                a('https://cran.r-project.org/web/packages/EGRET/index.html', href='https://cran.r-project.org/web/packages/EGRET/index.html', target='_blank'))
                            ) ,#end of box
       
        DT::dataTableOutput(ns("display_IHA_table_1")),
        shinyjs::hidden(
          div(
            id = ns("IHA_plot_1_panel"),
              hr(),
              plotOutput(ns("IHA_plot_1"))
          ) # div end
        ), # shinyjs:: hidden end
        br(),
        br(),
        DT::dataTableOutput(ns("display_IHA_table_2")),
        shinyjs::hidden(
          div(
            id = ns("IHA_plot_2_panel"),
              hr(),
              plotOutput(ns("IHA_plot_2a")),
              br(),
              plotOutput(ns("IHA_plot_2b"))
          ) # div end
        ) # shinyjs:: hidden end
        , br(),br(),
        DT::dataTableOutput(ns("display_IHA_table_3")),
        shinyjs::hidden(
          div(
            id = ns("IHA_plot_3_panel"),
              hr(),
              plotOutput(ns("IHA_plot_3"))
          ) # div end
        ) # shinyjs:: hidden end
        , br(),br(),
        DT::dataTableOutput(ns("display_IHA_table_4")),
        shinyjs::hidden(
          div(
            id = ns("IHA_plot_4_panel"),
              hr(),
              plotOutput(ns("IHA_plot_4a")),
              br(),
              plotOutput(ns("IHA_plot_4b"))
          ) # div end
        ) # shinyjs:: hidden end
        , br(),br(),
        DT::dataTableOutput(ns("display_IHA_table_5")),
        shinyjs::hidden(
          div(
            id = ns("IHA_plot_5_panel"),
              hr(),
              plotOutput(ns("IHA_plot_5")),
              br()
          ) # div end
        ) # shinyjs:: hidden end
      )
    ) # mainPanel end
  ) # sidebarLayout end

}

#' Continuous Data Exploration / Hydrology / IHA (server side)
#'
#' @param id 
#' @param dailyStats 
#' @param loaded_data 
#' @param uploaded_data 
#' @param to_download 
#' @param renderIHA 
#'
IHAModuleServer <- function(id, dailyStats, loaded_data, uploaded_data, to_download, renderIHA, gageDailyRawData) {
 
  localStats <- reactiveValues(stats=list(),myData.IHA=NULL,IHA.group.1=NULL)
  
  variables_avail <- reactiveVal()
  
  
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
            #localResults <- reactiveValues(myData.IHA=NULL,IHA.group.1=NULL)
            
          localResults <- reactiveValues(
            myData.IHA=NULL,
            IHA.group.1=NULL,
            IHA.group.2= NULL,
            IHA.group.3 = NULL,
            IHA.group.4 = NULL,
            IHA.group.5 = NULL
            )
            
          buttonState <- reactiveValues(
            btn1 = FALSE,
            btn2 = FALSE,
            btn3 = FALSE,
            btn4 = FALSE,
            btn5 = FALSE
          )
              
            
            observe({
            localStats <- dailyStats
            
            #gageData <- gageDailyRawData
            if(renderIHA$render == TRUE) {
              # This dropdown value is not used anywhere so commenting it out
              # output$IHA_input_1 <- renderUI({
              #   if (length(localStats$processed_dailyStats)==0){
              #   variables_avail <- names(uploaded_data)
              #   date_keys_in_favor_order <- c("Date.Time","DATE.TIME","Year","YEAR","Date","DATE","MonthDay")
              #   possible_date_columns <- date_keys_in_favor_order[date_keys_in_favor_order %in% variables_avail]
              #   }else{
              #     possible_date_columns <- "Date"
              #     variables_avail <- possible_date_columns
              #   }
              #   selectizeInput(ns("IHA_Date_name"),label ="Select Date Column",
              #                  choices=variables_avail,
              #                  multiple = FALSE,
              #                  selected=possible_date_columns[1],
              #                  options = list(hideSelected = FALSE))
              # })
              output$IHA_input_1 <- renderUI({
                radioButtons(ns("select_data_source"), label = "Select flow data source", 
                             choices = c("Uploaded data", "Downloaded USGS gage data"),
                             selected = "Uploaded data")
              })


              observeEvent(input$select_data_source, {
                # clear output
                output$IHA_input_2 <- renderUI({})
                variables_avail <- NULL
                
                  if(input$select_data_source == "Uploaded data"){
                    variables_avail <- names(localStats$processed_dailyStats)
                    parameter_to_select <- variables_avail[grep('Discharge',variables_avail,ignore.case=TRUE)][1]
                    
                   output$IHA_input_2 <- renderUI({ 
                     selectizeInput(ns("parameter_name"),label ="Select uploaded flow parameter",
                                   choices=variables_avail,
                                   multiple = FALSE,
                                   selected= parameter_to_select,
                                   options = list(hideSelected = FALSE))
                  })

                  }
                  if(input$select_data_source == "Downloaded USGS gage data"){
                    if(identical(gageDailyRawData$gageColName, character(0))){
                      shinyalert("Download USGS Data", "Please download data in the Download USGS daily flow tab to access this feature.", "warning")
                      } 
                      output$IHA_input_2 <- renderUI({selectizeInput(ns("parameter_name"), label = "Select downloaded USGS flow parameter",
                                     choices = gageDailyRawData$gageColName,
                                     multiple = FALSE)})
                      
                  }
              })
              


              output$display_IHA_button <- renderUI({
                actionButton(inputId=ns("display_IHA"), label="Display IHA tables",class="btn btn-primary")
              })
              
              #shinyjs::show(id=ns("display_help_text_IHA"), asis=TRUE)
              
              #Nilima Gandhi - Remvoing old way, it is a overkill, but keeping the code, do not know if there is a future plan to use the file.
              # output$display_help_text_IHA <- renderUI({
              #   verbatimTextOutput("help_text_IHA")
              # })
              # 
              # output$help_text_IHA <- renderText({
              #   filePath <- "help_text_files/Hydrology_IHA.txt"
              #   fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
              #   fileText
              # })
              
            }
          })
           observeEvent(input$parameter_name, {
             shinyjs::hide(ns("save_IHA"), asis=TRUE)
           })
          
          observeEvent(input$display_IHA, {
            localStats <- dailyStats
            #shinyjs::hide(id=ns("display_help_text_IHA"), asis=TRUE)

            output$display_IHA_table_1 <- renderUI({
              dataTableOutput("IHA_table_1")
            })
            
            output$display_IHA_table_2 <- renderUI({
              dataTableOutput("IHA_table_2")
            })
            
            output$display_IHA_table_3 <- renderUI({
              dataTableOutput("IHA_table_3")
            })
            
            output$display_IHA_table_4 <- renderUI({
              dataTableOutput("IHA_table_4")
            })
            
            output$display_IHA_table_5 <- renderUI({
              dataTableOutput("IHA_table_5")
            })

            #myList <- localStats$processed_dailyStats
            if(input$select_data_source == "Uploaded data"){
              myList <- localStats$processed_dailyStats
              
              if (length(myList)>0){
                variable_to_IHA <- input$parameter_name
                myData <- myList[[which(names(myList)==variable_to_IHA)]]
                mean_col <- paste0(input$parameter_name,".mean")
                myData <- myData[c('Date',mean_col)]
                myData.IHA<- read.zoo(myData,format="%Y-%m-%d")
                myData.IHA.noNA <- myData %>% dplyr::filter(is.na(!!sym(mean_col))==FALSE) %>% read.zoo(format="%Y-%m-%d")
                #localStats$myData.IHA <- myData.IHA
                localResults$myData.IHA <- myData.IHA
              }else{
                myData <- uploaded_data()
                print(paste0("the file name is:",loaded_data$name))
              }

              iha_output_file_str <- paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                            "_",
                                            input$parameter_name,
                                            "_iha_")
              
            } else if(input$select_data_source == "Downloaded USGS gage data"){
              myData <- gageDailyRawData$gagedata %>% dplyr::select(Date.Time, input$parameter_name) %>% dplyr::rename("Date" = "Date.Time")
              myData.IHA <- read.zoo(myData,format="%Y-%m-%d")
              myData.IHA.noNA <- myData %>% dplyr::filter(is.na(!!sym(input$parameter_name))==FALSE) %>% read.zoo(format="%Y-%m-%d")
              localResults$myData.IHA <- myData.IHA
              
              iha_output_file_str <- paste0("USGS_gage_", unique(unique(gageDailyRawData$gagedata$GageID)), "_", input$parameter_name, "_iha_")
            }
            

            myYr <- "calendar"
            ## IHA parameters group 1; Magnitude of monthly water conditions
            Analysis.Group.1 <- group1(myData.IHA.noNA, year=myYr, FUN = mean)
            #save(Analysis.Group.1,file="IHA_group_1.RData")
            Analysis.Group.1 <- as.data.frame(Analysis.Group.1) %>% mutate_if(is.numeric,round,digits=2)
            #localStats$IHA.group.1 <- Analysis.Group.1
            localResults$IHA.group.1 <- Analysis.Group.1 %>% tibble::rownames_to_column("year")
            renderTables(id="display_IHA_table_1", data=Analysis.Group.1, tblTitle="Group 1: Monthly mean", btnId=ns("display_IHA_plot_1"), iha_output_file_str = iha_output_file_str)
            
            
            ## IHA parameters group 2: Magnitude of monthly water condition and include 12 parameters
            Analysis.Group.2 <- group2(myData.IHA.noNA, year=myYr)
            #save(Analysis.Group.2,file="IHA_group_2.RData")
            Analysis.Group.2 <- as.data.frame(Analysis.Group.2) %>% mutate_if(is.numeric,round,digits=2)
            #localStats$IHA.group.2 <- Analysis.Group.2
            localResults$IHA.group.2 <- Analysis.Group.2 # LCN: I do not know why this table already has year as a column
            renderTables(id="display_IHA_table_2", data=Analysis.Group.2, tblTitle="Group 2: Twelve magnitude/duration statistics (1-, 3-, 7-, 30- and 90-day min/max, zero flow days, base index)",btnId=ns("display_IHA_plot_2"), iha_output_file_str = iha_output_file_str)

            
            ## IHA parameters group 3:Timing of annual extreme water conditions
            Analysis.Group.3 <- group3(myData.IHA.noNA, year=myYr)
            #save(Analysis.Group.3,file="IHA_group_3.RData")
            #localStats$IHA.group.3 <- Analysis.Group.3
            localResults$IHA.group.3 <- Analysis.Group.3 %>% as.data.frame() %>% tibble::rownames_to_column("year")
            renderTables(id="display_IHA_table_3", data=Analysis.Group.3, tblTitle="Group 3: Julian day of annual minimum and maximum discharge", btnId=ns("display_IHA_plot_3"), iha_output_file_str = iha_output_file_str)
            
            ## IHA parameters group 4; Frequency and duration of high and low pulses
            # defaults to 25th and 75th percentiles
            # need to remove NAs to deal with quantile defaults
           
            Analysis.Group.4 <- group4(myData.IHA.noNA, year=myYr)
            #save(Analysis.Group.4,file="IHA_group_4.RData")
            #localStats$IHA.group.4 <- Analysis.Group.4
            localResults$IHA.group.4 <- Analysis.Group.4 %>% as.data.frame() %>% tibble::rownames_to_column("year")
            renderTables(id="display_IHA_table_4", data=Analysis.Group.4, tblTitle="Group 4: Number and length of high and low pulses", btnId=ns("display_IHA_plot_4"), iha_output_file_str = iha_output_file_str)
            

            ## IHA parameters group 5; Rate and frequency of water condition changes
            Analysis.Group.5 <- group5(myData.IHA.noNA, year=myYr)
            #save(Analysis.Group.5,file="IHA_group_5.RData")
            #localStats$IHA.group.5 <- Analysis.Group.5
            localResults$IHA.group.5 <- Analysis.Group.5 %>% as.data.frame() %>% tibble::rownames_to_column("year")
            Analysis.Group.5 <- as.data.frame(Analysis.Group.5) %>% mutate_if(is.numeric,round,digits=2)
            
            renderTables(id="display_IHA_table_5", data=Analysis.Group.5, tblTitle="Group 5: Rate and frequency of water condition changes (rise rate, fall rate, reversals)", btnId=ns("display_IHA_plot_5"), iha_output_file_str = iha_output_file_str)
            

            
            ## create Excel Workbook 
            require(openxlsx)
            Group.Desc <- c("Monthly mean"
                            ,"Magnitude of monthly water condition and include 12 parameters"
                            ,"Timing of annual extreme water conditions"
                            ,"Frequency and duration of high and low pulses"
                            ,"Rate and frequency of water condition changes")
            df.Groups <- as.data.frame(cbind(paste0("Group",1:5),Group.Desc))
            myDate <- format(Sys.Date(),"%Y%m%d")
            Notes.User <- Sys.getenv("USERNAME")
            myYr <- "calendar"
            Notes.Names <- c("Dataset (SiteID)","IHA.Year","Analysis.Date (YYYYMMDD)"
                             ,"Analysis.Time (HHMMSS)","Analysis.User")
            Notes.Data <- c(loaded_data$name, myYr, myDate, Notes.User)
            df.Notes <- as.data.frame(cbind(Notes.Names,Notes.Data))
            # Open/Create file
            myFile.XLSX <- paste(iha_output_file_str, "xlsx", sep=".")
            Notes.Summary <- summary(localResults$myData.IHA)
            
            hs <- createStyle(fgFill = "#D9D9D9", textDecoration = "bold")
            
            wb <- createWorkbook()
            
            addWorksheet(wb, "AnalysisNotes")
            addWorksheet(wb, "Summary")
            addWorksheet(wb, "TableDescriptions")
            addWorksheet(wb, "Group1")
            addWorksheet(wb, "Group2")
            addWorksheet(wb, "Group3")
            addWorksheet(wb, "Group4")
            addWorksheet(wb, "Group5")
            
            writeData(wb, "AnalysisNotes", df.Notes, headerStyle = hs)
            writeData(wb, "Summary", Notes.Summary, headerStyle = hs)
            writeData(wb, "TableDescriptions", df.Groups, headerStyle = hs)
            writeData(wb, "Group1", localResults$IHA.group.1, headerStyle = hs)
            writeData(wb, "Group2", localResults$IHA.group.2, headerStyle = hs)
            writeData(wb, "Group3", localResults$IHA.group.3, headerStyle = hs)
            writeData(wb, "Group4", localResults$IHA.group.4, headerStyle = hs)
            writeData(wb, "Group5", localResults$IHA.group.5, headerStyle = hs)
            
            to_download$wb_IHA <- wb
            to_download$fileName_IHA <- myFile.XLSX

            output$display_save_IHA_button <- renderUI({
              downloadButton(outputId=ns("save_IHA"), label="Save IHA results to excel",class="btn btn-primary")
            })

            runjs(sprintf('document.getElementById("%s").scrollIntoView({ behavior: "smooth" });', ns("display_IHA_table_1")))
          }) #observeEvent end
          
          
          output$save_IHA <- downloadHandler(
            filename = function(){
              to_download$fileName_IHA
            },
            content = function(file){
              saveWorkbook(to_download$wb_IHA,file)
            }
            
          )
          
          renderTables <- function(id, data, tblTitle, options, btnId, iha_output_file_str) {
            
            tempStr <- paste0("function ( e, dt, node, config ) {",
                "Shiny.setInputValue('", btnId ,"' , true, {priority: 'event'})","}")
            print(tempStr)
            #c('csv','excel','pdf'),
            
            IHA.table.options <- list(
              scrollX = TRUE, #allow user to scroll wide tables horizontally
              stateSave = FALSE,
              pageLength = 15,
              dom = 'Bt',
              buttons = list(
                             list(extend='copy', text='Copy', className="btn btn-primary"),
                             list(extend='print', text='Print', className="btn btn-primary"),
                             
                             list(extend='collection', buttons = 
                                    list(
                                      list(extend = "csv", filename = paste0(iha_output_file_str, str_sub(tblTitle, 1, 7))),
                                      list(extend = "excel", filename = paste0(iha_output_file_str, str_sub(tblTitle, 1, 7))),
                                      list(extend = "pdf", filename =  paste0(iha_output_file_str, str_sub(tblTitle, 1, 7))) 
                                    ),text='Download', className="btn btn-primary"),
                             
                             
                             list(extend='collection', text='Show/Hide Plot',action = DT::JS(tempStr), className="btn btn-primary")
              ),
              columnDefs = list(list(className="dt-center",targets="_all")),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#e3e3e3', 'color': '#000'});",
                "$('.dt-buttons button').removeClass('dt-button');",
                "$('.dt-button-collection button').removeClass('dt-button');",
                "}")
            )
              
            output[[id]] <- DT::renderDataTable({
              table.title.1 <- tblTitle
              myTable <- DT::datatable(
                data,
                caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
                extensions ="Buttons",
                rownames = TRUE,
                options = IHA.table.options
              ) # dataTable end
              print(myTable)
            })  # renderDT end

          }
          
          observeEvent(input$display_IHA_plot_1, {
            if(buttonState$btn1 == FALSE) {
                shinyjs::show(ns("IHA_plot_1_panel"),asis=TRUE)
                buttonState$btn1 <- TRUE
                
                data_for_plot_1 <- data.frame(localResults$IHA.group.1,row.names = NULL) %>% 
                  rename("Year" = "year") %>%  
                  pivot_longer(cols = -Year, names_to = "Month", values_to = "Values") %>% 
                  mutate(Month = match(Month, month.name))

                output$IHA_plot_1 <- renderPlot({
                  p1 <- ggplot(data_for_plot_1, aes(x= Month,y=Values, color = Year))+
                    geom_line(size=0.8,linetype="dashed")+
                    labs(x = "Month",y = paste0("Mean monthly conditions"))+
                    theme_minimal()+
                    theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                          ,plot.background = element_rect(color="grey20",size=2)
                          ,legend.position = "right"
                    ) +
                    scale_x_continuous(breaks = c(1:12), labels = month.abb)
                  print(p1)
              
            }) # renderPlot end
          }else{
            buttonState$btn1 <- FALSE
            shinyjs::hide(ns("IHA_plot_1_panel"),asis=TRUE)
            
          } # if else loop end
    }) #observeEvent to display IHA plot 1 end
          
          
   observeEvent(input$display_IHA_plot_2, {

            if(buttonState$btn2 == FALSE){
              
              shinyjs::show(ns("IHA_plot_2_panel"), asis=TRUE)
              buttonState$btn2 <- TRUE
              data_to_plot <- localResults$IHA.group.2[,1:11]
              column_names <- colnames(data_to_plot)
              min_cols_to_select <- c("year",column_names[str_detect(column_names,"Min")])
              max_cols_to_select <- c("year",column_names[str_detect(column_names,"Max")])
              data_to_plot_min <- data_to_plot[min_cols_to_select]
              data_to_plot_max <- data_to_plot[max_cols_to_select]
              data_for_plot_2a <- data_to_plot_min %>% gather(key,value,-year) %>% mutate(key = as.factor(key) %>% 
                                                                                            fct_relevel(c("1 Day Min",
                                                                                                          "3 Day Min", 
                                                                                                          "7 Day Min",
                                                                                                          "30 Day Min", 
                                                                                                          "90 Day Min"))) ## convert into long format
              data_for_plot_2b <- data_to_plot_max %>% gather(key,value,-year)%>% mutate(key = as.factor(key) %>% 
                                                                                           fct_relevel(c("1 Day Max",
                                                                                                         "3 Day Max", 
                                                                                                         "7 Day Max",
                                                                                                         "30 Day Max", 
                                                                                                         "90 Day Max"))) ## convert into long format
              
              output$IHA_plot_2a <- renderPlot({
                p1 <- ggplot(data_for_plot_2a,aes(x=year,y=value,fill=key))+
                  geom_bar(stat="identity",position="dodge")+
                  labs(x = "Year",y = paste0("Water condition min"),fill="Parameters")+
                  theme_minimal()+
                  scale_x_continuous(breaks=unique(data_for_plot_2a$year),labels = unique(data_for_plot_2a$year))+
                  theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                        ,plot.background = element_rect(color="grey20",size=2)
                        ,legend.position = "right"
                  )
                print(p1)
                
              }) # renderPlot end
              
              output$IHA_plot_2b <- renderPlot({
                p2 <- ggplot(data_for_plot_2b,aes(x=year,y=value,fill=key))+
                  geom_bar(stat="identity",position="dodge")+
                  labs(x = "Year",y = paste0("Water condition max"),fill="Parameters")+
                  theme_minimal()+
                  scale_x_continuous(breaks=unique(data_for_plot_2b$year),labels = unique(data_for_plot_2b$year))+
                  theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                        ,plot.background = element_rect(color="grey20",size=2)
                        ,legend.position = "right"
                  )
                print(p2)
                
              }) # renderPlot end
            }else{
              buttonState$btn2 <- FALSE
              shinyjs::hide(ns("IHA_plot_2_panel"), asis=TRUE)
            } # if else loop end
    }) #observeEvent to display IHA plot 2 end
   
     observeEvent(input$display_IHA_plot_3, {

       if(buttonState$btn3 == FALSE){
         
         shinyjs::show(ns("IHA_plot_3_panel"), asis=TRUE)
         buttonState$btn3 <- TRUE
         data_to_plot <- cbind(rownames(localResults$IHA.group.3),data.frame(localResults$IHA.group.3,row.names = NULL))
         colnames(data_to_plot)[1]<-'year'
         
         data_for_plot_3 <- data.frame(localResults$IHA.group.3,row.names = NULL)%>% rename("Year" = "year") %>% 
           pivot_longer(cols = -Year, names_to = "Parameters", values_to = "Values")
         
         output$IHA_plot_3 <- renderPlot({
           p1 <- ggplot(data_for_plot_3,aes(x=Year,y=Values,fill=Parameters))+
             geom_bar(stat="identity",position="dodge")+
             labs(x = "Year",y = paste0("Julian days"))+
             theme_minimal()+
             theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                   ,plot.background = element_rect(color="grey20",size=2)
                   ,legend.position = "right"
             )
           print(p1)
           
         }) # renderPlot end
       }else{
         buttonState$btn3 <- FALSE
         shinyjs::hide(ns("IHA_plot_3_panel"), asis=TRUE)
       } # if else loop end
     }) #observeEvent to display IHA plot 3 end
     
     observeEvent(input$display_IHA_plot_4, {

       if(buttonState$btn4 == FALSE){
         shinyjs::show(ns("IHA_plot_4_panel"), asis=TRUE)
         buttonState$btn4 <- TRUE
         data_to_plot <- cbind(rownames(localResults$IHA.group.4),data.frame(localResults$IHA.group.4,row.names = NULL))
         colnames(data_to_plot)[1]<-'year'
         column_names <- colnames(data_to_plot)
         number_cols_to_select <- c("year",column_names[str_detect(column_names,"number")])
         length_cols_to_select <- c("year",column_names[str_detect(column_names,"length")])
         data_to_plot_number <- data_to_plot[number_cols_to_select]
         data_to_plot_length <- data_to_plot[length_cols_to_select]
         data_for_plot_4a <- data_to_plot_number %>% gather(key,value,-year) ## convert into long format
         data_for_plot_4b <- data_to_plot_length %>% gather(key,value,-year) ## convert into long format
         data_for_plot_4a$year <- as.numeric(as.character(data_for_plot_4a$year))
         data_for_plot_4b$year <- as.numeric(as.character(data_for_plot_4b$year))
         
         data_for_plot_4a <- data.frame(localResults$IHA.group.4,row.names = NULL) %>% 
           select(year, Low.pulse.number, High.pulse.number) %>% 
           rename("Year" = "year", "High pulse number" = "High.pulse.number", "Low pulse number" = "Low.pulse.number") %>% 
           pivot_longer(-Year, names_to = "Parameters", values_to = "Values") %>% 
           mutate(Year = as.numeric(Year))
         data_for_plot_4b <- data.frame(localResults$IHA.group.4,row.names = NULL) %>% 
           select(year, Low.pulse.length, High.pulse.length) %>% 
           rename("Year" = "year", "High pulse length" = "High.pulse.length", "Low pulse length" = "Low.pulse.length") %>% 
           pivot_longer(-Year, names_to = "Parameters", values_to = "Values") %>% 
           mutate(Year = as.numeric(Year))
         
         output$IHA_plot_4a <- renderPlot({
           p1 <- ggplot(data_for_plot_4a,aes(x=Year,y=Values,fill=Parameters))+
             geom_bar(stat="identity",position="dodge")+
             labs(x = "Year",y = paste0("Frequency"),fill="Parameters")+
             theme_minimal()+
             theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                   ,plot.background = element_rect(color="grey20",size=2)
                   ,legend.position = "right"
             )
           print(p1)
           
         }) # renderPlot end
             output$IHA_plot_4b <- renderPlot({
               p2 <- ggplot(data_for_plot_4b,aes(x=Year,y=Values,fill=Parameters))+
                 geom_bar(stat="identity",position="dodge")+
                 labs(x = "Year",y = paste0("Duration"),fill="Parameters")+
                 theme_minimal()+
                 theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                       ,plot.background = element_rect(color="grey20",size=2)
                       ,legend.position = "right"
                 )
               print(p2)

             }) # renderPlot end
           }else{
             buttonState$btn4 <- FALSE
             shinyjs::hide(ns("IHA_plot_4_panel"), asis=TRUE)
           } # if else loop end
         }) #observeEvent to display IHA plot 4 end
         
     observeEvent(input$display_IHA_plot_5, {

       if(buttonState$btn5 == FALSE){
         
         shinyjs::show(ns("IHA_plot_5_panel"),asis=TRUE)
         data_to_plot <- cbind(rownames(localResults$IHA.group.5),data.frame(localResults$IHA.group.5,row.names = NULL))
         colnames(data_to_plot)[1]<-'Year'
         
         data_for_plot_5 <- data.frame(localResults$IHA.group.5,row.names = NULL) %>% select(-Reversals) %>% 
           rename("Year" = "year") %>% 
           pivot_longer(cols = -Year, names_to = "Parameters", values_to = "Values") %>% 
           mutate(Year = as.numeric(Year))
         
         data_to_plot <- data_to_plot[,1:3]
         data_for_plot_1 <- data_to_plot %>% gather(key,value,-Year) ## convert into long format
         data_for_plot_1$Year <- as.numeric(as.character(data_for_plot_1$Year))
         buttonState$btn5 <- TRUE
         
         output$IHA_plot_5 <- renderPlot({
           p1 <- ggplot(data_for_plot_5, aes(x=Year,y=Values,fill=Parameters))+
             geom_bar(stat = "identity", position = "dodge", size=0.8,linetype="dashed")+
             labs(x = "Year",y = paste0("Rate"))+
             theme_minimal()+
             theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                   ,plot.background = element_rect(color="grey20",size=2)
                   ,legend.position = "right"
             )
           print(p1)
           
         }) # renderPlot end
       }else{
         buttonState$btn5 <- FALSE
         shinyjs::hide(ns("IHA_plot_5_panel"),asis=TRUE)
       } # if else loop end
     }) #observeEvent to display IHA plot 5 end
          
  }) # end of module server
}
