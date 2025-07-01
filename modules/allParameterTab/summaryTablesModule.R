#' Continuous Data Exploration / All Parameter / Summary Tables (user interface side)
#'
#' @param id 
#'
SummaryTablesModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # EWL, Start
          div(class = "panel panel-default",width = "100%",style="margin:10px;",
              # div(class = "panel-heading",
              #     p("You must have completed step 1 to step 4 to use Summary Tables", style='font-weight:bold;font-family: Helvetica Neue, Helvetica, Arial, sans-serif;')
              # ),
              div(class="panel-body",
                  uiOutput(ns("summary_table_input_1")),
                  uiOutput(ns("summary_table_input_2")),
                  uiOutput(ns("summary_table_input_3")),
                  hr(),
                  uiOutput(ns("summary_table_input_4")),
              )
          ),
        ),
        mainPanel(
          width = 9,
                 shinydashboard::box(id=ns("summary_tab_help"), width=12, class="well",
                                     h4("Any parameters – Summary tables"),
                                     div(style="width:100%;", "Calculates the mean of user-selected daily summary statistics (e.g., mean, median, standard deviation) over user-selected time periods (year/month, year, year/season, season) for any processed continuous parameter. Seasons are defined as: Winter (December, January, February), Spring (March, April, May), Summer (June, July, August), Fall (September, October, November).")), 
                 
                 DT::dataTableOutput(ns("display_summary_table_1")),
          br(),
          br(),
          DT::dataTableOutput(ns("display_summary_table_2"))
        ) # mainPanel end
      ) # sidebarLayout end
}




#' Continuous Data Exploration / All Parameter / Summary Tables (server side)
#'
#' @param id 
#' @param dailyStats 
#' @param renderSummaryTables 
#'
SummaryTablesModuleServer <- function(id, dailyStats, renderSummaryTables, loaded_data) { # added loaded_data

  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveValues(params=list())

  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
          observe({
            localStats <- dailyStats
            variables_avail$params <- names(localStats$processed_dailyStats)
            localStats$stats <- localStats$processed_dailyStats
            #print(localStats$stats)
          })

          observe({
            if(renderSummaryTables$render == TRUE) {
                output$summary_table_input_1 <- renderUI({
                  selectizeInput(ns("summarise_variable_name"),
                                 label ="Select variable name",
                                 choices=variables_avail$params,
                                 multiple = FALSE,
                                 selected=variables_avail$params[1],
                                 options = list(hideSelected = FALSE))
                })

                output$summary_table_input_2 <- renderUI({
                  radioButtons(ns("summarise_by"), "Summarize by", choices = c("year/month"="year/month"
                                                                           ,"year"="year"
                                                                           ,"year/season"="year/season"
                                                                           ,"season"="season"),
                               selected = "year/month")
                })

                output$summary_table_input_3 <- renderUI({
                  selectizeInput(ns("summarise_metrics"),label ="Select metric",
                                 choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                                 multiple = FALSE,
                                 selected="mean",
                                 options = list(hideSelected = FALSE))
                })

                output$summary_table_input_4 <- renderUI({
                  actionButton(inputId=ns("display_table"), label="Display",class="btn btn-primary")
                })
              }
          })

          observeEvent(input$display_table, {
            localStats <- dailyStats
            variables_avail$params <- names(localStats$processed_dailyStats)
            # output$display_summary_table_1 <- renderUI({
            #   withSpinner(dataTableOutput("summary_table_1"))
            # })
            #myList <- processed$processed_dailyStats
            myList <- localStats$processed_dailyStats
            variable_to_summarise <- input$summarise_variable_name
            myData <- myList[[which(names(myList)==variable_to_summarise)]]
            summary_df <- mySummarisemore(df=myData,variable=input$summarise_variable_name,metrics=input$summarise_metrics,timeframe=input$summarise_by)
            n_df <- mySummarisen(df=myData,variable=input$summarise_variable_name,metrics=input$summarise_metrics,timeframe=input$summarise_by)
            
            table_title <- paste0(input$summarise_variable_name," ",input$summarise_metrics)

            if(!is.null(summary_df)) {
            output$display_summary_table_1 <- DT::renderDataTable({
              #print("inside renderDT now...")
              myTable <- DT::datatable(
                summary_df,
                caption = htmltools::tags$caption(table_title,style="color:black;font-size:16px;font-weight:bold;text-align:center;"),
                extensions ="Buttons",
                rownames = FALSE,
                options = list(
                  scrollX = TRUE, #allow user to scroll wide tables horizontally
                  stateSave = FALSE,
                  pageLength = 15,
                  dom = 'Bt',
                  buttons = list(
                    list(extend='copy', text='Copy', className="btn btn-primary"),
                    list(extend='print', text='Print', className="btn btn-primary"),
                    #list(extend='collection', buttons = c('csv','excel','pdf'), text='Download', className="btn btn-primary")
                    list(extend='collection', buttons = 
                           list(
                             list(extend = "csv", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                    isolate(input$summarise_variable_name),"_summary_",
                                                                    isolate(input$summarise_by), "_",
                                                                    isolate(input$summarise_metrics))),
                             list(extend = "excel", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                      isolate(input$summarise_variable_name),"_summary_",
                                                                      isolate(input$summarise_by), "_",
                                                                      isolate(input$summarise_metrics))),
                             list(extend = "pdf", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                               isolate(input$summarise_variable_name),"_summary_",
                                                                               isolate(input$summarise_by), "_",
                                                                               isolate(input$summarise_metrics)))
                           ),
                         
                         text='Download', className="btn btn-primary")
                  ),
                  columnDefs = list(list(className="dt-center",targets="_all")),
                  initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#e3e3e3', 'color': '#000'});",
                    "$('.dt-buttons button').removeClass('dt-button');",
                    "}")
                )
              ) # dataTable end
                #saveToReport$summaryTable <- myTable
                print(myTable)
              })  # renderDT end
            }
            #runjs(sprintf('document.getElementById("%s").scrollIntoView({ behavior: "smooth" });', ns("display_summary_table_1")))

            
            if(!is.null(n_df)) {
              output$display_summary_table_2 <- DT::renderDataTable({
                #print("inside renderDT now...")
                myTable <- DT::datatable(
                  n_df,
                  caption = htmltools::tags$caption("Days of available data",style="color:black;font-size:16px;font-weight:bold;text-align:center;"),
                  extensions ="Buttons",
                  rownames = FALSE,
                  options = list(
                    scrollX = TRUE, #allow user to scroll wide tables horizontally
                    stateSave = FALSE,
                    pageLength = 15,
                    dom = 'Bt',
                    buttons = list(
                      list(extend='copy', text='Copy', className="btn btn-primary"),
                      list(extend='print', text='Print', className="btn btn-primary"),
                      #list(extend='collection', buttons = c('csv','excel','pdf'), text='Download', className="btn btn-primary")
                      list(extend='collection', buttons = 
                             list(
                               list(extend = "csv", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                      isolate(input$summarise_variable_name),"_n-days_",
                                                                      isolate(input$summarise_by), "_",
                                                                      isolate(input$summarise_metrics))),
                               list(extend = "excel", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                        isolate(input$summarise_variable_name),"_n-days_",
                                                                        isolate(input$summarise_by), "_",
                                                                        isolate(input$summarise_metrics))),
                               list(extend = "pdf", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                      isolate(input$summarise_variable_name),"_n-days_",
                                                                      isolate(input$summarise_by), "_",
                                                                      isolate(input$summarise_metrics)))
                             ),
                           
                           text='Download', className="btn btn-primary")
                    ),
                    columnDefs = list(list(className="dt-center",targets="_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#e3e3e3', 'color': '#000'});",
                      "$('.dt-buttons button').removeClass('dt-button');",
                      "}")
                  )
                ) # dataTable end
                #saveToReport$summaryTable <- myTable
                print(myTable)
              })  # renderDT end
            }
            
          }) # observeEvent end


          mySummarisemore <- function(df=myDf, variable=myVariable,metrics=myMetrics,timeframe=myTimeframe){
            tryCatch({
            variable_col_name <- paste0(variable,".",metrics)
            df <- df %>% mutate(Year = lubridate::year(Date),
                                 Month = lubridate::month(Date),
                                 Season = case_when(
                                   Month %in% c(12,1,2) ~ "Winter",
                                   Month %in% c(3,4,5) ~ "Spring",
                                   Month %in% c(6,7,8) ~ "Summer", 
                                   Month %in% c(9,10,11) ~ "Fall"
                                 ))
            
            if (timeframe == "year/month"){
              df.ret1 <- df %>% group_by(Year, Month) %>% 
                summarize(mean_val = mean(!!sym(variable_col_name), na.rm = TRUE) %>% round(2)) %>% 
                ungroup() %>% 
                pivot_wider(id_cols = Month, names_from = Year, values_from = mean_val) %>% 
                mutate(across(everything(), ~ replace(., is.infinite(.), NA)),
                       across(everything(), ~ replace(., is.nan(.), NA))) 
              
              df.ret2 <- df %>% group_by(Month) %>% 
                summarize(mean_val = mean(!!sym(variable_col_name), na.rm = TRUE) %>% round(2)) %>% 
                rename("Overall" = mean_val)
              
              df.ret1 <- df.ret1 %>% full_join(df.ret2, by = "Month")
            }
            if(timeframe == "year"){
              df.ret1 <- df %>% group_by(Year) %>% 
                summarize(mean_val = mean(!!sym(variable_col_name), na.rm = TRUE) %>% round(2)) %>% 
                pivot_wider(names_from = Year, values_from = mean_val)
              
              df.ret1$Overall <- df %>% 
                summarize(Overall = mean(!!sym(variable_col_name), na.rm = TRUE) %>% round(2)) %>% 
                pull(Overall)
            }
            if(timeframe == "year/season"){
              df.ret1 <- df %>% group_by(Year, Season) %>% 
                summarize(mean_val = mean(!!sym(variable_col_name), na.rm = TRUE) %>% round(2)) %>% 
                ungroup() %>% 
                pivot_wider(id_cols = Season, names_from = Year, values_from = mean_val) %>% 
                mutate(across(everything(), ~ replace(., is.infinite(.), NA)),
                       across(everything(), ~ replace(., is.nan(.), NA))) 
              
              df.ret2 <- df %>% group_by(Season) %>% 
                summarize(mean_val = mean(!!sym(variable_col_name), na.rm = TRUE) %>% round(2)) %>% 
                rename("Overall" = mean_val)
              
              df.ret1 <- df.ret1 %>% full_join(df.ret2, by = "Season")
            }
            if(timeframe == "season"){
              df.ret1 <- df %>% group_by(Season) %>% 
                summarize(mean_val = mean(!!sym(variable_col_name), na.rm = TRUE) %>% round(2)) %>% 
                pivot_wider(names_from = Season, values_from = mean_val)
              
              df.ret1$Overall <- df %>% 
                summarize(Overall = mean(!!sym(variable_col_name), na.rm = TRUE) %>% round(2)) %>% 
                pull(Overall)
            }
            return(df.ret1)
            }, error=function(err){
              print("Error in mySummarisemore2")
              return(NULL)
            })
          }
          
          mySummarisen <- function(df=myDf, variable=myVariable,metrics=myMetrics,timeframe=myTimeframe){
            tryCatch({
              variable_col_name <- paste0(variable,".",metrics)
              df <- df %>% mutate(Year = lubridate::year(Date),
                                  Month = lubridate::month(Date),
                                  Season = case_when(
                                    Month %in% c(12,1,2) ~ "Winter",
                                    Month %in% c(3,4,5) ~ "Spring",
                                    Month %in% c(6,7,8) ~ "Summer", 
                                    Month %in% c(9,10,11) ~ "Fall"
                                  ))
              
              if (timeframe == "year/month"){
                df.ret1 <- df %>% group_by(Year, Month) %>% 
                  summarize(num_days = sum(!is.na(!!sym(variable_col_name)))) %>% 
                  ungroup() %>% 
                  pivot_wider(id_cols = Month, names_from = Year, values_from = num_days) 
                
                df.ret2 <- df %>% group_by(Month) %>% 
                  summarize(num_days = sum(!is.na(!!sym(variable_col_name)))) %>% 
                  rename("Overall" = num_days)
                
                df.ret1 <- df.ret1 %>% full_join(df.ret2, by = "Month")
              }
              if(timeframe == "year"){
                df.ret1 <- df %>% group_by(Year) %>% 
                  summarize(num_days = sum(!is.na(!!sym(variable_col_name)))) %>% 
                  pivot_wider(names_from = Year, values_from = num_days)
                
                df.ret1$Overall <- df %>% 
                  summarize(Overall = sum(!is.na(!!sym(variable_col_name)))) %>% 
                  pull(Overall)
              }
              if(timeframe == "year/season"){
                df.ret1 <- df %>% group_by(Year, Season) %>% 
                  summarize(num_days = sum(!is.na(!!sym(variable_col_name)))) %>% 
                  ungroup() %>% 
                  pivot_wider(id_cols = Season, names_from = Year, values_from = num_days) %>% 
                  mutate(across(everything(), ~ replace(., is.infinite(.), NA)),
                         across(everything(), ~ replace(., is.nan(.), NA))) 
                
                df.ret2 <- df %>% group_by(Season) %>% 
                  summarize(num_days = sum(!is.na(!!sym(variable_col_name)))) %>% 
                  rename("Overall" = num_days)
                
                df.ret1 <- df.ret1 %>% full_join(df.ret2, by = "Season")
              }
              if(timeframe == "season"){
                df.ret1 <- df %>% group_by(Season) %>% 
                  summarize(num_days = sum(!is.na(!!sym(variable_col_name)))) %>% 
                  pivot_wider(names_from = Season, values_from = num_days)
                
                df.ret1$Overall <- df %>% 
                  summarize(Overall = sum(!is.na(!!sym(variable_col_name)))) %>% 
                  pull(Overall)
              }
              return(df.ret1)
            }, error=function(err){
              print("Error in mySummarisen")
              return(NULL)
            })
          }
          
          # mySummarisemore <- function(df=myDf, variable=myVariable,metrics=myMetrics,timeframe=myTimeframe){
          #   tryCatch({
          #       noResult <- NULL
          #       variable_col_name <- paste0(variable,".",metrics)
          #       names(df)[match(variable_col_name,names(df))] <- "x"
          #       if (timeframe == "year/month"){
          #         ## summarise by each year&month first
          #         df[,"yearmonth"] <- format(df[,"Date"],"%Y%m")
          #         # df<- df %>% mutate(yearmonth = paste0(lubridate::year(Date), 
          #         #                        if_else(lubridate::month(Date) %>% nchar()== 1, 
          #         #                                paste0("0", lubridate::month(df$Date)),
          #         #                                lubridate::month(Date) %>% as.character())))
          #         df.summary <- doBy::summaryBy(x~yearmonth,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
          #         df.summary[,"year"] <- substr(df.summary[,"yearmonth"],1,4)
          #         df.summary[,"month"] <- substr(df.summary[,"yearmonth"],5,6)
          #         df.summary <- df.summary[2:4]
          #         df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
          #         df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
          #         ## summarise by each month regardless of the year to get overall mean for each month
          #         df[,"month"] <- format(df[,"Date"],"%m")
          #         df.summary.overall <- doBy::summaryBy(x~month,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
          #         df.summary.overall[,"x.mean"] <- formatC(df.summary.overall[,"x.mean"],digits=2,format="f")
          #         names(df.summary.overall)[2] <- "Overall"
          #         df.summary.all <- merge(df.summary.wide,df.summary.overall,by="month")
          #         return(df.summary.all)
          #       }else if(timeframe =="year"){
          #         df[,"year"] <- format(df[,"Date"],"%Y")
          #         df.summary <- doBy::summaryBy(x~year,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
          #         df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
          #         df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
          #         df.summary.wide[,"Overall"] <- formatC(mean(df$x,na.rm=TRUE),digits=2,format="f")
          #         return(df.summary.wide)
          #       }else if(timeframe == "year/season"){
          #         df <- addSeason(df)
          #         df.summary <- doBy::summaryBy(x~yearseason,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
          #         df.summary[,"year"] <- substr(df.summary[,"yearseason"],1,4)
          #         df.summary[,"season"] <- substr(df.summary[,"yearseason"],5,nchar(df.summary[,"yearseason"]))
          #         df.summary <- df.summary[2:4]
          #         df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
          #         df.summary.wide <- pivot_wider(df.summary,names_from=year,values_from=x.mean)
          #         df.summary.overall <- doBy::summaryBy(x~season,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
          #         df.summary.overall[,"x.mean"] <- formatC(df.summary.overall[,"x.mean"],digits=2,format="f")
          #         names(df.summary.overall)[2] <- "Overall"
          #         df.summary.all <- merge(df.summary.wide,df.summary.overall,by="season")
          #         return(df.summary.all)
          # 
          #       }else if(timeframe =="season"){
          #         df <- addSeason(df)
          #         df.summary <- doBy::summaryBy(x~season,data=df,FUN=mean,na.rm=TRUE,variable.names=variable)
          #         df.summary[,"x.mean"] <- formatC(df.summary[,"x.mean"],digits=2,format="f")
          #         df.summary.wide <- pivot_wider(df.summary,names_from=season,values_from=x.mean)
          #         df.summary.wide[,"Overall"] <- formatC(mean(df$x,na.rm=TRUE),digits=2,format="f")
          #         return(df.summary.wide)
          #       }else{
          #         stop("please specify one of the following summarise timeframes:
          #                'year/month','year','year/season','season'")
          #       }
          #   }, error=function(err) {
          #     print("error in mySummarisemore function")
          #     return(noResult)
          #   })
          # }

    }) # end of module server
}
