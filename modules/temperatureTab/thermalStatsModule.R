#' Continuous Data Exploration / Temperature / Thermal Statistics (user interface side)
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
#' 
ThermalStatsModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("thermal_input_1")),
              uiOutput(ns("thermal_input_2")),
              uiOutput(ns("thermal_input_3")),
              uiOutput(ns("display_run_thermal_button")),
              hr(),
              uiOutput(ns("display_save_thermal_button")),
          )#end of panel body
      ) # end of panel
    ),
    mainPanel(
      width = 9,
      column(
        width = 12,
        uiOutput(ns("errorDiv")),
        shinydashboard::box(id=ns("display_help_text_thermal_statistics"), width=12, class="well",
                            h4("Temperature – Thermal Statistics"),
                            div(style="width:100%;", "The output from this Shiny app is derived from the StreamThermal R package on GitHub (",
                            a('https://github.com/tsangyp/StreamThermal', href='https://github.com/tsangyp/StreamThermal', target='_blank'),
                                                      " ). It generates over 200 different thermal metrics that cover five categories of stream thermal regimes: frequency, magnitude, rate of change, timing, and variability. Critical temperature refers to the cutoff temperature for the frequency calculation, which sums the number of observations greater than the cutoff."),
                            br(),
                            div(style = "width:100%;font-weight:bold;", "Citation:"),
                            div(style="width:100%;", "Tsang, Y.P., Infante, D., Stewart, J., Wang, L., Tingly, R., Thornbrugh, D., Cooper, A., Daniel, W. 2016. StreamThermal: A Software Package for Calculating Thermal Metrics from Stream Temperature Data. Fisheries. 41: 548-554.",
                                a('https://doi.org/10.1080/03632415.2016.1210517', href='https://doi.org/10.1080/03632415.2016.1210517', target='_blank')),

        ), # end of box
        
        DT::dataTableOutput(ns("display_thermal_table_1")),
        br(),
        DT::dataTableOutput(ns("display_thermal_table_2")),
        br(),
        DT::dataTableOutput(ns("display_thermal_table_3")),
        br(),
        DT::dataTableOutput(ns("display_thermal_table_4")),
        br(),
        DT::dataTableOutput(ns("display_thermal_table_5"))
      )
    ) # mainPanel end
  ) # sidebarLayout end

}

#' Continuous Data Exploration / Temperature / Thermal Statistics (server side)
#'
#' @param id 
#' @param uploaded_data 
#' @param formated_raw_data 
#' @param dailyStats 
#' @param loaded_data 
#' @param to_download 
#' @param renderThermalStats 
#'
ThermalStatsModuleServer <- function(id, uploaded_data, formated_raw_data, dailyStats, loaded_data, to_download, renderThermalStats) {
 
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveVal()
  
  moduleServer(
    id,
#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
    function(input, output, session) {
          ns <- session$ns
           observe({
            localStats <- dailyStats
            variables_avail <- names(uploaded_data())
            if(renderThermalStats$render == TRUE) {

              # TODO: if you keep this module, remove this because the user specifies SiteID name in the set up 
              output$thermal_input_1 <- renderUI({
                site_keys_in_favor_order <- c("Site","SITE","SiteID","SITEID")
                possible_site_columns <- site_keys_in_favor_order[site_keys_in_favor_order %in% variables_avail]
                if (length(possible_site_columns)==0){
                  site_to_select <- variables_avail[grep('site',variables_avail,ignore.case=TRUE)][1]
                }else{
                  site_to_select <- possible_site_columns[1]
                }
                selectizeInput(ns("thermal_SiteID_name"),label ="Select site identifier column",
                               choices=variables_avail,
                               multiple = FALSE,
                               selected=site_to_select,
                               options = list(hideSelected = FALSE))
              })
              
              output$thermal_input_2 <- renderUI({
                numericInput(ns("critical_temp"), label = "Select critical temperature (°C)", value = 16)
              })
              
              output$thermal_input_3 <- renderUI({
                temp_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C",
                                              "WATER_TEMP_C","Air.Temp.C","AIR.TEMP.C","Air_Temp_C","AIR_TEMP_C")
                possible_temp_columns <- temp_keys_in_favor_order[temp_keys_in_favor_order %in% variables_avail]
                if (length(possible_temp_columns)==0){
                  temp_to_select <- variables_avail[grep('temp',variables_avail,ignore.case=TRUE)][1]
                }else{
                  temp_to_select <- possible_temp_columns[1]
                }
                selectizeInput(ns("thermal_Temp_name"),label ="Select temperature column",
                               choices=variables_avail,
                               multiple = FALSE,
                               selected= c(temp_to_select),
                               options = list(hideSelected = FALSE))
              })
              
              output$display_run_thermal_button <- renderUI({
                actionButton(inputId=ns("display_thermal"), label="Display stream thermal metrics",class="btn btn-primary")
              })
              

              
              #Remvoing old way, it is a overkill, but keeping the code, do not know if there is a future plan to use the file.
              #shinyjs::show(id=ns("display_help_text_thermal_statistics"), asis=TRUE)
              
              # output$display_help_text_thermal_statistics <- renderUI({
              #   verbatimTextOutput(ns("help_text_thermal_statistics"))
              # })
              # 
              # output$help_text_thermal_statistics <- renderText({
              #   filePath <- "help_text_files/Temperature_ThermalStatistics.txt"
              #   fileText <- paste(readLines(filePath,encoding="UTF-8"),collapse="\n")
              #   fileText
              # })
              
            }
          })
           
            observeEvent(input$thermal_Temp_name, {
              shinyjs::hide(ns('save_thermal'), asis=TRUE);
            })
            
            observeEvent(input$display_thermal, {
                        #remove previous error messages if any
                        output$errorDiv <- renderUI({})
                        
                        localStats <- dailyStats
                        #shinyjs::hide(id=ns("display_help_text_thermal_statistics"), asis=TRUE)
                        temp_data <- localStats$processed_dailyStats[[input$thermal_Temp_name]] %>% 
                          dplyr::select("SiteID", 
                                        "Date", 
                                        "MaxT" = paste0(input$thermal_Temp_name,".max"),
                                        "MinT" = paste0(input$thermal_Temp_name,".min"), 
                                        "MeanT" = paste0(input$thermal_Temp_name,".mean")) %>% 
                          mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
                          as.data.frame()
                        
                        tryCatch({
                        # streamThermal_exported <- Export.StreamThermal(formated_raw_data$derivedDF
                        #                                                ,fun.col.SiteID = input$thermal_SiteID_name
                        #                                                ,fun.col.Date = "date.formatted"
                        #                                                ,fun.col.Temp = input$thermal_Temp_name
                        # )
                        # print("passed Export.StreamThermal")
                          #]browser()
                        
                        ##save(streamThermal_exported, file="test_streamThermal_exported.RData")
                        ST.freq <- T_frequency(temp_data, cT = input$critical_temp) %>% mutate_if(is.numeric,round,digits=2)
                        ST.mag  <- T_magnitude(temp_data) %>% mutate_if(is.numeric,round,digits=2)
                        ST.roc  <- T_rateofchange(temp_data) %>% mutate_if(is.numeric,round,digits=2)
                        ST.tim  <- T_timing(temp_data) %>% mutate_if(is.numeric,round,digits=2)
                        ST.var  <- T_variability(temp_data) %>% mutate_if(is.numeric,round,digits=2)
                        
                        # ST.freq <- T_frequency(streamThermal_exported, cT = input$critical_temp) %>% mutate_if(is.numeric,round,digits=2)
                        # ST.mag  <- T_magnitude(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
                        # ST.roc  <- T_rateofchange(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
                        # ST.tim  <- T_timing(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
                        # ST.var  <- T_variability(streamThermal_exported) %>% mutate_if(is.numeric,round,digits=2)
                        
                        
                        localStats$ST.freq <- ST.freq
                        localStats$ST.mag <- ST.mag
                        localStats$ST.roc <- ST.roc
                        localStats$ST.tim <- ST.tim
                        localStats$ST.var <- ST.var
                        
          
                        renderTables <- function(id, data, tblTitle, options) {
                          
                          thermal.statistics.table.options <- list(
                            scrollX = TRUE, #allow user to scroll wide tables horizontally
                            stateSave = FALSE,
                            pageLength = 15,
                            dom = 'Bt',
                            
                            
                            buttons = list(
                              list(extend='copy', text='Copy', className="btn btn-primary"),
                              list(extend='print', text='Print', className="btn btn-primary"),
                              list(extend='collection', buttons = 
                                     list(
                                       list(extend = "csv", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                              input$thermal_Temp_name,"_thermal-statistics_",
                                                                              tblTitle)),
                                       list(extend = "excel", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                                input$thermal_Temp_name,"_thermal-statistics_",
                                                                                tblTitle)),
                                       list(extend = "pdf", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_", 
                                                                              input$input$thermal_Temp_name,"_thermal-statistics_",
                                                                              tblTitle))
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
                          
                          output[[id]] <- DT::renderDataTable({
                            table.title.1 <- tblTitle
                            myTable <- DT::datatable(
                              data,
                              caption = htmltools::tags$caption(table.title.1,style="color:black;font-size:16px;font-weight:bold;text-align:left;"),
                              extensions ="Buttons",
                              rownames = TRUE,
                              options = thermal.statistics.table.options
                            ) # dataTable end
                            print(myTable)
                          })  # renderDT end
                          
                        }
                        renderTables(id="display_thermal_table_1", data=ST.freq, tblTitle="Frequency")
                        renderTables(id="display_thermal_table_2", data=ST.mag, tblTitle="Magnitude") 
                        renderTables(id="display_thermal_table_3", data=ST.roc, tblTitle="Rate of Change") 
                        renderTables(id="display_thermal_table_4", data=ST.tim, tblTitle="Timing") 
                        renderTables(id="display_thermal_table_5", data=ST.var, tblTitle="Variability") 
                        
                        runjs(sprintf('document.getElementById("%s").scrollIntoView({ behavior: "smooth" });', ns("display_thermal_table_1")))
          
                        require(openxlsx)

                        #Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var descriptions are defined in the constants.R file
                        Group.Desc <- c(Desc.freq, Desc.mag, Desc.roc, Desc.tim, Desc.var)
                        #df.Groups <- as.data.frame(cbind(c("freq","mag","roc","tim","var"),Group.Desc))
                        df.Groups <- data.frame(Table = c("freq","mag","roc","tim","var"), Description = Group.Desc) # LCN changed so renders with meaningful column names
                        SiteID <- localStats$ST.freq[1,1]
                        myDate <- format(Sys.Date(),"%Y%m%d")
                        myTime <- format(Sys.time(),"%H%M%S")

                        Notes.User <- Sys.getenv("USERNAME")
                        Notes.Names <- c("Dataset (SiteID)", "Analysis.Date (YYYYMMDD)"
                                         , "Analysis.Time (HHMMSS)", "Analysis.User")
                        Notes.Data <- c(SiteID, myDate, myTime, Notes.User)
                        df.Notes <- as.data.frame(cbind(Notes.Names, Notes.Data))
                        ## New File Name
                        fileName <- paste("StreamThermal"
                                          , str_remove(loaded_data$name, ".csv|.xlsx")
                                          , SiteID
                                          , myDate
                                          , "xlsx"
                                          , sep=".")
                        ## Copy over template with Metric Definitions
                        file.copy(file.path(path.package("ContDataQC")
                                            ,"extdata"
                                            ,"StreamThermal_MetricList.xlsx")
                                  , file.path(tempdir(),fileName))

                        hs <- createStyle(fgFill = "#D9D9D9", textDecoration = "bold")
                        
                        wb <- loadWorkbook(file.path(tempdir(),fileName))
                        
                        addWorksheet(wb, "AnalysisNotes")
                        addWorksheet(wb, "TableDescriptions")
                        addWorksheet(wb, "freq")
                        addWorksheet(wb, "mag")
                        addWorksheet(wb, "roc")
                        addWorksheet(wb, "tim")
                        addWorksheet(wb, "var")
                        
                        writeData(wb, "AnalysisNotes", df.Notes, headerStyle = hs)
                        writeData(wb, "TableDescriptions", df.Groups, headerStyle = hs)
                        writeData(wb, "freq", localStats$ST.freq, headerStyle = hs)
                        writeData(wb, "mag", localStats$ST.mag, headerStyle = hs)
                        writeData(wb, "roc", localStats$ST.roc, headerStyle = hs)
                        writeData(wb, "tim", localStats$ST.tim, headerStyle = hs)
                        writeData(wb, "var", localStats$ST.var, headerStyle = hs)
                        
                        # save workbook
                        to_download$wb <- wb
                        to_download$fileName <- fileName
                        
                        output$display_save_thermal_button <- renderUI({
                          downloadButton(outputId=ns("save_thermal"), label="Save thermal statistics to excel",class="btn btn-primary")
                        })
              
              
              }, error=function(e){
                errorMsg <- print(paste0("Error in thermalClassification", e$message))
                if(e$message == "'x' must be numeric") {
                  errorMsg[1] <- "Selected temperature column must have numeric values."
                } else {
                  errorMsg[1] <- e$message
                }
                print(paste0("Error in thermalStatsModule", e$message))
                output$errorDiv <- renderUI({
                  div(h4(errorMsg), class="alert alert-danger")
                })
                
              })
              
          }) #observeEvent end
            
              output$save_thermal <- downloadHandler(
              filename = function(){
                to_download$fileName
              },
              content = function(file){
                saveWorkbook(to_download$wb,file)
   
              }
            ) # downloadHandler close
            
  
          
    }) # end of module server
}
