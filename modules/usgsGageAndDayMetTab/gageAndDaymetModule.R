#'USGS & Daymet exploration tab (user interface)
#'
#' @param id 
#'
GageAndDaymetModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(width = 3, uiOutput(ns("gage_panel")), uiOutput(ns("daymet_panel")), uiOutput(ns(
      "base_gage_daymet_panel"
    ))),
    mainPanel(width = 9, column(
      width = 12,
      fluidRow(
        shinydashboard::box(
          id = ns("gage_daymet_help"),
          width = 12,
          class = "well",
          h4("USGS & Daymet Exploration"),
          div(
            style = "width:100%;",
            "Contextualize uploaded data with relevant, high-frequency USGS stream gage and Daymet daily weather and climate data."
          ),
          br(),
          div(
            style = "width: 100%",
            "USGS gage data available (depending on the selected gage): discharge, water level, air temperature, water temperature, pH, precipitation, air pressure, and water pressure"
          ),
          div(
            style = "width:100%",
            "Daymet data available: precipitation, shortwave radiation, snow water equivalent, maximum air temperature, minimum air temperature, and water vapor pressure"
          ),
          br(),
          div(
            style = "width:100%",
            "The date ranges auto-populate with the date range of user-uploaded data, but can be modified. Select the Import data buttons to download data and the View data buttons to view time series of the data."
          ),
          br(),
          div(
            style = "width:100%",
            "USGS data is downloaded using the dataRetrieval package. For more information visit ",
            a(
              'https://doi-usgs.github.io/dataRetrieval/',
              href = 'https://doi-usgs.github.io/dataRetrieval/',
              target = '_blank'
            )
          ),
          div(
            style = "width:100%",
            "Daymet data is downloaded using the daymetr package. For more information visit ",
            a(
              'https://cran.r-project.org/web/packages/daymetr/index.html',
              href = 'https://cran.r-project.org/web/packages/daymetr/index.html',
              target = '_blank'
            )
          )
        ) # end box
      ),
      fluidRow(div(style = "width:100%", uiOutput(
        ns("gageDayMetError")
      ))),
      fluidRow(div(style = "height:1500px;overflow-y:auto", withSpinner(plotlyOutput(
        ns("display_downloaded_data")
      )), type = 1))
    )) # mainPanel end
  ) # sidebarLayout end
}

#'USGS & Daymet exploration tab (server side)
#'
#' @param id 
#' @param homeDTvalues 
#' @param dateRange 
#' @param formated_raw_data 
#' @param renderUsgsAndDaymet 
#'
GageAndDaymetModuleServer <- function(id, homeDTvalues, dateRange, formated_raw_data, renderUsgsAndDaymet, gageRawData, dayMetRawData) {
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      gageColNames  <- NULL
      daymetCols <- NULL
      consoleUSGS <- NULL
      # gageRawData <- reactiveValues(gagedata = data.frame(),
      #                               gageColName = as.character())
      # dayMetRawData <- reactiveValues(dayMetData = data.frame(),
      #                                 daymetColumns = as.character())
      # 
      output$display_downloaded_data <- renderPlotly({
        plotly_empty()
      })

      # if(dateRange$min %>% lubridate::year() < 1980 | dateRange$max %>% lubridate::year() > 2023){
      #   output$daymet_range_warning <- renderText({paste0("Uploaded data is outside of the range of available Daymet data (1980-", as.numeric(format(Sys.time(), "%Y")) - 2, ")")})
      # }
      
      observe({
        if(renderUsgsAndDaymet$render == TRUE) {
          
          if(is.null(dateRange$min)){
            min_ymd <- ""
            max_ymd <- ""
            
            min_y <- ""
            max_y <- ""
            
          } else{
            if(dateRange$min %>% lubridate::year() < 1980 | dateRange$max %>% lubridate::year() > 2023){
              output$daymet_range_warning <- renderText({paste0("Uploaded data is outside of the range of available Daymet data (1980-", as.numeric(format(Sys.time(), "%Y")) - 2, ")")})
            }
            min_ymd <- dateRange$min %>% as.character()
            max_ymd <- dateRange$max %>% as.character()
            
            min_y <- dateRange$min %>% lubridate::year() %>% as.numeric()
            max_y <- dateRange$max %>% lubridate::year() %>% as.numeric()
          }
          
          # div(class="panel-heading", "Step 2: Select date and time format", style="font-weight:bold;",
          #     icon("info-circle", style = "color:#2fa4e7", id="datetimeHelp"),
          #     bsPopover(id="datetimeHelp", title=HTML("<b>Helpful Hints</b>"), content = HTML("Lorem ipsum"),
          #               placement = "right", trigger = "hover")),
          
          output$gage_panel <- renderUI({
            div(
              class = "panel panel-default",
              style = "padding:10px;margin-top:20px;",
              div(
                class = "panel-heading",
                style = "padding:10px 5px 10px 10px;",
                span("USGS gage data", style = "font-weight:bold;"),
                a(
                  "View gage IDs",
                  href = "https://dashboard.waterdata.usgs.gov/app/nwd/en/?region=lower48",
                  target = "_blank",
                  style = "float:right"
                ),
                icon("info-circle", style = "color:#2fa4e7", id = "usgsHelp"),
                bsPopover(
                  id = "usgsHelp",
                  title = HTML("<b>Helpful Hints</b>"),
                  content = HTML(
                    "Relevant USGS gages for the uploaded site can be found using the National Water Dashboard linked under View gage IDs to the right. For the provided test data from Posey Creek (38.89431, -78.147258) near Front Royal, VA, the nearby gage 01631000 on the South Fork of the Shenandoah River may provide relevant context data from a much larger stream."
                  ),
                  placement = "right",
                  trigger = "hover"
                )
              ),
              div(
                style = "padding:5px;",
                textInput(
                  inputId = ns("gage_id"),
                  label = "Gage ID",
                  value = ""
                ),
                div(div(
                  dateInput(
                    ns("gage_date_start"),
                    "Date start",
                    value = min_ymd,
                    min = "1980-01-01",
                    max = "2100-01-01",
                    format = "yyyy-mm-dd"
                  )
                ), div(
                  dateInput(
                    ns("gage_date_end"),
                    "Date end",
                    value = max_ymd,
                    min = "1980-01-01",
                    max = "2100-01-01",
                    format = "yyyy-mm-dd"
                  )
                )),
                actionButton(
                  inputId = ns("display_gage_ts"),
                  label = "Import USGS gage data",
                  class = "btn btn-primary"
                )
              ),
                div(id=ns("gageVarsDiv") , style="padding:5px;display:none",
                    selectizeInput(ns("gaze_params"), label ="Select USGS gage variables",
                                   choices=gageColNames,
                                   multiple = TRUE,
                                   selected=NULL,
                                   options = list(hideSelected = FALSE,
                                                  plugins = list("remove_button"))
                                   ),
                    actionButton(inputId=ns("display_gage_raw"), label="View USGS gage time series",class="btn btn-primary")
                ),
              div(id = ns("gageDownloadDiv"), style = "padding:5px;display:none",
                  downloadButton(ns("download_gage_raw"), "Download imported USGS gage data", class="btn btn-primary"))
            )
          })
          
          # END OF USGS gage
          #Daymet
          output$daymet_panel <- renderUI({
            div(class="panel panel-default", style="padding:10px;",
                div(class = "panel-heading", style="padding:10px 5px 10px 10px;",
                    span("DayMet data", style="font-weight:bold;"),
                    icon("info-circle", style = "color:#2fa4e7", id="daymetHelp"),
                    bsPopover(id="daymetHelp", title=HTML("<b>Helpful Hints</b>"), content = HTML("Provide the latitude and longitude in decimal degrees of the site from which the uploaded data were collected. This value cannot at this time be pulled from the uploaded data file. The coordinates of the site in the test data, Posey Creek, are 38.89431, -78.147258."),
                              placement = "right", trigger = "hover")),
                div(style="padding:5px;",
                    textInput(inputId=ns("daymet_lat"), label="Site latitude (decimal degrees)",value=""),
                    textInput(inputId=ns("daymet_long"), label="Site longitude (decimal degrees)",value=""),
                    div(div(selectInput(ns("daymet_date_start"),"Date start",selected = min_y, choices = 1980:(as.numeric(format(Sys.time(), "%Y")) - 2))),
                        div(selectInput(ns("daymet_date_end"),"Date end",selected = max_y, choices = 1980:(as.numeric(format(Sys.time(), "%Y")) - 2)))),
                    actionButton(inputId=ns("get_daymet_data"), label="Import Daymet data",class="btn btn-primary"),
                    div(textOutput(ns("daymet_range_warning")), style = "color:red;")
                ),
                div(id=ns("daymetVarsDiv"), style="padding:5px;display:none",
                    selectizeInput(ns("daymet_params"), label ="Select Daymet variables",
                                   choices=daymetCols,
                                   multiple = TRUE,
                                   selected=daymetCols[1],
                                   options = list(hideSelected = FALSE,
                                                  plugins = list("remove_button"))),
                    actionButton(inputId=ns("display_daymet_raw"), label="View Daymet raw data",class="btn btn-primary")
                ),
                div(id = ns("daymetDownloadDiv"), style = "padding:5px;display:none",
                    downloadButton(ns("download_daymet_raw"), "Download imported Daymet data", class="btn btn-primary"))
            ) #end of parent div
          })
          #end of Daymet

     if(is.null(dateRange$min)){
       variables_avail <- c()
     } else{
       variables_avail <- homeDTvalues$homeDateAndTime$parmToProcess()
     }
    
          output$base_gage_daymet_panel <- renderUI({
            
            div(class="panel panel-default", style="padding:10px;",
                div(class = "panel-heading", style="padding:10px 5px 10px 10px;",
                    span("View Base, Gage and DayMet data merged in a subplot", style="font-weight:bold;", icon("info-circle", style = "color: #2fa4e7", id=ns("baseDataDef")))
                ),
                bsPopover(id=ns("baseDataDef"), title="What are base data\\?", content = "Base data are the data uploaded on the \\'Upload Data\\' tab.", 
                          placement = "right", trigger = "hover"),
                div(style="padding:5px;",
                    selectizeInput(ns("dailyStats_ts_variable_name2"),label ="Select base variable names",
                                   choices=variables_avail,
                                   multiple = TRUE,
                                   selected=variables_avail[1],
                                   options = list(hideSelected = FALSE,
                                                  plugins = list("remove_button"))),
                    actionButton(inputId=ns("display_subplot_ts"), label="View merged data",class="btn btn-primary"),
                    div(id = ns("mergedDownloadDiv"), style = "display:none;margin-top:10px;",
                        downloadButton(ns("download_merged"), "Download merged data", class="btn btn-primary"))
                )
            )
          })
          
        } # end of render if
      })
      
      ########USGS Gage########
      #Download USGS gage data
      observeEvent(input$display_gage_ts, {
        clearContents()
        if(input$gage_id != "" && length(input$gage_id) > 0) {
          #data <- uploaded_data()
          consoleUSGS$disp <- data.frame(consoleOutputUSGS = character())
          Sys.sleep(0.5)
          defaultTimeZone = "America/New_York"
          
          withProgress(message = paste("Getting USGS data"), value = 0, {
            incProgress(0, detail = paste("Retrieving records for site ", input$gage_id))
            #Actually gets the gage data from the USGS NWIS system
            tryCatch({
            gageRawData$gagedata <- fun.GageData(
              myData.SiteID           <- input$gage_id,
              myData.Type             <- "Gage",
              myData.DateRange.Start  <- as.character(input$gage_date_start),#as.character(dateRange$min),
              myData.DateRange.End    <- as.character(input$gage_date_end), #as.character(dateRange$max),
              myDir.export            <- file.path(".", "data"),
              fun.myTZ = defaultTimeZone #ContData.env$myTZ
            )
            
            allVars <- colnames(gageRawData$gagedata)
            varsToPlot <- allVars[!(allVars %in% c("SiteID","GageID","Date.Time"))]
            updateSelectizeInput(session, 'gaze_params', choices = varsToPlot, selected = varsToPlot[1])
            
            shinyjs::show(id=ns("gageVarsDiv"),asis=TRUE)
            shinyjs::show(id=ns("gageDownloadDiv"),asis=TRUE)
            
            #Names the single column of the R console output data.frame
            colnames(consoleUSGS$disp) <- "R console messages for all USGS data retrieval:"
            
            message("USGS data retrieved")
            #Fills in the progress bar once the operation is complete
            incProgress(1/1, detail = paste("Retrieved records for site ", input$gage_id))
            Sys.sleep(1)
          
            }, error = function(err){
              print("Error while downloading data from USGS gage")
              print(err$message)
              renderErrorMsg(err$message)
              
            })
            
            if(class(gageRawData$gagedata)=="character"){
              renderErrorMsg(paste0("Error downloading USGS gage data. Check user guide for guidance on the following error: ", gageRawData$gagedata, ". Correct before proceeding."))
              shinyjs::hide(id=ns("gageVarsDiv"),asis=TRUE)
              shinyjs::hide(id=ns("gageDownloadDiv"),asis=TRUE)
            }
            
            # message("USGS data retrieved")
            # #Fills in the progress bar once the operation is complete
            # incProgress(1/1, detail = paste("Retrieved records for site ", input$gage_id))
            # Sys.sleep(1)
          })
        } else {
          renderErrorMsg(noGageIdFound)
        }
      })
      
      observeEvent(input$display_gage_raw, {
        clearContents()
        output$display_downloaded_data <- renderUI({})
        if (input$gage_id != "" & length(input$gage_id) > 0 & nrow(gageRawData$gagedata) > 0 & length(input$gaze_params) > 0) {
          gageRawPlot <- fun.gageRawPlot(fun.gage.data = gageRawData$gagedata,
                                         fun.gage.vars.to.process = input$gaze_params,
                                         fun.internal = TRUE)
          
          output$display_downloaded_data <- renderPlotly({
            ggplotly(gageRawPlot, height = calculatePlotHeight(length(isolate(input$gaze_params)) * 2), dynamicTicks = TRUE) %>% 
              plotly::layout(xaxis = list(type = "date")) %>% 
              plotly::config(toImageButtonOptions = list(format = "png", 
                                                         filename = paste0("USGS_gage_", input$gage_id, "_TS")))
            #%>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
          })
        } else {
          if (input$gage_id != "" & input$gage_id > 0 & nrow(gageRawData$gagedata) == 0) {
            renderErrorMsg(noGagaDataDownloaded)
            clearPlot()
          } else if (input$gage_id != "" & input$gage_id > 0 & length(input$gaze_params) == 0){
            renderErrorMsg(selectGageVars)
            clearPlot()
          } else if (input$gage_id == "") {
            renderErrorMsg(noGageIdFound)
            clearPlot()
          }
        }
      })

        output$download_gage_raw <- downloadHandler(
          filename = function(){
            paste0("USGS_gage_", input$gage_id, ".csv")
          },
          content = function(file){
            write.csv(gageRawData$gagedata  %>% mutate(Date.Time = format(Date.Time, "%Y-%m-%d %H:%M:%S")), file, row.names = FALSE)
          }
        )


      
      ########DayMet########
      observeEvent(input$get_daymet_data, {
        clearContents()
        clearPlot()
        #Sys.sleep(0.5)
        if (input$daymet_lat != "" && length(input$daymet_lat) > 0 && input$daymet_long != "" && length(input$daymet_long) > 0) {
          

          
          withProgress(message = "Getting DayMet data", value = 0, {
            incProgress(0, detail = paste("Retrieving records for Latitude and Longitude ", input$daymet_lat, input$daymet_long))
            
            startYear <- format(dateRange$min, format='%Y')
            endYear <- format(dateRange$max, format='%Y')
            
            # browser()
            #Actually gets the gage data from the USGS NWIS system
            tryCatch({
            rawResult <- fun.dayMetData(
              fun.lat <- input$daymet_lat,
              fun.lon <- input$daymet_long,
              fun.year.start <-  input$daymet_date_start, #as.numeric(startYear),
              fun.year.end <-  input$daymet_date_end, #as.numeric(endYear),
              fun.internal <-  TRUE
            )
            
            dayMetRawData$dayMetData <- rawResult$dayMetData
            daymetCols <- rawResult$daymetColumns
            
            dayMetRawData$daymetColumns <- rawResult$daymetColumns #
            updateSelectizeInput(session,
                                 'daymet_params',
                                 choices = daymetCols %>% setNames(
                                   c(
                                     "Precipitation (mm)",
                                     "Shortwave radiation (W m^-2)",
                                     "Snow water equivalent (kg m^-2)",
                                     "Maximum air temperature (degrees C)",
                                     "Minimum air temperature (degrees C)",
                                     "Water vapor pressure (Pa)"
                                   )
                                 ),
                                 selected = daymetCols[1])
            
            shinyjs::show(id="daymetVarsDiv")
            shinyjs::show(id = "daymetDownloadDiv")
            
            }, error = function(err){
              print("error while downloading dayMet data")
              renderErrorMsg(err$message)
            })
            
            #Fills in the progress bar once the operation is complete
            incProgress(1/1, detail = paste("Retrieved records for Latitude and Longitude ",input$daymet_lat, input$daymet_long))
          })
        } else {
          renderErrorMsg(noDaymetDataDownloaded)
          clearPlot()
        }
        
      })
      
      observeEvent(input$display_daymet_raw, {
        clearContents()
        dayMetPlotRaw <- draw_daymet_raw("DayMet raw data")
        if (!is.null(dayMetPlotRaw) & length(input$daymet_params) > 0) {

          output$display_downloaded_data <- renderPlotly({
            ggplotly(dayMetPlotRaw, height = calculatePlotHeight(length(isolate(input$daymet_params)) * 2), dynamicTicks = TRUE) %>% 
              plotly::layout(xaxis = list(type = "date")) %>% 
              plotly::config(toImageButtonOptions = list(format = "png", filename = paste0("Daymet_", input$daymet_lat, "_", input$daymet_long, "_TS")))

          })
        } else {
          alertMsg <- NULL
          if (is.null(dayMetRawData$dayMetData)) {
            alertMsg <- downlaodDaymetData
          } else if(!is.null(dayMetRawData$dayMetData) & length(input$daymet_params) == 0) {
            alertMsg <- selectDaymetVars
          }
          renderErrorMsg(alertMsg)
          clearPlot()
        }
      }) 
      
      draw_daymet_raw <- function(plotTitle) {
        dayMetPlot <- NULL
        if (!is.null(dayMetRawData$dayMetData) & length(input$daymet_params) > 0) {
          dayMetPlot <-  fun.dayMetRawPlot(
            fun.daymet.data = dayMetRawData$dayMetData,
            fun.daymet.vars.to.process = input$daymet_params,
            fun.daymet.title = plotTitle,
            fun.internal = TRUE
          )
        } 
        return(dayMetPlot)
      }
    
      output$download_daymet_raw <- downloadHandler(
        filename = function(){
          paste0("Daymet_", input$daymet_lat, input$daymet_long, ".csv")
        },
        content = function(file){
          write.csv(dayMetRawData$dayMetData, file, row.names = FALSE)
        }
      )
      
      #subplot for daymet, gage and base
      observeEvent(input$display_subplot_ts, {
        daymet_data_raw <- NULL
        gage_data_raw <- NULL
        base_data_raw <- NULL
        mergedList <- list()
        totalH <- 0L
        
        if(is.null(dateRange$min)){
          shinyalert("Upload base data", "Please upload a continuous dataset in the Upload Data tab to access this feature.", "warning")
        }
        
        if(length(input$dailyStats_ts_variable_name2) > 0) {
          clearContents()
        
          #test all row data
          if (!is.null(dayMetRawData$dayMetData) & length(input$daymet_params) > 0) {
            daymet_data_raw  <- dayMetRawData$dayMetData %>%
              select(c(input$daymet_params), c("year", "yday")) %>%
              mutate(Date=as.Date(yday, origin=paste(as.character(year - 1), "-12-31", sep=""))) %>%
              select(c(input$daymet_params), "Date") %>%
              gather(key = "Parameter", value = "value",-Date)
            allParames <- daymet_data_raw %>% pull(Parameter)
            
            totalH <- totalH + length(input$daymet_params)
            daymet_data_raw$Parameter <- paste("DayMet",allParames,sep="_")
            mergedList[["DayMet"]] <- daymet_data_raw
          }
          
          if(paste0(input$gaze_params, collapse = "") != "" && length(input$gaze_params) > 0) {
            #gageGroup <- paste(input$gage_params, "Gage", sep=".")
            gage_data_raw  <- gageRawData$gagedata %>%
              select(all_of(input$gaze_params), all_of("GageID"), 'Date'=all_of("Date.Time")) %>%
              gather(key = "Parameter", value = "value",-GageID, -Date) %>% 
              select(-GageID)
            
            totalH <- totalH + length(input$gaze_params)
            allParames <- gage_data_raw %>% pull(Parameter)
            gage_data_raw$Parameter <- paste("Gage",allParames,sep="_")
            mergedList[["Gage"]] <- gage_data_raw
          }
          raw_data <- formated_raw_data$derivedDF
          ##print(formated_raw_data$derivedDF)
          variable_to_plot <- input$dailyStats_ts_variable_name2
          if (!is.null(variable_to_plot) & nrow(raw_data) != nrow(raw_data[is.na(raw_data$date.formatted),])){
            
            base_data_raw  <- raw_data %>%
              select(any_of(variable_to_plot), Date= c("date.formatted")) %>%
              gather(key = "Parameter", value = "value", -Date)
            
            totalH <- totalH + length(variable_to_plot)
            allParames <- base_data_raw %>% pull(Parameter)
            base_data_raw$Parameter <- paste("BaseFile",allParames,sep="_")
            mergedList[["BaseFile"]] <- base_data_raw
            
            shinyjs::show(id = "mergedDownloadDiv")

            merge_to_dwnld <- bind_rows(mergedList, .id="df") %>% 
              dplyr::select(-df) %>% 
              mutate(Date = format(Date, "%Y-%m-%d %H:%M:%S")) %>% 
              group_by(Date, Parameter) %>% 
              slice(1) %>% # deal with two values on daylight savings time hours
              pivot_wider(values_from = value, names_from = Parameter)
            
            
            output$download_merged <- downloadHandler(
              filename = function(){
                paste0("Base_", input$dailyStats_ts_variable_name2, "_USGS_", input$gage_id,"_Daymet_", input$daymet_lat, input$daymet_long, ".csv")
              },
              content = function(file){
                write.csv(merge_to_dwnld, file, row.names = FALSE)
              }
            )
          }
          
          
          allCom <- ggplot(arrange(bind_rows(mergedList, .id="df"),Parameter) %>% mutate(Date = as.POSIXct(Date,format="%Y-%m-%d")), aes(x = Date, y = value)) +
            geom_line(aes(colour=Parameter)) +
            labs(title="Base, USGS gage and DayMet Data", y="Parameters", x="Date") + 
            #scale_x_datetime(date_labels=main_x_date_label,date_breaks=mainBreaks)+
            theme_bw()+
            theme(
              strip.background = element_blank()
              #,strip.text.y = element_blank()
              ,strip.placement = "outside" 
              ,text=element_text(size=10,face = "bold", color="cornflowerblue")
              ,plot.title = element_text(hjust=0.5)
              ,legend.position="bottom"
              #,axis.text.x=element_text(angle=65, hjust=10)
            )
          allCom = allCom + facet_grid(Parameter ~ ., scales = "free_y")
          
          
          output$display_downloaded_data <- renderPlotly({
            ggplotly(allCom, height = calculatePlotHeight(totalH*2), dynamicTicks = TRUE) %>% 
              plotly::layout(xaxis = list(type = "date")) %>% 
              plotly::config(toImageButtonOptions = list(format = "png", filename = paste0("Base_USGS_", input$gage_id, "_Daymet_",input$daymet_lat, "_", input$daymet_long, "_TS")))
          }) 
        } else {
          renderErrorMsg(selectBaseVars)
          clearPlot()
        }
      })
      
      
      #common
      renderErrorMsg <- function(msg) {
        output$gageDayMetError <- renderUI({
          div(class="alert alert-danger" , msg) 
        })
      }
      clearContents <- function(){
        output$gageDayMetError <- renderUI({})
      }
      
      clearPlot <- function(){
        output$display_downloaded_data <- renderPlotly({
          plotly_empty()
        })
      }

    }) # end of module server
}
