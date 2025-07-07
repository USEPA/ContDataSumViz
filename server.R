server <- function(input, output, session) {
  useShinyjs()
  conflict_prefer("box", "shinydashboard")
  conflict_prefer("dataTableOutput", "DT")
  conflict_prefer("yday", "data.table")
  conflict_prefer("select", "dplyr")
  conflict_prefer("isolate", "shiny")
  
  loaded_data <- reactiveValues()
  raw_data_columns <- reactiveValues()
  dateRange <- reactiveValues()
  workflowStatus <- reactiveValues(
    finish = FALSE,
    elementId = "step1",
    state = "init"
  )
  
  output$display_time_series_discrete <- renderPlotly({
    plotly_empty()
  })
  
  currentOutPutId <- reactiveValues()
  selected_to_plot <- reactiveValues(all_selected = data.frame())
  processed <- reactiveValues(
    processed_dailyStats = list(),
    ST.freq = data.frame(),
    ST.mag = data.frame(),
    ST.roc = data.frame(),
    ST.tim = data.frame(),
    ST.var = data.frame()
  )
  
  formated_raw_data <- reactiveValues(
    derivedDF = data.frame(),
    baseColNames = as.character()
  )
  homeDTvalues <- reactiveValues(
    homeDateAndTime = list(),
    homeDateFormat = as.character()
  )
  
  metaHomeValues <- reactiveValues(
    metaVal = list()
  )
  
  discreteDTvalues <- reactiveValues(disDateAndTime = list())
  formatted_dis_data <- reactiveValues(derivedDF = list())
  
  newDTvalues <- reactiveValues(newDateAndTime = list())
  
  to_download <- reactiveValues()
  saveToReport <- reactiveValues(metadataTable = data.frame())
  
  dailyStatusCalculated <- reactiveValues(status = "unfinished")
  readyForCalculation <- reactiveValues(status = FALSE)
  renderDataExp <- reactiveValues(render = FALSE)
  renderSummaryTables <- reactiveValues(render = FALSE)
  renderTSOverlay <- reactiveValues(render = FALSE)
  renderTSBoxPlot <- reactiveValues(render = FALSE)
  renderCDFPlot <- reactiveValues(render = FALSE)
  renderRasterPlot <- reactiveValues(render = FALSE)
  
  # Temperature Tab
  renderThermalStats <- reactiveValues(render = FALSE)
  renderAirVsWater <- reactiveValues(render = FALSE)
  renderGrowingDegree <- reactiveValues(render = FALSE)
  renderThermalClassification <- reactiveValues(render = FALSE)
  renderUsgsAndDaymet <- reactiveValues(render = TRUE)
  renderTempNTE <- reactiveValues(render = FALSE)
  
  # Hydrology Tab
  renderUSGSDaily <- reactiveValues(render = FALSE)
  renderIHA <- reactiveValues(render = FALSE)
  renderFlashiness <- reactiveValues(render = FALSE)
  renderDiscrete <- reactiveValues(render = FALSE)
  homePageInputs <- reactiveValues(changed = FALSE)
  
  gageRawData <- reactiveValues(gagedata = data.frame(),
                                gageColName = as.character())
  dayMetRawData <- reactiveValues(dayMetData = data.frame(),
                                  daymetColumns = as.character())
  
  gageDailyRawData <- reactiveValues(gagedata = data.frame(),
                                     gageColName = as.character())
  
  flags <- reactiveValues(flagOptions = as.character(),
                          flagCols = list(),
                          flagCodes = list())
  ##############################
  # init shiny modules
  ##############################
  
  # workflow module (step1, step2, step3, step4, step5)
  progressWorkflowModuleServer("statusWorkflow", workflowStatus)
  
  # calculate daily statistics
  calculateDailyStatsModuleServer("calculateDailyStats", formated_raw_data, homeDTvalues, metaHomeValues, loaded_data, dailyStatusCalculated, processed, readyForCalculation, flags)
  
  # flag module
  flagServer("displayStep3", homeDTvalues, formated_raw_data, flags)
  
  ############ Continuous Data Exploration >>  All parameters ############
  
  # Continuous Data Exploration > All Parameters >  Summary tables tab
  SummaryTablesModuleServer(id = "DataExpSummaryTbls", dailyStats = processed, renderSummaryTables, loaded_data)
  
  # Continuous Data Exploration > All Parameters >  Time Series plots tab
  DataExplorationTSModuleServer(id = "dataExpTS", dailyStats = processed, renderDataExp, loaded_data)
  
  # Continuous Data Exploration > All Parameters >  Time series - Annual overlays tab
  TsOverlayModuleServer(id = "tsOverlayTab", dailyStats = processed, renderTSOverlay, loaded_data)
  
  # Continuous Data Exploration > All Parameters > Box plots tab
  TsBoxPlotModuleServer(id = "tsBoxPlot", dailyStats = processed, renderTSBoxPlot, loaded_data)
  
  # Continuous Data Exploration > All Parameters > CDFs tab
  TsCDFPlotModuleServer(id = "tsCDFPlot", dailyStats = processed, renderCDFPlot, loaded_data)
  
  # Continuous Data Exploration > All Parameters > Raster graphs tab
  TsRasterPlotModuleServer(id = "tsRasterPlot", dailyStats = processed, renderRasterPlot)
  
  # Continuous Data Exploration >  Temperature > Thermal statistics tab
  ThermalStatsModuleServer("thermalStats", uploaded_data, formated_raw_data, dailyStats = processed, loaded_data, to_download, renderThermalStats)
  
  # Continuous Data Exploration >  Temperature > Air Vs Water tab
  AirVsWaterModuleServer("airVsWater", uploaded_data, dailyStats = processed, renderAirVsWater)
  
  # Continuous Data Exploration >  Temperature > Growing Degree days tab
  GrowingDegreeModuleServer("growingDegree", uploaded_data, dailyStats = processed, renderGrowingDegree, loaded_data)
  
  # Continuous Data Exploration >  Temperature > Thermal Classification tab
  ThermalClassificationModuleServer("thermalClassification", dailyStats = processed, uploaded_data, renderThermalClassification, loaded_data)
  
  # Continuous Data Exploration >  Hydrology > IHA tab
  IHAModuleServer("IHATab", dailyStats = processed, loaded_data, uploaded_data, to_download, renderIHA, gageDailyRawData)
  
  # Continuous Data Exploration >  Hydrology > Flashiness tab
  FlashinessModuleServer("flashinessTab", uploaded_data, dailyStats = processed, renderFlashiness, loaded_data, gageDailyRawData)
  
  # USGS & Daymet Exploration tab
  GageAndDaymetModuleServer("gageDaymetAndBase", homeDTvalues, dateRange, formated_raw_data, renderUsgsAndDaymet, gageRawData, dayMetRawData)
  
  # Temperature Not to Exceed tab
  TempNotToExceedServer("tempNTE", uploaded_data, formated_raw_data, renderTempNTE, loaded_data)
  
  # USGS Daily Flow Download tab
  USGSDailyModuleServer("USGSDailyTab",dateRange, uploaded_data, dailyStats = processed, renderUSGSDaily, loaded_data, gageDailyRawData)
  
  #  Upload Data##############################
  if (file.exists("_moved/File_Format.rds")) file.remove("_moved/File_Format.rds")
  do.call(file.remove, list(list.files("Selected_Files", full.names = TRUE)))
  
  # click upload
  uploaded_data <- eventReactive(c(input$uploaded_data_file), {
    readyForCalculation$status <- FALSE
    dailyStatusCalculated$status <- "unfinished"
    
    my_data <- uploadFile(c(input$uploaded_data_file), stopExecution = FALSE, tab = "homePage")
    # drop all rows where all the columns are empty
    if (length(my_data) > 0) {
      my_data <- my_data[rowSums(is.na(my_data) | is.null(my_data) | my_data == "") != ncol(my_data), ]
      workflowStatus$elementId <- "step1"
      workflowStatus$state <- "success"
      
    }
    
    return(my_data)
  })
  
  # Behavior on upload
  observeEvent(uploaded_data(), {
    
    output$displayFC <- renderUI({
      data <- uploaded_data() 
      tagList(
        radioButtons("disp", "Display file information",
                     choices = c("First six rows" = "head", "Last six rows" = "tail", "Column names" = "Column names"),
                     selected = "head"
        ),
        hr(),
        actionButton(inputId = "displayidLeft", label = "Display file contents", class = "btn btn-primary")
      )
    })
    
    homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data(), homePageInputs)
  })
  
  # Click display file contents
  observeEvent(input$displayidLeft, {
    output$contents <- renderTable(
      {
        sub_df <- uploaded_data()
        
        if (isolate(input$disp == "head")) {
          return(head(sub_df))
        } else if (isolate(input$disp == "tail")) {
          return(tail(sub_df))
        } else {
          return(colnames(sub_df))
        }
      },
      type = "html",
      bordered = TRUE,
      striped = TRUE,
      align = "c",
      width = "100%"
    )
    output$display_paramselect <- renderUI({
      div(class="panel panel-default", style="margin:10px;",
          div(class="panel-heading", "Step 2: Select date and time format", style="font-weight:bold;",icon("info-circle", style = "color:#2fa4e7", id="datetimeHelp"),
              bsPopover(id="datetimeHelp", title=HTML("<b>Helpful Hints</b>"), content = HTML("To download the time series, mouse over the plot to display the control panel in the upper righthand corner and select the camera icon. <br><br> The test data from Posey Creek has datetimes displayed according to ISO 8601 notation. Date in year, month, day format is separated from time in hours, minutes, seconds format with the character `T.` The time zone follows, which is represented as the deviation from coordinated universal time (UTC) with the letter `Z`. No number after `Z` indicates that the test data are in UTC time zone. "),
                        placement = "right", trigger = "hover")),
          div(class = "panel-body", style = "margin-left: 10px;margin-right: 10px;margin-top: 10px",
              dateAndTimeUI(id = "homePage", paramChoices =  fun.findVariableToProcess(colnames(uploaded_data()), getDateCols = FALSE), uploadedCols = colnames(uploaded_data()))),
          div("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;margin-left: 10px;margin-bottom: 10px"),
          div(actionButton(inputId = "showrawTS", label = "Display time series", class = "btn btn-primary", style = "margin: 10px;")),
          # div("To download the plot, mouse over the plot to display the control panel in the upper righthand corner and select the camera icon.", style = "margin-left:10px;margin-bottom: 10px; margin-right:10px;"),
          div(id = "dateAndTimeError")
      )
    })
  })
  
  # click display time series
  observeEvent(input$showrawTS, {
    
    shinyjs::removeClass("dateAndTimeError", "alert alert-danger")
    
    raw_data <- uploaded_data()
    homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data(), homePageInputs)
    
    # handle rows with no date time
    if(homeDTvalues$homeDateAndTime$dateColumnNums() == "separate"){
      temp <- raw_data %>% dplyr::filter(get(homeDTvalues$homeDateAndTime$dateFieldName()) != "",  
                                         get(homeDTvalues$homeDateAndTime$timeFieldName()) != "",  
                                         is.na(get(homeDTvalues$homeDateAndTime$dateFieldName())) == FALSE,  
                                         is.na(get(homeDTvalues$homeDateAndTime$timeFieldName())) == FALSE)
    }
    
    if(homeDTvalues$homeDateAndTime$dateColumnNums() == "combined"){
      temp <- raw_data %>% dplyr::filter(get(homeDTvalues$homeDateAndTime$dateFieldName()) != "",  
                                         is.na(get(homeDTvalues$homeDateAndTime$dateFieldName())) == FALSE)
    }
    
    if(nrow(temp)!= nrow(raw_data)){
      rows_missing_data <- nrow(raw_data) - nrow(temp)
      shinyAlertUI("removed_rows_msg", 
                   paste0("Number of rows removed with missing date/time: ", rows_missing_data), 
                   "Warning")
      print(paste0("Number of rows removed with missing date/time: ", rows_missing_data))
    }
    
    raw_data <- temp
    
    showRawDateAndTime <- homeDTvalues$homeDateAndTime
    
    # display_validation_msgs dateBox
    if (showRawDateAndTime$isTimeValid() & showRawDateAndTime$isDateAndtimeValid()) {

      tryCatch(
        {
          # if error had occured then on fix reset the step
          shinyjs::removeClass("statusWorkflow-step3", "btn-danger")
          shinyjs::addClass("statusWorkflow-step3", "btn-primary")
          formated_raw_data$derivedDF <- getFormattedRawData(showRawDateAndTime, raw_data, tabName = "homePage", errorDivId = "dateAndTimeError")
          rawTSModuleServer("displayRawTS", showRawDateAndTime, formated_raw_data, loaded_data)

          workflowStatus$elementId <- "step2"
          workflowStatus$state <- "success"
        },
        error = function(parsingMsg) {
          processErrors(parsingMsg, tab = "homePage", elementId = "dateAndTimeError")
          workflowStatus$elementId <- "step2"
          workflowStatus$state <- "error"
        },
        warning = function(parsingMsg) {
          processErrors(parsingMsg, tab = "homePage", elementId = "dateAndTimeError")
          workflowStatus$elementId <- "step2"
          workflowStatus$state <- "error"
         } #,
        # message = function(parsingMsg) {
        #   print(parsingMsg)
        #   workflowStatus$elementId <- "step2"
        #   workflowStatus$state <- "success"
        # }
      ) # end of tryCatch
    } # end of validation check
    

    output$display_subsetTS <- 
      renderUI({
        div(class="panel panel-default", style="margin:10px;",
            div(class="panel-heading", "Step 2b: Subset time series (optional)", style="font-weight:bold;", icon("info-circle", style = "color:#2fa4e7", id="subsetHelp")),
            div(bsPopover(id="subsetHelp", title=HTML("<b>Helpful Hints</b>"), content = HTML("Selecting the subset data and update time series button will subset the data for all subsequent outputs. Once subset, the app will need to be reloaded and raw uploaded to return to the full time series. If you wish to view the time series with a different date range, please use the box zoom, which can be accessed by hovering over the plot to reveal the control panel in the upper right, selecting the magnifying glass icon, clicking, and dragging across the range you wish to view."), 
                          placement = "right", trigger = "hover")),
            div(div(dateInput("date_start","Date start",value = min(formated_raw_data$derivedDF$date.formatted) %>% as.character(),min="1980-01-01",max="2100-01-01",format="yyyy-mm-dd")),
                div(dateInput("date_end","Date end",value =max(formated_raw_data$derivedDF$date.formatted) %>% as.character(),min="1980-01-01",max="2100-01-01",format="yyyy-mm-dd")), style="margin:10px;"),
            div(actionButton(inputId = "updateTS", label = "Subset data & update time series", class = "btn btn-primary", style = "margin-left: 10px;margin-right: 10px;margin-bottom: 10px;margin-top: 20px;")),
            div("Selecting will subset the data for all subsequent outputs", style = "margin-left:10px;margin-bottom:10px;")
        )
      })
    
    output$display_runmetasummary <-
      renderUI({
        div(class="panel panel-default", style="margin:10px;",
            div(class="panel-heading", "Step 3: Run meta summary", style="font-weight:bold;", icon("info-circle", style = "color:#2fa4e7", id="metadataHelp")),
            div(bsPopover(id="metadataHelp",   title=HTML("<b>Helpful Hints</b>"), content = HTML("Identify quality flags to eliminate observations with quality issues. Flags must be available for each selected parameter in a separate column for each parameter or a single column, if a common flag applies to all parameters. All included flag codes (fail, suspect, known) must be in the flag column(s) and consistent across parameters, with unique codes for different flags. Run meta summary generates a main panel table summarizing missing and flagged data, the total period of record, and days in the record. <br><br> If data were prepared using ContDataQC with the default configuration, the codes are: fail = F, suspect = S, and not known = X. For modified configurations, refer to the config file for codes. <br><br> The Posey Creek test data use quality flag columns named the parameter plus QF. Observations passing quality checks have a 0; failed checks have a 1. The test data only include Fail quality flags."), 
                          placement = "right", trigger = "hover")),
            div(class = "panel-body", flagUI("displayStep3"), style = "margin-left: 10px; margin-right: 10px;")
        )
      })
    
    output$ts_right <- renderUI({
      div(id = "display_all_raw_ts_div", style = paste0("height:", calculatePlotHeight(length(showRawDateAndTime$parmToProcess()))), rawTSModuleUI("displayRawTS") #"height:100%;width:100%" # time series calculatePlotHeight(length(isolate(input$dailyStats_ts_variable_name)) * 2)
      ) # end of div
    })
  }) ## observeEvent end
  
  observeEvent(input$updateTS,{
    # Updated formated_raw_data
    formated_raw_data$derivedDF <- formated_raw_data$derivedDF %>% dplyr::filter(date.formatted >= input$date_start & date.formatted < (input$date_end + days(1)))
    
    showRawDateAndTime <- homeDTvalues$homeDateAndTime
    rawTSModuleServer("displayRawTS", showRawDateAndTime, formated_raw_data, loaded_data)
    

  })
  
  # click Run meta summary
  observeEvent(input$runQS, {
    tryCatch(
      {

        localHomeDateAndTime <- homeDTvalues$homeDateAndTime
        # display_validation_msgs dateBox
        if (localHomeDateAndTime$isTimeValid() & localHomeDateAndTime$isDateAndtimeValid()) {
          # update the reactiveValues
          # All the variables are selected
          workflowStatus$elementId <- "step2"
          workflowStatus$state <- "success"

          raw_data <- formated_raw_data$derivedDF
          # now shorten the varname
          if ("date.formatted" %in% colnames(raw_data) & !is.null(localHomeDateAndTime$parmToProcess()) & nrow(raw_data) != nrow(raw_data[is.na(raw_data$date.formatted), ])) {
            print("passed fun.ConvertDateFormat")
            
            # set dateRange for other modules
            formated_raw_data$derivedDF <- raw_data
            dateRange$min <- min(as.Date(raw_data$date.formatted), na.rm = TRUE)
            dateRange$max <- max(as.Date(raw_data$date.formatted), na.rm = TRUE)
            raw_data_columns$date_column_name <- "date.formatted"

            # now shorten the varname
            raw_data <- formated_raw_data$derivedDF
            metaHomeValues$metaVal <- metaDataServer("metaDataHome", localHomeDateAndTime$parmToProcess(), formatedUploadedData = raw_data, uploadData = uploaded_data(), flags)
            raw_data_columns$date_column_name <- "date.formatted"
            output$display_fill_data <- renderUI({
              div(style = "margin-top:20px;", metaDataUI("metaDataHome")) 
            })
            
            shinyjs::runjs("$('#dateTimeBoxButton').click()")
            
            if (workflowStatus$finish == FALSE) {
              workflowStatus$elementId <- "step3"
              workflowStatus$state <- "success"
              readyForCalculation$status <- TRUE
              homePageInputs$changed <- FALSE
            }
            
            output$display_actionButton_calculateDailyStatistics <-
              renderUI({
                div(class="panel panel-default", style="margin:10px;",
                    div(class="panel-heading", "Step 4: Calculate daily statistics", style="font-weight:bold;",
                        icon("info-circle", style = "color:#2fa4e7", id="calcDailyHelp")),
                    div(bsPopover(id="calcDailyHelp", title=HTML("<b>Helpful Hints</b>"), content = HTML("Use this module to calculate and download daily statistics. Saving per site per parameter will generate a zipped folder with a csv file for each selected parameter. Saving per site with all parameters will generate a single csv file with data for every selected parameter. Save in long format will create a single column for all parameters and a single column for their corresponding value for each date/time. Saving the data in long format is a step in formatting data in Water Quality eXchange (WQP) format for upload to the Water Quality Portal (WQP)."), 
                                  placement = "right", trigger = "hover")),
                    div(step4UI("metaDataHome"), style = "margin:10px"), #; margin-top:30px
                    div(calculateDailyStatsModuleUI("calculateDailyStats", readyForCalculation))
                )
                
              })
            output$step5 <-
              renderUI({
                div(step5ui("calculateDailyStats"))
              })
          } else {
            # shinyAlertUI("common_alert_msg" , invalidDateFormt, "ERROR")
            print("it should have updated users on the UI")
          }
        }
      },
      error = function(parsingMsg) {
        processErrors(parsingMsg, tab = "homePage", elementId = "dateAndTimeError")
        readyForCalculation$status <- FALSE
      },
      warning = function(parsingMsg) {
        processErrors(parsingMsg, tab = "homePage", elementId = "dateAndTimeError")
        readyForCalculation$status <- FALSE
      },
      message = function(parsingMsg) {
        processErrors(parsingMsg, tab = "homePage", elementId = "dateAndTimeError")
        readyForCalculation$status <- FALSE
      }
    ) # end of tryCatch
  }) ## observeEvent end
  
  
  # click Calculate Daily Statistics reveals Step 5 panel from inside the calculateDailyStatsModuleUI function
  
  #' supporting function - formats date and time based on user inputs
  #' for the uploaded files
  #'
  #' @param dateAndTimeFields 
  #' @param userData 
  #' @param tabName 
  #' @param errorDivId 
  #'
  #' @return userDataL (formated user data)
  
  getFormattedRawData <- function(dateAndTimeFields, userData, tabName, errorDivId) {
    tryCatch(
      {
        shinyjs::runjs(paste0("$('#", errorDivId, "').text('')"))
        shinyjs::runjs(paste0("$('#", errorDivId, "').css('padding', '0px')"))
        shinyjs::removeClass(errorDivId, "alert alert-danger")
        userDataL <- fun.ConvertDateFormat(
          fun.userDateFormat = dateAndTimeFields$dateFormat(),
          fun.userTimeFormat = dateAndTimeFields$timeFormat(),
          fun.userTimeZone = dateAndTimeFields$timeZone(),
          fun.userDateFieldName = dateAndTimeFields$dateFieldName(),
          fun.userTimeFieldName = dateAndTimeFields$timeFieldName(),
          fun.rawData = userData,
          fun.date.org = dateAndTimeFields$dateColumnNums()
        )
      },
      error = function(parsingMsg) {
        processErrors(parsingMsg, tab = tabName, elementId = errorDivId)
      },
      warning = function(parsingMsg) {
        processErrors(parsingMsg, tab = tabName, elementId = errorDivId)
        } #,
      # message = function(parsingMsg) {
      #   #processErrors(parsingMsg, tab = tabName, elementId = errorDivId)
      #   parsingMsg
      # }
    ) # end of tryCatch
    return(userDataL)
  }
  
  
  observeEvent(input$dateTimeBoxButton, {
    hideShowDateTimeBox("dateTimeBoxButton")
  })
  observeEvent(input$dateTimeBoxButton_discrete, {
    hideShowDateTimeBox("dateTimeBoxButton_discrete")
  })
  
  observeEvent(input$dateTimeBoxButton_new, {
    hideShowDateTimeBox("dateTimeBoxButton_new")
  })
  
  hideShowDateTimeBox <- function(buttonId) {
    boxId <- "dateBox"
    if (grepl("_new", buttonId, fixed = TRUE)) {
      boxId <- "dateBox_new"
    } else if (grepl("discrete", buttonId, fixed = TRUE)) {
      boxId <- "dateBox_discrete"
    }
    if (input[[buttonId]] %% 2 == 1) {
      shinyjs::hide(id = boxId)
      updateActionButton(session, buttonId, icon = icon("arrow-up"), label = "Show Selection")
    } else {
      shinyjs::show(id = boxId)
      updateActionButton(session, buttonId, icon = icon("arrow-down"), label = "Hide Selection")
    }
  }
  
  
  #' Supporting function to prepare error messages for display
  #'
  #' @param errorMsg 
  #' @param tabName 
  #' @param elementId 
  #'
  processErrors <- function(errorMsg, tabName = "", elementId) {
    processedMsg <- prepareDateFormatErrorMsg(errorMsg, tab = tabName)
    shinyjs::runjs(paste0("$('#", elementId, "').text('", processedMsg, "')"))
    shinyjs::addClass(elementId, "alert alert-danger")
  }
  
  
  observe({
    if (dailyStatusCalculated$status == "finished") {
      workflowStatus$elementId <- "step4"
      workflowStatus$state <- "success"
      workflowStatus$elementId <- "step5"
      workflowStatus$state <- "success"
    } else if (dailyStatusCalculated$status == "error") {
      workflowStatus$elementId <- "step4"
      workflowStatus$state <- "error"
    }
  })
  
  #' supporting function to prepare error message
  #'
  #' @param errorMsg 
  #' @param tab 
  #'
  prepareDateFormatErrorMsg <- function(errorMsg, tab = "") {
    if (tab == "homePage") {
      workflowStatus$elementId <- "step3"
      workflowStatus$state <- "error"
      dailyStatusCalculated$status <- "unfinished"
      readyForCalculation$status <- FALSE
    }
    readyForCalculation$status <- FALSE
    if (grepl("All formats failed to parse. No formats found.", errorMsg[1], fixed = TRUE) |
        grepl("failed to parse", errorMsg[1], fixed = TRUE)) {
      formattedError <- "There is a mismatch between uploaded file date/time format and selected date/time format, please correct and try again."
      return(formattedError)
    } else {
      return(errorMsg[1])
    }
  }
  
  ## close the warning messages inside the above oberveEvent
  
  # Discrete Data Exploration
  uploaded_discreteData <- eventReactive(c(input$uploaded_discrete_file), {
    discrete_data <- uploadFile(c(input$uploaded_discrete_file), stopExecution = FALSE)
    discrete_data <- discrete_data[rowSums(is.na(discrete_data) | is.null(discrete_data) | discrete_data == "") != ncol(discrete_data), ]
    return(discrete_data)
  })
  
  observeEvent(input$uploaded_discrete_file, {
    cols_avail <- colnames(uploaded_discreteData())
    fileContentForDisplay <- head(uploaded_discreteData())
    output$discreteDateAndTimeBox <- renderUI({
      div(
        id = "dt_discrete",
        div(
          class = "panel panel-default", width = "100%",
          div(
            class = "panel-heading", style = "width:100%;",
            span("Select date and time format for discrete data", style = "font-weight:bold;")
          ),
          
          div(class = "panel-body",
              div(
                style = "margin-left:10px;margin-right:10px;",
                dateAndTimeUI(id = "discretePage", paramChoices = cols_avail, uploadedCols = cols_avail)
              ),
              hr(),
              div("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;margin-left:10px;margin-bottom:10px"),
              div(
                actionButton(inputId = "display_discrete_data", label = "View discrete-continuous plot", class = "btn btn-primary"), style = "margin:10px 15px 10px 10px;"),
              div(id = "discreteDownloadDiv", style = "margin:10px 15px 10px 10px; display:none;", renderUI({
                downloadButton("download_discrete", "Download combined data", class="btn btn-primary")
              })),
              div(id = "disDateAndTimeError", style = "margin-top:10px; margin-left:10px;")
          ) # end of box
        )
      )
    })
    
    output$discreteHeader <- renderUI({
      fluidRow(
        column(
          width = 12,
          tags$div(
            renderTable(
              {
                fileContentForDisplay
              },
              type = "html",
              bordered = TRUE,
              striped = TRUE,
              align = "c",
              width = "100%"
            ),
            style = "overflow-x:auto;"
          ) # end of div
        ) # end of column
      ) # end of row
    })
    
    # init the module
    discreteDTvalues$disDateAndTime <- dateAndTimeServer(id = "discretePage", uploaded_discreteData(), homePageInputs)
  })
  
  output$baseParameters <- renderUI({
    baseParams <- homeDTvalues$homeDateAndTime$parmToProcess()
    selectizeInput(
      "discreteBaseId",
      label = "Select parameters to process",
      choices = baseParams,
      multiple = TRUE,
      selected = baseParams[1],
      options = list(
        hideSelected = FALSE,
        plugins = list("remove_button")
      )
    )
  })
  
  #observe(if(nrow(gageRawData$gagedata) > 0 | nrow(dayMetRawData$dayMetData) > 0){
    output$gage_daymet_discrete <- renderUI({
      checkboxGroupInput(inputId = "gageday_discrete", 
                         label = "Display with downloaded data",
                         choices = c("USGS gage", "DayMet"), 
                         selected = )
    })
    observeEvent(input$gageday_discrete, {
      if("USGS gage" %in% input$gageday_discrete){
        if(nrow(gageRawData$gagedata) == 0){
          shinyalert("Downloaded data required","Please download USGS gage data in the USGS & Daymet Exploration tab to access this feature", type = "warning")
        }
        
        output$gage_select_discrete <- renderUI({
          selectizeInput("gage_param_discrete", label ="Select USGS Gage variables",
                         choices= setdiff(names(gageRawData$gagedata), c("GageID", "SiteID", "Date.Time")),
                         multiple = TRUE,
                         options = list(hideSelected = FALSE, plugins = list("remove_button")))
        }) 
      }
      if("DayMet" %in% input$gageday_discrete){
        if(nrow(dayMetRawData$dayMetData) == 0){
          shinyalert("Downloaded data required", "Please download Daymet data in the USGS & Daymet Exploration tab to access this feature", type = "warning")
        }
        output$daymet_select_discrete <- renderUI({
          selectizeInput("daymet_param_discrete", label ="Select Daymet variables",
                         choices= setdiff(names(dayMetRawData$dayMetData), c("year", "yday", "dayl..s."))%>% setNames(
                           c("Precipitation (mm)",
                             "Shortwave radiation (W m^-2)",
                             "Snow water equivalent (kg m^-2)",
                             "Maximum air temperature (degrees C)",
                             "Minimum air temperature (degrees C)",
                             "Water vapor pressure (Pa)")),
                         multiple = TRUE,
                         options = list(hideSelected = FALSE, plugins = list("remove_button")))
        })
      }
    }) 
 # })
  
  observeEvent(input$display_discrete_data, {
    discreteDTvalues$disDateAndTime <- dateAndTimeServer(id = "discretePage", uploaded_discreteData(), homePageInputs)
    localDiscreteDateAndTime <- discreteDTvalues$disDateAndTime
    mainPlot <- NULL
    if (localDiscreteDateAndTime$isTimeValid() & localDiscreteDateAndTime$isDateAndtimeValid()) {
      tryCatch(
        {
          variable_to_plot <- sort(localDiscreteDateAndTime$parmToProcess(), decreasing = FALSE) # discrete
          base_vars_to_plot <- sort(input$discreteBaseId, decreasing = FALSE) # continuous
          if (length(intersect(variable_to_plot, base_vars_to_plot)) != 0) {
            discrete_data <- getFormattedRawData(localDiscreteDateAndTime, uploaded_discreteData(), tabName = "", errorDivId = "disDateAndTimeError")
            
            if ("date.formatted" %in% colnames(discrete_data) & nrow(discrete_data) != nrow(discrete_data[is.na(discrete_data$date.formatted), ])) {
              base_data <- formated_raw_data$derivedDF
              if (!is.null(base_vars_to_plot) & nrow(base_data) != nrow(base_data[is.na(base_data$date.formatted), ])) {
                mergedData <- NULL
                mergedDataOriginal <- NULL
                for (varName in variable_to_plot) {
                  step1original <- base_data %>% select("continuous_value" = all_of(varName), "Date" = c(date.formatted)) 
                  
                  step1 <- base_data %>% select("continuous_value" = all_of(varName), "Date" = c(date.formatted)) %>% 
                    mutate(continuous_value = if_else((is.na(continuous_value) & 
                                                         is.na(dplyr::lag(continuous_value)) == FALSE & 
                                                         is.na(dplyr::lead(continuous_value))==FALSE),  
                                                      dplyr::lag(continuous_value), continuous_value))
                  step2 <- discrete_data %>% select("discrete_value" = all_of(varName), "Date" = c(date.formatted))
                  
                  tempdf <- full_join(step1, step2, by = "Date")
                  tempdforiginal <- full_join(step1original, step2, by = "Date")
                  
                  mergedData[[varName]] <- tempdf
                  mergedDataOriginal[[varName]] <- tempdforiginal
                }
                
                combinded_df <- bind_rows(mergedData, .id = "df") %>% select(df, Date, continuous_value, discrete_value) 
                combinded_df_original <- bind_rows(mergedDataOriginal, .id = "df") %>% select(df, Date, continuous_value, discrete_value) 
                
                if("USGS gage" %in% input$gageday_discrete){
                  temp <- gageRawData$gagedata %>% select(Date.Time, input$gage_param_discrete) %>% rename("Date" = "Date.Time") %>% 
                    pivot_longer(cols = !Date, names_to = "df", values_to = "continuous_value") %>% 
                    mutate(df = paste0("USGS_gage_", df)) %>% 
                    mutate(discrete_value = NA)
                  
                  combinded_df <- combinded_df %>% bind_rows(temp)
                  combinded_df_original <- combinded_df_original %>% bind_rows(temp)
                }
                
                if("DayMet" %in% input$gageday_discrete){
                  temp <- dayMetRawData$dayMetData %>% 
                    mutate(Date = as.Date(yday, origin = paste(as.character(year - 1), "-12-31", sep = ""))) %>% 
                    select(Date, input$daymet_param_discrete) %>% 
                    pivot_longer(cols = !Date, names_to = "df", values_to = "continuous_value") %>% 
                    mutate(df = case_when(
                      df == "prcp..mm.day." ~ "Daymet_Precipitation (mm)",
                      df == "srad..W.m.2." ~ "Daymet_Shortwave radiation (W m^-2)",
                      df == "swe..kg.m.2." ~ "Daymet_Snow water equivalent (kg m^-2)", 
                      df == "tmax..deg.c." ~ "Daymet_Maximum air temperature (degrees C)", 
                      df == "tmin..deg.c." ~ "Daymet_Minimum air temperature (degrees C)", 
                      df == "vp..Pa." ~"Daymet_Water vapor pressure (Pa)"
                    )) %>% 
                    mutate(discrete_value = NA)
                  
                  combinded_df <- combinded_df %>% bind_rows(temp)
                  combinded_df_original <- combinded_df_original %>% bind_rows(temp)
                }
                
                if(length(setdiff(base_vars_to_plot, variable_to_plot))!=0){
                  temp <- base_data %>% select(date.formatted, setdiff(base_vars_to_plot, variable_to_plot)) %>% 
                    rename("Date" = "date.formatted") %>% 
                    pivot_longer(cols = !Date, names_to = "df", values_to = "continuous_value") %>% 
                    mutate(discrete_value = NA)
                  combinded_df <- combinded_df %>% bind_rows(temp)
                  combinded_df_original <- combinded_df_original %>% bind_rows(temp)
                }
                
                combinded_df$bothValues <- c(paste(
                  "\nContinuous Value: ", combinded_df$continuous_value, "\n",
                  "Discrete Value: ", combinded_df$discrete_value, "\n"
                ))
               
                combinded_df_original$bothValues <- c(paste(
                  "\nContinuous Value: ", combinded_df_original$continuous_value, "\n",
                  "Discrete Value: ", combinded_df_original$discrete_value, "\n"
                ))
                
                mainPlot <- prepareDiscretePlot(combinded_df)
                if (!is.null(mainPlot) & length(variable_to_plot) > 0) {
                  #shinyjs::runjs("$('html, body').animate({scrollTop: $(document).height()},2000)")
                  shinyjs::runjs("$('#dateTimeBoxButton_discrete').click()")
                  output$display_time_series_discrete <- renderPlotly({
                    ggplotly(mainPlot, height = calculatePlotHeight((length(variable_to_plot) + length(input$daymet_param_discrete) + length( input$gage_param_discrete)) * 2), dynamicTicks = TRUE) %>% 
                      plotly::layout(xaxis = list(type = "date")) %>% 
                      style(hovertext = combinded_df$bothValues)
                  })
                }
                
                shinyjs::show(id="discreteDownloadDiv",asis=TRUE)
                
                output$download_discrete <- downloadHandler(
                  filename = function(){
                    paste0("Discrete_continuous_", input$uploaded_discrete_file)
                  },
                  content = function(file){
                    write.csv(combinded_df_original %>% 
                                rename("Parameter" = "df") %>% 
                                dplyr::select(-bothValues) %>% 
                                mutate(Date = format(Date, "%Y-%m-%d %H:%M:%S")), file, row.names = FALSE)
                  }
                )
                
              }
              
            }
            
          } else {
            shinyAlertUI("common_alert_msg", discreteVarMismatch, "ERROR")
            return(mainPlot)
          }
        },
        error = function(parsingMsg) {
          processErrors(parsingMsg, elementId = "disDateAndTimeError")
          
        },
        warning = function(parsingMsg) {
          processErrors(parsingMsg, elementId = "disDateAndTimeError")
        },
        message = function(parsingMsg) {
          processErrors(parsingMsg, elementId = "disDateAndTimeError")
        }
      ) # end of tryCatch
    }
    
  })
  
  # End of Discrete data related functions
  observeEvent(input$dtNumOfCols, {
    if (isolate(input$dtNumOfCols) == "combined") {
      shinyjs::hide("timeFieldDiv")
    } else if (isolate(input$dtNumOfCols) == "separate") {
      shinyjs::show("timeFieldDiv")
    }
  })
  
  
  observeEvent(input$dtNumOfCols1, {
    if (isolate(input$dtNumOfCols1) == "combined") {
      shinyjs::hide("timeFieldDiv2")
    } else if (isolate(input$dtNumOfCols1) == "separate") {
      shinyjs::show("timeFieldDiv2")
    }
  })
  
  observeEvent(input$dtNumOfColsDis, {
    if (isolate(input$dtNumOfColsDis) == "combined") {
      shinyjs::hide("timeFieldDiv3")
    } else if (isolate(input$dtNumOfColsDis) == "separate") {
      shinyjs::show("timeFieldDiv3")
    }
  })
  
  observeEvent(input$toggleLayout, {
    shinyjs::toggle(id = "ts_sidePanel")
    shinyjs::toggle(id = "ts_mainPanel")
  })
  
  changeDateFormat <- function(uploadedDate, uploadedFormat) {
    newFormat <- ""
    print(nchar(uploadedFormat))
    if (nchar(uploadedFormat) > 8) {
      newFormat <- substr(uploadedFormat, 1, 8)
    } else {
      newFormat <- uploadedFormat
    }
    tryCatch(
      {
        tmpDate <- as.Date(uploadedDate, newFormat)
        tmpDateChar <- format(tmpDate, format = "%Y-%m-%d")
        print(tmpDateChar)
      },
      error = function(err) {
        FALSE
      }
    )
  }
  
  #' Prepares discrete data plot object
  #'
  #' @param mergedDataSet 
  #' @param mapTitle 
  #' @param xDateLabel 
  #' @param xDateBrakes 
  #' @param baseVarsToPlot 
  #'
  #' @return mainPlot (object of plot)
  
  prepareDiscretePlot <- function(mergedDataSet) {
    mainPlot <- NULL
    mainPlot <- ggplot(mergedDataSet %>% rename("Parameters" = "df")) +
      geom_line(aes(x= Date, y= continuous_value, color = Parameters)) +
      geom_point(aes(x= Date, y= discrete_value), shape = 21, color = "black", show.legend = FALSE) +
      aes(fill = Parameters)+
      labs(title =  "Discrete and continuous data", x = "Date", y = "Parameters") +
      theme_bw() +
      facet_grid(Parameters ~ ., scales = "free_y") +
      theme(
        strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 10, face = "bold", color = "cornflowerblue"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 5)),
        legend.position = "bottom",
        #axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1)
      )+
      guides(fill = "none")
    return(mainPlot)
  }
  
  #' function for all file uploads
  #'
  #' @param uploadedFile 
  #' @param stopExecution 
  #' @param tab 
  #'
  #' @return my_data
  #'
  uploadFile <- function(uploadedFile, stopExecution = FALSE, tab = "") {
    my_data <- NULL
    otherExtension <- FALSE
    loaded_data$name <- uploadedFile$name
    if (grepl("csv$", uploadedFile$datapath)) {
      my_data <- import_raw_data(uploadedFile$datapath, "csv", has_header = TRUE)
    } else if (grepl("xlsx$", uploadedFile$datapath)) {
      my_data <- import_raw_data(uploadedFile$datapath, "xlsx", has_header = TRUE)
    } else {
      otherExtension <- TRUE
    }
    
    if (!is.null(my_data)) {
      if (tab == "homePage") {
        workflowStatus$finish <- FALSE
        workflowStatus$elementId <- "step1"
        workflowStatus$state <- "success"
      }
      return(my_data)
    } else {
      if (tab == "homePage") {
        workflowStatus$finish <- FALSE
        workflowStatus$elementId <- "step1"
        workflowStatus$state <- "error"
      }
      if (otherExtension == TRUE) {
        shinyAlertUI("common_alert_msg", "Uploaded your data in .csv format.", "ERROR")
      } else {
        shinyAlertUI("common_alert_msg", wrongDataFormat, "ERROR")
      }
      if (stopExecution == TRUE) {
        stop()
      }
    }
  }
  
  observeEvent(input$common_alert_msg, {
    shinyjs::runjs("swal.close();")
  })
  
  shinyAlertUI <- function(id, msg, type) {
    shinyalert(inputId = id, type, msg, closeOnClickOutside = TRUE, closeOnEsc = TRUE, confirmButtonText = "OK")
  }
  
  # mainTab
  observe({
    if (input$mainTabs == "discreateDataEx") {
      renderDiscrete$render <- TRUE
    }
    
    # if (input$mainTabs == "downloadData") {
    #   renderUsgsAndDaymet$render <- TRUE
    # } else if (input$mainTabs == "discreateDataEx") {
    #   renderDiscrete$render <- TRUE
    # }
  })
  
  # hydrology subtabs
  observe({
    if(input$hydro_subtabs == "USGSDaily_tab"){
      renderUSGSDaily$render <- TRUE
    } else if (input$hydro_subtabs == "IHA_tab") {
      renderIHA$render <- TRUE
    } else if (input$hydro_subtabs == "Flashiness_tab") {
      renderFlashiness$render <- TRUE
    }
  })
  
  # temperature subtabs
  observe({
    if (input$temp_subtabs == "sb1") {
      renderThermalStats$render <- TRUE
    } else if (input$temp_subtabs == "sb2") {
      renderAirVsWater$render <- TRUE
    } else if (input$temp_subtabs == "sb3") {
      renderGrowingDegree$render <- TRUE
    } else if (input$temp_subtabs == "sb4") {
      renderThermalClassification$render <- TRUE
    }
     else if(input$temp_subtabs == "sb5"){
       renderTempNTE$render <- TRUE
     }
  })
  
  # All Parameters subtabs
  observe({
    if (input$all_parameters_subtabs == "tab_time_series") {
      renderDataExp$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_summary_tables") {
      renderSummaryTables$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_time_series_overlay") {
      renderTSOverlay$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_box") {
      renderTSBoxPlot$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_CDF") {
      renderCDFPlot$render <- TRUE
    } else if (input$all_parameters_subtabs == "tab_raster") {
      renderRasterPlot$render <- TRUE
    }
  })
  
  observe({
    if (homePageInputs$changed == TRUE & workflowStatus$finish == TRUE) {
      workflowStatus$finish <- FALSE
      workflowStatus$elementId <- "step3"
      workflowStatus$state <- "error"
      readyForCalculation$status <- FALSE
      dailyStatusCalculated$status <- "unfinished"
      # Now reset to false
      homePageInputs$changed <- FALSE
    }
  })
  
}