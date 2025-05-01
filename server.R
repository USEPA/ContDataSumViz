server <- function(input, output, session) {
  useShinyjs()
  conflict_prefer("box", "shinydashboard")
  conflict_prefer("dataTableOutput", "DT")
  conflict_prefer("yday", "data.table")
  conflict_prefer("select", "dplyr")
  
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
  renderUsgsAndDaymet <- reactiveValues(render = FALSE)
  
  # Hydrology Tab
  renderIHA <- reactiveValues(render = FALSE)
  renderFlashiness <- reactiveValues(render = FALSE)
  renderDiscrete <- reactiveValues(render = FALSE)
  homePageInputs <- reactiveValues(changed = FALSE)
  
  
  ##############################
  # init shiny modules
  ##############################
  
  # workflow module (step1, step2, step3, step4, step5)
  progressWorkflowModuleServer("statusWorkflow", workflowStatus)
  
  # caluclate daily statistics
  calculateDailyStatsModuleServer("calculateDailyStats", formated_raw_data, homeDTvalues, metaHomeValues, loaded_data, dailyStatusCalculated, processed, readyForCalculation)
  
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
  IHAModuleServer("IHATab", dailyStats = processed, loaded_data, uploaded_data, to_download, renderIHA)
  
  # Continuous Data Exploration >  Hydrology > Flashiness tab
  FlashinessModuleServer("flashinessTab", renderFlashiness)
  
  # USGS & Daymet Exploration tab
  GageAndDaymetModuleServer("gageDaymetAndBase", homeDTvalues, dateRange, formated_raw_data, renderUsgsAndDaymet)
  
  
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
                     choices = c(Head = "head", Tail = "tail", "Column names" = "Column names"),
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
          div(class="panel-heading", "Step 2: Select Date and Time", style="font-weight:bold;"),
          div(class = "panel-body", style = "margin-left: 10px;margin-right: 10px;margin-top: 10px",
              dateAndTimeUI(id = "homePage", paramChoices =  fun.findVariableToProcess(colnames(uploaded_data()), getDateCols = FALSE), uploadedCols = colnames(uploaded_data()))),
          div("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;margin-left: 10px;margin-bottom: 10px"),
          div(actionButton(inputId = "showrawTS", label = "Display time series", class = "btn btn-primary", style = "margin: 10px;")),
          div("To download the plot, mouse over the plot to display the control panel in the upper righthand corner and select the camera icon.", style = "margin-left:10px;margin-bottom: 10px; margin-right:10px;"),
          div(id = "dateAndTimeError")
      )
    })
  })
  
  
  
  # click display time series
  observeEvent(input$showrawTS, {
    
    # shinyjs::show(id = "display_all_raw_ts_div")
    shinyjs::removeClass("dateAndTimeError", "alert alert-danger")
    
    raw_data <- uploaded_data()
    homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data(), homePageInputs)
    
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
        },
        message = function(parsingMsg) {
          processErrors(parsingMsg, tab = "homePage", elementId = "dateAndTimeError")
          workflowStatus$elementId <- "step2"
          workflowStatus$state <- "error"
        }
      ) # end of tryCatch
    } # end of validation check
    

    output$display_subsetTS <- 
      renderUI({
        div(class="panel panel-default", style="margin:10px;",
            div(class="panel-heading", "Step 2b: Subset Time Series (optional)", style="font-weight:bold;"),
            div(div(dateInput("date_start","Date Start:",value = min(formated_raw_data$derivedDF$date.formatted) %>% as.character(),min="1980-01-01",max="2100-01-01",format="yyyy-mm-dd")),
                div(dateInput("date_end","Date End:",value =max(formated_raw_data$derivedDF$date.formatted) %>% as.character(),min="1980-01-01",max="2100-01-01",format="yyyy-mm-dd")), style="margin:10px;"),
            div(actionButton(inputId = "updateTS", label = "Subset data and update time series", class = "btn btn-primary", style = "margin-left: 10px;margin-right: 10px;margin-bottom: 20px;margin-top: 20px;"))
        )
      })
    
    output$display_runmetasummary <-
      renderUI({
        div(class="panel panel-default", style="margin:10px;",
            div(class="panel-heading", "Step 3: Run meta summary", style="font-weight:bold;"),
            div(actionButton(inputId = "runQS", label = "Run meta summary", class = "btn btn-primary", style = "margin-left: 10px;margin-right: 10px;margin-bottom: 20px;margin-top: 20px;"))
        )
      })
    
    output$ts_right <- renderUI({
      div(id = "display_all_raw_ts_div", style = paste0("height:", calculatePlotHeight(length(showRawDateAndTime$parmToProcess()))), rawTSModuleUI("displayRawTS") #"height:100%;width:100%" # time series calculatePlotHeight(length(isolate(input$dailyStats_ts_variable_name)) * 2)
      ) # end of div
    })
  }) ## observeEvent end
  
  observeEvent(input$updateTS,{
    # Updated formated_raw_data
    formated_raw_data$derivedDF <- formated_raw_data$derivedDF %>% dplyr::filter(date.formatted >= input$date_start & date.formatted <= input$date_end)
    
    showRawDateAndTime <- homeDTvalues$homeDateAndTime
    rawTSModuleServer("displayRawTS", showRawDateAndTime, formated_raw_data, loaded_data)
    

  })
  
  # click Run meta summary
  observeEvent(input$runQS, {
    tryCatch(
      {
        raw_data <- uploaded_data()
        
        homeDTvalues$homeDateAndTime <- dateAndTimeServer(id = "homePage", uploaded_data(), homePageInputs)
        localHomeDateAndTime <- homeDTvalues$homeDateAndTime
        # display_validation_msgs dateBox
        if (localHomeDateAndTime$isTimeValid() & localHomeDateAndTime$isDateAndtimeValid()) {
          # output$display_quick_summary_table <- renderUI({
          #    column(12, align = "center", withSpinner(tableOutput("quick_summary_table")))
          #  })
          # update the reactiveValues
          # All the variables are selected
          workflowStatus$elementId <- "step2"
          workflowStatus$state <- "success"
          raw_data <- getFormattedRawData(localHomeDateAndTime, raw_data, tabName = "homePage", errorDivId = "dateAndTimeError")
          raw_data <- raw_data %>% dplyr::filter(date.formatted >= input$date_start & date.formatted <= input$date_end)

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
            metaHomeValues$metaVal <- metaDataServer("metaDataHome", localHomeDateAndTime$parmToProcess(), formatedUploadedData = raw_data, uploadData = uploaded_data())
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
                    div(class="panel-heading", "Step 4: Calculate daily statistics", style="font-weight:bold;"),
                    div(step4UI("metaDataHome"), style = "margin:10px; margin-top:30px"),
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
      },
      message = function(parsingMsg) {
        processErrors(parsingMsg, tab = tabName, elementId = errorDivId)
      }
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
            span("Select Date and Time for discrete data", style = "font-weight:bold;"),
            # span(
            #   actionButton(
            #     inputId = "dateTimeBoxButton_discrete",
            #     style = "float:right;", class = "btn btn-primary btn-xs",
            #     label = "Hide Selection", icon = icon("arrow-down")
            #   )
            # )
          ),
          
          # box(
          #   width = "100%", class = "displayed", id = "dateBox_discrete",
          #   div(
          #     style = "margin-left:10px",
          #     dateAndTimeUI(id = "discretePage", paramChoices = cols_avail, uploadedCols = cols_avail)
          #   ),
          #   hr(style = "margin:0px;padding:0px;"),
          #   fluidRow(
          #     div(span("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;")),
          #     div(
          #       style = "padding:2px;",
          #       span(width = "85%", actionButton(inputId = "display_discrete_data", label = "View Discrete-Continuous Plot", class = "btn btn-primary"), style = "margin:5px 15px 5px 25px;"),
          #       # span("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;")
          #     )
          #   )
          #   # ,
          #   # hr(style = "margin:0px;padding:0px;"),
          #   # fluidRow(
          #   #   column(
          #   #     width = 12,
          #   #     tags$div(
          #   #       renderTable(
          #   #         {
          #   #           fileContentForDisplay
          #   #         },
          #   #         type = "html",
          #   #         bordered = TRUE,
          #   #         striped = TRUE,
          #   #         align = "c",
          #   #         width = "100%"
          #   #       ),
          #   #       style = "overflow-x:auto;"
          #   #     ) # end of div
          #   #   ) # end of column
          #   # ) # end of row
          # ) # end of box
          div(class = "panel-body",
              #width = "100%", class = "displayed", id = "dateBox_discrete",
              div(
                style = "margin-left:10px",
                dateAndTimeUI(id = "discretePage", paramChoices = cols_avail, uploadedCols = cols_avail)
              ),
              hr(),
              #fluidRow(
              
              div("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;margin-left:10px;margin-bottom:10px"),
              div(
                actionButton(inputId = "display_discrete_data", label = "View Discrete-Continuous Plot", class = "btn btn-primary"), style = "margin:10px 15px 10px 10px;"),
              # span("Note: Red border denotes required fields.", style = "font-weight:bold;color:#b94a48;")
              #div(uiOutput("disDateAndTimeError"), style = "margin-left:10px"),
              div(id = "disDateAndTimeError", style = "margin-top:10px; margin-left:10px;")
              #)
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
      label = "Select continuous parameters to process",
      choices = baseParams,
      multiple = TRUE,
      selected = baseParams[1],
      options = list(
        hideSelected = FALSE,
        plugins = list("remove_button")
      )
    )
  })
  
  observeEvent(input$display_discrete_data, {
    discreteDTvalues$disDateAndTime <- dateAndTimeServer(id = "discretePage", uploaded_discreteData(), homePageInputs)
    localDiscreteDateAndTime <- discreteDTvalues$disDateAndTime
    mainPlot <- NULL
    if (localDiscreteDateAndTime$isTimeValid() & localDiscreteDateAndTime$isDateAndtimeValid()) {
      tryCatch(
        {
          variable_to_plot <- sort(localDiscreteDateAndTime$parmToProcess(), decreasing = FALSE)
          base_vars_to_plot <- sort(input$discreteBaseId, decreasing = FALSE)
          if (identical(variable_to_plot, base_vars_to_plot)) {
            discrete_data <- getFormattedRawData(localDiscreteDateAndTime, uploaded_discreteData(), tabName = "", errorDivId = "disDateAndTimeError")
            
            if ("date.formatted" %in% colnames(discrete_data) & nrow(discrete_data) != nrow(discrete_data[is.na(discrete_data$date.formatted), ])) {
              base_data <- formated_raw_data$derivedDF
              if (!is.null(base_vars_to_plot) & nrow(base_data) != nrow(base_data[is.na(base_data$date.formatted), ])) {
                mergedData <- NULL
                for (varName in variable_to_plot) {
                  step1 <- base_data %>% select("continuous_value" = all_of(varName), "Date" = c(date.formatted))
                  step2 <- discrete_data %>% select("discrete_value" = all_of(varName), "Date" = c(date.formatted))
                  step2 <- step2 %>%
                    dplyr::left_join(step1, by = "Date") %>%
                    dplyr::select(discrete_value, "Matching_Continuous_value" = continuous_value, "discrete_Date" = c(Date))
                  
                  timediff <- get_interval(step1$Date)
                  # print(timediff)
                  timediff <- ifelse(timediff == "min", "15 mins", timediff)
                  
                  step1 <- step1 %>%
                    mutate(Date = as.POSIXct(Date)) %>%
                    tidyr::complete(Date = seq(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE), by = timediff))
                  # write.csv(step1,"step1.csv",row.names=FALSE)
                  
                  tempdf <- as.data.frame(qpcR:::cbind.na(step1, step2))
                  mergedData[[varName]] <- tempdf
                }
                combinded_df <- bind_rows(mergedData, .id = "df")
                
                combinded_df$bothValues <- c(paste(
                  "\nContinuous Value: ", combinded_df$Matching_Continuous_value, "\n",
                  "Discrete Value: ", combinded_df$discrete_value, "\n"
                ))
                
                # shared x axis so calculate using base data file
                mainMapTitle <- "Discrete and continuous data"
                main_range <- calculate_time_range(as.list(combinded_df))
                mainBreaks <- main_range[[1]]
                main_x_date_label <- main_range[[2]]
                
                
                mainPlot <- prepareDiscretePlot(combinded_df, mapTitle = mainMapTitle, xDateLabel = main_x_date_label, xDateBrakes = mainBreaks, base_vars_to_plot)
                if (!is.null(mainPlot) & length(variable_to_plot) > 0) {
                  shinyjs::runjs("$('html, body').animate({scrollTop: $(document).height()},2000)")
                  shinyjs::runjs("$('#dateTimeBoxButton_discrete').click()")
                  output$display_time_series_discrete <- renderPlotly({
                    ggplotly(mainPlot, height = calculatePlotHeight(length(variable_to_plot) * 2))
                    # %>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
                  })
                }
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
  
  prepareDiscretePlot <- function(mergedDataSet, mapTitle, xDateLabel, xDateBrakes, baseVarsToPlot) {
    mainPlot <- NULL
    discrete <- mergedDataSet$df
    mainPlot <- ggplot(data = mergedDataSet, dynamicTicks = TRUE, aes(name = bothValues, group = df)) +
      geom_line(inherit.aes = FALSE, aes(x = as.POSIXct(Date), y = continuous_value, colour = df)) +
      geom_point(inherit.aes = TRUE, aes(x = as.POSIXct(discrete_Date), y = discrete_value, shape = discrete, colour = "discrete")) +
      labs(title = mapTitle, x = "Date", y = "Parameters") +
      scale_x_datetime(date_labels = xDateLabel, date_breaks = xDateBrakes) +
      theme_bw() +
      facet_grid(df ~ ., scales = "free_y") +
      scale_color_discrete(name = "continuous") +
      theme(
        strip.background = element_blank(),
        legend.title = element_blank()
        # ,strip.text.y = element_blank()
        , strip.placement = "outside",
        text = element_text(size = 10, face = "bold", color = "cornflowerblue"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1)
      )
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
    if (input$mainTabs == "downloadData") {
      renderUsgsAndDaymet$render <- TRUE
    } else if (input$mainTabs == "discreateDataEx") {
      renderDiscrete$render <- TRUE
    }
  })
  
  # hydrology subtabs
  observe({
    if (input$hydro_subtabs == "IHA_tab") {
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
      # print("I am listening and work flow status is")
      # print(workflowStatus$finish)
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