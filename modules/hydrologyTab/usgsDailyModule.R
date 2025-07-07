#' Continuous Data Exploration / Hydrology / Flashiness (user interface side)
#' @param id 
#'
USGSDailyModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput(ns("gage_panel"))
      # div(class="panel panel-default", #,style="margin:10px;"
      #     div(class="panel-heading"),
      #     div(class="panel-body",
      #         uiOutput(ns("gage_panel")),
      #         #uiOutput(ns("errorDiv"))
      #     )
      # )
    ),
    mainPanel(
        width = 9, 
        div(style="width:100%", uiOutput(ns("gageDayMetError"))),
        fluidRow(shinydashboard::box(id=ns("display_help_text_usgsdaily"), width=12, class="well",
                                h4("Download Daily USGS Flow Data"),
                                div(style="width:100%;", "The IHA and Flashiness modules accept data from the user-uploaded file or from flow data downloaded from a USGS gage. If you would like to calculate IHA metrics or flashiness using USGS gage data, download the desired data using in this module. Note that this download is independent of the earlier USGS & Daymet Exploration module, which downloads instantaneous rather than daily values from USGS gages")
                                
        )) , # end of box
        fluidRow(withSpinner(plotlyOutput(ns("display_downloaded_data"))))
      )
      
      
  )
}

#' Continuous Data Exploration / Hydrology / Flashiness (server side)
#'
#' @param id 
#' @param uploaded_data
#' @param dailyStats 
#' @param renderFlashiness 
#'
USGSDailyModuleServer <- function(id, dateRange, uploaded_data,dailyStats,renderUSGSDaily, loaded_data, gageDailyRawData) {

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      gageColNames  <- NULL
      consoleUSGS <- NULL
      # gageDailyRawData <- reactiveValues(gagedata = data.frame(),
      #                               gageColName = as.character())

      output$display_downloaded_data <- renderPlotly({
        plotly_empty()
      })
      
      observe({
        if(renderUSGSDaily$render == TRUE) {
          #shinyjs::show(id=ns("display_help_text_usgsdaily"), asis=TRUE)
          
          output$gage_panel <- renderUI({
            div(class="panel panel-default", style="padding:5px;margin-top:20px;",
                div(class = "panel-heading", style="padding:10px 5px 10px 10px;",
                    span("USGS Gage data", style="font-weight:bold;"),
                    a("View Gage Ids", href="https://waterdata.usgs.gov/nwis/rt", target="_blank", style="float:right")
                ),
                div(style="padding:5px;",
                    textInput(inputId=ns("gage_id"), label="Gage Id",value=""),
                    div(div(dateInput(ns("gage_date_start"),"Date Start",value = dateRange$min %>% as.character(),min="1980-01-01",max="2100-01-01",format="yyyy-mm-dd")),
                        div(dateInput(ns("gage_date_end"),"Date End",value = dateRange$max %>% as.character(),min="1980-01-01",max="2100-01-01",format="yyyy-mm-dd"))),
                    actionButton(inputId=ns("display_gage_ts"), label="Import USGS gage data",class="btn btn-primary")
                ),
                div(id=ns("gageVarsDiv") , style="padding:5px;display:none",
                    selectizeInput(ns("gaze_params"), label ="Select USGS gage variables",
                                   choices=gageColNames,
                                   multiple = TRUE,
                                   selected=NULL,
                                   options = list(hideSelected = FALSE, plugins = list("remove_button"))),
                    actionButton(inputId=ns("display_gage_raw"), label="View USGS raw data",class="btn btn-primary")
                )
            )
          })
          
          observeEvent(input$display_gage_ts, {
            runjs(sprintf('document.getElementById("%s").scrollIntoView({ behavior: "smooth" });', ns("display_downloaded_data")))
            clearContents()
            if(input$gage_id != "" && length(input$gage_id) > 0) {
              #data <- uploaded_data()
              consoleUSGS$disp <- data.frame(consoleOutputUSGS = character())
              Sys.sleep(0.5)
              
              withProgress(message = paste("Getting USGS data"), value = 0, {
                incProgress(0, detail = paste("Retrieving records for site ", input$gage_id))
                #Actually gets the gage data from the USGS NWIS system
                #tryCatch({
                  gageDailyRawData$gagedata <- fun.DailyGageData(
                    myData.SiteID           <- input$gage_id,
                    myData.Type             <- "Gage",
                    myData.DateRange.Start  <- as.character(input$gage_date_start),#as.character(dateRange$min),
                    myData.DateRange.End    <- as.character(input$gage_date_end), #as.character(dateRange$max),
                    myDir.export            <- file.path(".", "data")
                  )
                # }, error = function(err){
                #   print("Error while downloading data from USGS gage")
                #   print(err$message)
                #   renderErrorMsg(err$message)
                #   
                # })
                
                if(class(gageDailyRawData$gagedata)=="character"){
                  renderErrorMsg(paste0("Error downloading USGS gage data. Check user guide for guidance on the following error: ", gageDailyRawData$gagedata, ". Correct before proceeding."))
                }
                
                message("USGS data retrieved")
                #Fills in the progress bar once the operation is complete
                incProgress(1/1, detail = paste("Retrieved records for site ", input$gage_id))
                Sys.sleep(1)
              })
              #browser()
              #print(gageRawData$gagedata)
              allVars <- colnames(gageDailyRawData$gagedata)
              varsToPlot <- allVars[!(allVars %in% c("SiteID","GageID","Date.Time"))]
              gageDailyRawData$gageColName <- varsToPlot
              updateSelectizeInput(session, 'gaze_params', choices = varsToPlot, selected = varsToPlot[1])
              shinyjs::show(id=ns("gageVarsDiv"),asis=TRUE)
              
              #Names the single column of the R console output data.frame
              colnames(consoleUSGS$disp) <- "R console messages for all USGS data retrieval:"
            } else {
              renderErrorMsg(noGageIdFound)
            }
          })
          
          observeEvent(input$display_gage_raw, {
            shinyjs::show(id=ns("display_downloaded_data"), asis=TRUE)
            #shinyjs::hide(id=ns("display_help_text_usgsdaily"), asis=TRUE)
            
            clearContents()
            output$display_downloaded_data <- renderUI({})
            if (input$gage_id != "" & length(input$gage_id) > 0 & nrow(gageDailyRawData$gagedata) > 0 & length(input$gaze_params) > 0) {
              gageRawPlot <- fun.gageDailyRawPlot(fun.gage.data = gageDailyRawData$gagedata,
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
              if (input$gage_id != "" & input$gage_id > 0 & nrow(gageDailyRawData$gagedata) == 0) {
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
          
      }
    })
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
      
})
}