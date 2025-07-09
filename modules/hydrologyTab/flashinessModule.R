#' Continuous Data Exploration / Hydrology / Flashiness (user interface side)
#' @param id 
#'
FlashinessModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
   sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("flash_input_1")),
              uiOutput(ns("flash_input_2")),
              uiOutput(ns("display_flash_button")),
              uiOutput(ns("errorDiv"))
          )
      )
    ),
    mainPanel(
      width = 9, 
      #shinydashboard::box(id=ns("display_help_text_flashiness"), style="display:none;", width=12, class="well",
      shinydashboard::box(id=ns("display_help_text_flashiness"), width=12, class="well",
                                                h4("Hydrology – Flashiness"),
                                                div(style="width:100%;", "The Richards-Baker flashiness index (RBI) (Baker et al. 2004) reflects the frequency and rapidity of short-term changes in streamflow. It measures oscillations in discharge relative to total discharge. Flashier streams receive higher scores."),
                                                br(),
                                                div(style="width:100%;", "The calculation is made by dividing the sum of the absolute values of day-to-day changes in mean daily flow by total discharge during the specified time period. The reported number of days used for the calculation represents the number of days with data on the preceeding day, for which a day-to-day change can be calculated."),
                                                br(),
                                                div(style="width:100%;", "The Shiny app calculation is based on mean daily values and calendar year."),
                                                br(),
                                                div(style="width:100%;", "The RBI is intended to be used with discharge data but we’re experimenting with also using it with sensor depth and water level data (since discharge data aren’t available for some of the RMN sites)."),
                                                br(),
                                                div(style="width:100%; font-weight:bold;", "Citation:"),
                                                div(style="width:100%;",    "Baker, D.B., Richards, R.P., Loftus, T.T., Kramer, J.K. 2004. A New Flashiness Index: Characteristics and Applications to Midwestern Rivers and Streams.
                                                    Journal of the American Water Resources Association 40 (2): 503-522."),
                                                a('https://doi.org/10.1111/j.1752-1688.2004.tb01046.x', href='https://doi.org/10.1111/j.1752-1688.2004.tb01046.x', target='_blank')

                            ), # end of box
      div(DT::dataTableOutput(ns("flash_table")), style = "width: 50%; margin: 0 auto;")
      
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
FlashinessModuleServer <- function(id, uploaded_data,dailyStats,renderFlashiness, loaded_data, gageDailyRawData) {
  
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveVal()
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        
        localStats <- dailyStats
        
        if(renderFlashiness$render == TRUE) {
          #shinyjs::show(id=ns("display_help_text_flashiness"), asis=TRUE)
          
          output$flash_input_1 <- renderUI({
            radioButtons(ns("flash_data_source"), label = "Select flow data source", 
                         choices = c("Uploaded data", "Downloaded USGS gage data"),
                         selected = "Uploaded data")
          })
          
          observeEvent(input$flash_data_source,{
            if(input$flash_data_source == "Uploaded data"){
              variables_avail <- names(localStats$processed_dailyStats)
              parameter_to_select <- variables_avail[grep('Discharge',variables_avail,ignore.case=TRUE)][1]
              
              output$flash_input_2 <- renderUI({ 
                selectizeInput(ns("flash_name"),label ="Select uploaded flow parameter",
                               choices=variables_avail,
                               multiple = FALSE,
                               selected= parameter_to_select,
                               options = list(hideSelected = FALSE))
              })
            }
            if(input$flash_data_source == "Downloaded USGS gage data"){
              if(identical(gageDailyRawData$gageColName, character(0))){
                shinyalert("Download USGS Data", "Please download data in the Download USGS daily flow tab to access this feature.", "warning")
              } 
              output$flash_input_2 <- renderUI({selectizeInput(ns("flash_name"), label = "Select downloaded USGS flow parameter",
                                                             choices = gageDailyRawData$gageColName,
                                                             multiple = FALSE)})
              
            }
            
          })
          
          output$display_flash_button <- renderUI({
            actionButton(inputId=ns("display_RBindex"), label="Display flashiness",class="btn btn-primary")
          })
          

            observeEvent(input$display_RBindex, {
            output$errorDiv <- renderUI({})
            #shinyjs::hide(id=ns("display_help_text_flashiness"), asis=TRUE)
            
            if(input$flash_data_source == "Uploaded data"){
              daily_Value <- localStats$processed_dailyStats[[input$flash_name]]%>% 
                select(Date, paste0(input$flash_name, ".mean")) %>% 
                mutate(Year = lubridate::year(Date)) %>% 
                rename("mean" = paste0(input$flash_name, ".mean")) 
                #%>% dplyr::filter(is.na(mean) == FALSE)
              years_available <- unique(daily_Value$Year)
              
              flash_output_file_str <- paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                        "_",
                                        input$flash_name,
                                        "_RB-Index")
            }
            
            if(input$flash_data_source == "Downloaded USGS gage data"){
              daily_Value <- gageDailyRawData$gagedata %>% 
                dplyr::select(Date.Time, input$flash_name) %>% 
                dplyr::rename("Date" = "Date.Time", "mean" = input$flash_name) %>% 
                mutate(Year = lubridate::year(Date)) 
                #%>% dplyr::filter(is.na(mean) == FALSE)
              years_available <- unique(daily_Value$Year)
              
              flash_output_file_str <- paste0("USGS_gage_", unique(unique(gageDailyRawData$gagedata$GageID)), "_", input$flash_name, "_RB-Index")
            }
            
            tryCatch({
              DailyChange_df <- daily_Value  %>% 
                mutate(DailyChangeValue = abs(mean - dplyr::lag(mean)))
              #%>%  mutate(DailyChangeValue = replace_na(DailyChangeValue, 0))
              
              RB_df <- DailyChange_df %>% 
                dplyr::filter(is.na(DailyChangeValue) == FALSE) %>% # remove daily change NA, which occurs when mean is NA or lag 1 mean is NA
                group_by(Year) %>% 
                summarize(RB_Index = (sum(DailyChangeValue, na.rm = TRUE)/sum(mean, na.rm = TRUE)) %>% round(3), 
                          n_days = sum(!is.na(DailyChangeValue))) %>% 
                rename("RB Index" = "RB_Index",
                       "Number of days" = "n_days")

              
              output$flash_table <- DT::renderDataTable({
                myTable <- DT::datatable(
                  RB_df,
                  extensions ="Buttons",
                  rownames = FALSE,
                  options = list(
                    scrollX = FALSE, #allow user to scroll wide tables horizontally
                    stateSave = FALSE,
                    pageLength = 15,
                    dom = 'Bt',
                    buttons = list(
                      list(extend='copy', text='Copy', className="btn btn-primary"),
                      list(extend='print', text='Print', className="btn btn-primary"),
                      list(extend='collection', buttons =
                             list(
                               list(extend = "csv", filename = flash_output_file_str),
                               list(extend = "excel", filename = flash_output_file_str),
                               list(extend = "pdf", filename = flash_output_file_str)
                             ),text='Download', className="btn btn-primary")
                    ),
                    columnDefs = list(list(className="dt-center",targets="_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#e3e3e3', 'color': '#000'});",
                      "$('.dt-buttons button').removeClass('dt-button');",
                      "}")
                  )
                ) # dataTable end
                print(myTable)
              })  ## renderDataTable end
            },
            error = function(e){

              if(e$message == "no applicable method for 'select' applied to an object of class \"NULL\"") {
                error_append <- "Select a flow column"
              } else {
                error_append <- e$message
              }

              errorMsg <- print(paste0("Error in flashiness module ", error_append))

              output$errorDiv <- renderUI({
                div(h4(errorMsg), class="alert alert-danger")
              })
            } # end error
            )
            runjs(sprintf('document.getElementById("%s").scrollIntoView({ behavior: "smooth" });', ns("flash_table")))
            }) 
        }
      })
    })
}
