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
              uiOutput(ns("flash_input")),
              uiOutput(ns("display_flash_button")),
              uiOutput(ns("errorDiv"))
          )
      )
    ),
    mainPanel(
      width = 9, 
      shinydashboard::box(id=ns("display_help_text_flashiness"), style="display:none;", width=12, class="well",
                                                h4("Hydrology – Flashiness"),
                                                div(style="width:100%;", "The Richards-Baker flashiness index (RBI) (Baker et al. 2004) reflects the frequency and rapidity of short-term changes in streamflow.
                                                    It measures oscillations in discharge relative to total discharge, such that flashier streams receive higher scores.
                                                    Results are scaled from 0 to 1 (most flashy)."),
                                                br(),
                                                div(style="width:100%;", "The calculation is based on mean daily flows and is calculated by dividing the sum of the absolute values of day-to-day changes in mean daily flow by total discharge during the specified time period."),
                                                br(),
                                                div(style="width:100%;", "The Shiny app calculation is based on mean daily values and calendar year. Those settings can be changed if you are using the R package instead of Shiny app.
                                                    For more information, contact Erik Leppo (Erik.Leppo@tetratech.com)."),
                                                br(),
                                                div(style="width:100%;", "The RBI is intended to be used with discharge data but we’re experimenting with using it with sensor depth and water level data as well (since discharge data aren’t available for some of the RMN sites)."),
                                                br(),
                                                div(style="width:100%;", "Citation:"),
                                                div(style="width:100%;",    "Baker, D.B., Richards, R.P., Loftus, T.T. and J.K. Kramer. 2004. A New Flashiness Index: Characteristics and Applications to Midwestern Rivers and Streams.
                                                    Journal of the American Water Resources Association 40(2): 503-522."),
                                                a('https://doi.org/10.1111/j.1752-1688.2004.tb01046.x', href='https://doi.org/10.1111/j.1752-1688.2004.tb01046.x', target='_blank')

                            ), # end of box
      DT::dataTableOutput(ns("flash_table"))
      
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
FlashinessModuleServer <- function(id, uploaded_data,dailyStats,renderFlashiness, loaded_data) {
  
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveVal()
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        
        localStats <- dailyStats
        variables_avail <- names(uploaded_data())
        
        if(renderFlashiness$render == TRUE) {
          shinyjs::show(id=ns("display_help_text_flashiness"), asis=TRUE)
          
          output$flash_input <- renderUI({
            selectizeInput(ns("flash_name"),label ="Select Variable",
                           choices=variables_avail,
                           multiple = FALSE,
                           options = list(hideSelected = FALSE))
          })
          
          
          output$display_flash_button <- renderUI({
            actionButton(inputId=ns("display_RBindex"), label="Display RB Index",class="btn btn-primary")
          })
          
          
            observeEvent(input$display_RBindex, {
            output$errorDiv <- renderUI({})
            shinyjs::hide(id=ns("display_help_text_flashiness"), asis=TRUE)

            daily_Value <- localStats$processed_dailyStats[[input$flash_name]]
            years_available <- unique(lubridate::year(daily_Value$Date))

           
            tryCatch({
              
              DailyChange_df <- daily_Value %>% 
                select(Date, paste0(input$flash_name, ".mean")) %>% 
                mutate(Year = lubridate::year(Date)) %>% 
                rename("mean" = paste0(input$flash_name, ".mean")) %>% 
                mutate(DailyChangeValue = abs(mean - dplyr::lag(mean))) %>% 
                mutate(DailyChangeValue = replace_na(DailyChangeValue, 0))
              
              RB_df <- DailyChange_df %>% 
                group_by(Year) %>% 
                summarize(RB_Index = (sum(DailyChangeValue)/sum(mean)) %>% round(3))

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
                               list(extend = "csv", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                      "_",
                                                                      input$flash_name,
                                                                      "_RB-Index")),
                               list(extend = "excel", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                        "_",
                                                                        input$flash_name,
                                                                        "_RB-Index")),
                               list(extend = "pdf", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                      "_",
                                                                      input$flash_name,
                                                                      "_RB-Index"))
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
            }) 
        }
      })
    })
}
