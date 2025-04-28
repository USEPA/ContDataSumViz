#' Continuous Data Exploration / Temperature / Growing degree days (user interface side)
#'
#' @param id 
#'
GrowingDegreeModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("temp_input")),
              uiOutput(ns("base_input")),
              uiOutput(ns("display_gdd_button")),
              uiOutput(ns("errorDiv"))
              )
          )
      ),
    mainPanel(
      width = 9,
      DT::dataTableOutput(ns("gdd_table"))
    ))
  
  
  #currently sidebarLayout is not needed so commentting it out
  #column(
  #width = 12,
  # sidebarLayout(
  #   sidebarPanel(
  # width = 3,
  # hr(),
  # uiOutput("growing_degree_days_input_1"),
  # hr(),
  # uiOutput("display_growing_degree_days_button"),
  #),
  #mainPanel(
  # width = 11,
      # column(
      #   width = 12,
      #   shinydashboard::box(id=ns("display_help_text_growing_degree_days"), style="display:none;", width=12, class="well",
      #           h4("Temperature – Growing Degree Days"),
      #           div("COMING LATER…"),
      #           div(style="width:100%;", "When resources permit, we will add in a function to calculate Growing Degree Days (GDD), which are used to estimate the growth and development of insects during the growing season.
      #               The basic concept is that development will only occur if the temperature exceeds some minimum development threshold, or base temperature, which varies depending on the type of organisms being studied."),
      #           br(),
      #           div(style="width:100%;", "Although we aren’t able to provide a GDD calculator at this time, one of the RMN partners, Tim Martin from Minnesota DNR (tim.martin@state.mn.us),
      #           has generously shared code that people with R software can use in the meantime. Click below to download the R script."),
      #           div(
      #             id = "hyper_link_panel",
      #             conditionalPanel(
      #               condition = "0==0",
      #               a("Download R script", href = "GDD.R")
      #             ), # conditionalPanel end
      #           ) # div end
      #   ), # end of box
      # )
  # column(
  #   width = 12,
  #   uiOutput("display_growing_degree_days_table")
  # )
  #) # mainPanel end
  #) # sidebarLayout end
  #) # column close


}

#' Continuous Data Exploration / Temperature / Growing degree days (server side)
#'
#' @param id 
#' @param renderGrowingDegree 
#'
GrowingDegreeModuleServer <- function(id, uploaded_data, dailyStats, renderGrowingDegree, loaded_data) {
  
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveVal()
  
  moduleServer(
    id,
    function(input, output, session) {
          ns <- session$ns
          
          observe({
             
             localStats <- dailyStats
             variables_avail <- names(uploaded_data())
             
            if(renderGrowingDegree$render == TRUE) {

              #shinyjs::show(id=ns("display_help_text_growing_degree_days"), asis=TRUE)
              
              output$temp_input <- renderUI({
                selectizeInput(ns("temp_name"),label ="Select Temperature Column",
                               choices=variables_avail,
                               multiple = FALSE,
                               options = list(hideSelected = FALSE))
              })
              
              output$base_input <- renderUI({
                numericInput(ns("base_temp"), label = "Select Base", value = 5, step = 0.1)
              })
              
              output$display_gdd_button <- renderUI({
                actionButton(inputId=ns("display_gdd"), label="Display growing degree days",class="btn btn-primary")
              })

              observeEvent(input$display_gdd, {
                output$errorDiv <- renderUI({})
                
                daily_temp <- localStats$processed_dailyStats[[input$temp_name]]
                years_available <- unique(lubridate::year(daily_temp$Date))
                
                
                tryCatch({
                  gdd <- NULL
                  for(i in 1:length(years_available)){
                    temp_date <- daily_temp %>% dplyr::filter(lubridate::year(Date) %>% as.numeric() == years_available[i])
                    
                    if(nrow(temp_date)< 365 * 0.99){
                      gdd <- c(gdd, "insufficient days")
                    }
                    else{
                      daily_dd <- daily_temp %>% dplyr::filter(lubridate::year(Date) %>% as.numeric() == years_available[i]) %>% 
                        mutate(gdd = case_when(
                          get(paste0(input$temp_name, ".mean")) <= input$base_temp ~ 0,
                          get(paste0(input$temp_name, ".mean")) > input$base_temp ~ get(paste0(input$temp_name, ".mean")) - input$base_temp
                        ))
                      
                      year_dd <- sum(daily_dd$gdd, na.rm = T) %>% round(1) %>% as.character()
                      
                      gdd <- c(gdd, year_dd)
                    }
                  }
                  
                  gdd_df <- data.frame(Year = years_available, GDD = gdd) %>% rename("Growing Degree Days" = GDD)
                  
                  output$gdd_table <- DT::renderDataTable({
                    myTable <- DT::datatable(
                      gdd_df,
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
                                                                          input$temp_name,
                                                                          "_growing-degree-days")),
                                   list(extend = "excel", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                            "_",
                                                                            input$temp_name,
                                                                            "_growing-degree-days")),
                                   list(extend = "pdf", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                          "_",
                                                                          input$temp_name,
                                                                          "_growing-degree-days"))
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
                  
                  
                  if(e$message == "no applicable method for 'filter' applied to an object of class \"NULL\"") {
                    error_append <- "Select a temperature column"
                  } else {
                    error_append <- e$message
                  }
                  
                  errorMsg <- print(paste0("Error in growing degree module ", error_append))
                  
                  output$errorDiv <- renderUI({
                    div(h4(errorMsg), class="alert alert-danger")
                  })
                }
                  
                )

              
              }
              )

            #
            
            } # end of if statement
          })
    }) # end of module server
}
