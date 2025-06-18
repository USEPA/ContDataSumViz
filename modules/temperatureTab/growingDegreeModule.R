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
        shinydashboard::box(id=ns("display_help_text_growing_degree_days"), width=12, class="well",
                h4("Temperature – Growing Degree Days"),
                div(style="width:100%;", "Growing Degree Days (GDD) are used to estimate the growth and development of insects during the growing season.
                    The basic concept is that development will only occur if the temperature exceeds some minimum development threshold, or base temperature, which varies depending on the type of organisms being studied.")
                  ), 
      div(DT::dataTableOutput(ns("gdd_table")), style = "width: 50%; margin: 0 auto;")
    ))
  
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
             #variables_avail <- names(uploaded_data())
             variables_avail <- names(dailyStats$processed_dailyStats)
             
            if(renderGrowingDegree$render == TRUE) {
              
              output$temp_input <- renderUI({
                water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
                possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
                if (length(possible_water_columns)==0){
                  water_to_select <- variables_avail[grep('water',variables_avail,ignore.case=TRUE)][1]
                }else{
                  water_to_select <- possible_water_columns[1]
                }
                selectizeInput(ns("temp_name"),label ="Select temperature column",
                               choices=variables_avail,
                               multiple = FALSE,
                               selected = water_to_select,
                               options = list(hideSelected = FALSE))
              })
              
              output$base_input <- renderUI({
                numericInput(ns("base_temp"), label = "Select base temperature", value = 5, step = 0.1)
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

                runjs(sprintf('document.getElementById("%s").scrollIntoView({ behavior: "smooth" });', ns("gdd_table")))
              }
              )

            #
            
            } # end of if statement
          })
    }) # end of module server
}
