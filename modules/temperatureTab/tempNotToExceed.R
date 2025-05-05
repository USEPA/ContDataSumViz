#' Continuous Data Exploration / Temperature / 4T3 & 6T3 (user interface side)
#' 4T3 Temperature = temperature not to be exceeded for four or more consecutive 
#' hours in a 24-hour period, on more than three consecutive days
#'
#' 6T3 Temperature = temperature not to be exceeded for six or more consecutive 
#' hours in a 24-hour period, on more than three consecutive days
#'
#' Thus, need to calculate the minimum temperature for every 4 (or 6) consecutive hours, 
#' for any 24-hour period (calendar day), 
#' and check whether this occurs on more than 3 consecutive days (within a 4 day period)
#'
#' @param id 
#'
TempNotToExceedUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("temp_input")),
              uiOutput(ns("hour_input")),
              uiOutput(ns("day_input")),
              uiOutput(ns("display_tempExceed_button")),
              uiOutput(ns("errorDiv"))
          )
      )
    ),
    mainPanel(
      width = 9,
      DT::dataTableOutput(ns("tempExceed_table"))
    ))
  
}

#' Continuous Data Exploration / Temperature / Growing degree days (server side)
#'
#' @param id
#' @param renderGrowingDegree 
#'
TempNotToExceedServer <- function(id, uploaded_data, formated_raw_data, renderTempNotToExceed, loaded_data) {
  
  cont_temp_df <- reactiveVal()
  variables_avail <- reactiveVal()
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observe({
        
        cont_temp_df <- formated_raw_data$derivedDF
        variables_avail <- names(uploaded_data())

        
        if(renderTempNotToExceed$render == TRUE) {
          
          output$temp_input <- renderUI({
            selectizeInput(ns("temp_name"),label ="Select Temperature Column",
                           choices=variables_avail,
                           multiple = FALSE,
                           options = list(hideSelected = FALSE))
          })
          
          output$hour_input <- renderUI({
            numericInput(ns("hour_num"), label = "Select number of hours for the time window calculation", value = 4, min = 1, max = 24, step = 1)
          })
          
          output$day_input <- renderUI({
            numericInput(ns("day_num"), label = "Select number of days for the time window calculation", value = 3, min = 1, max = 365, step = 1)
          })
          
          output$display_tempExceed_button <- renderUI({
            actionButton(inputId=ns("display_tempExceed"), label="Display temperature not to exceed",class="btn btn-primary")
          })
          
          observeEvent(input$display_tempExceed, {
            output$errorDiv <- renderUI({})
            
            
            tryCatch({
              # using data.table for faster processing
              # Step 1: Calculate the minimum temperature for every # consecutive hours
              metric_nm <- paste0(input$hour_num, "T", input$day_num)  
              
              cont_temp_dt <- as.data.table(cont_temp_df %>% rename("temp_var" = input$temp_name))
              min_temp_hr <- numeric(nrow(cont_temp_dt))
              
              for(i in 1:nrow(cont_temp_dt)){
                start_time <- cont_temp_dt$date.formatted[i]
                end_time <- start_time + hours(input$hour_num)
                
                min_temp_hr[i] <- cont_temp_dt[date.formatted >= start_time & date.formatted <= end_time, 
                                               min(temp_var, na.rm = TRUE)]
              }
              
              cont_temp_dt$min_temp_hr <- min_temp_hr
              
              # Step 2: Calculate the maximum of the # hour minimum temperature for each calendar day
              cont_data_hr_sum <- cont_temp_dt %>% 
                mutate(date.fm = lubridate::date(date.formatted)) %>% 
                group_by(date.fm) %>% 
                summarize(window_max = max(min_temp_hr)) 
              
              cont_data_hr_sum <- as.data.table(cont_data_hr_sum)
              
              # Step 3: Calculate the minimum temperature every # +1  days
              min_temp_day <- numeric(nrow(cont_data_hr_sum))
              
              for(j in 1:nrow(cont_data_hr_sum)){
                start_date <- cont_data_hr_sum$date.fm[j]
                end_date <- start_date + days(input$day_num + 1)
                
                min_temp_day[j] <- cont_data_hr_sum[date.fm >= start_date & date.fm <= end_date, min(window_max, na.rm = TRUE)]
              }
              
              cont_data_hr_sum$min_temp_day <- min_temp_day
              
              # Step 4
              cont_data_hr_sum$year <- lubridate::year(cont_data_hr_sum$date.fm)
              
              ret_table <- cont_data_hr_sum %>% 
                group_by(year) %>% 
                summarize(year_max = max(min_temp_day), num_days = n()) %>% 
                rename("Year" = "year", !!metric_nm := "year_max", "Number of days" = "num_days")
              
              
              output$tempExceed_table <- DT::renderDataTable({
                myTable <- DT::datatable(
                  ret_table,
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
                                                                      "_",
                                                                      metric_nm)),
                               list(extend = "excel", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                        "_",
                                                                        input$temp_name,
                                                                        "_",
                                                                        metric_nm)),
                               list(extend = "pdf", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                      "_",
                                                                      input$temp_name,
                                                                      "_",
                                                                      metric_nm))
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
              
              errorMsg <- print(paste0("Error in temperature not to exceed module ", error_append))
              
              output$errorDiv <- renderUI({
                div(h4(errorMsg), class="alert alert-danger")
              })
            })

          }
          )
        } # end of if statement
      })
    }) # end of module server
}
