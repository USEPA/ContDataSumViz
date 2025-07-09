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
              uiOutput(ns("summary_window")),
              uiOutput(ns("display_tempExceed_button")),
              uiOutput(ns("errorDiv"))
          )
      )
    ),
    mainPanel(
      width = 9,
      shinydashboard::box(id=ns("display_help_text_not_to_exceed"), width=12, class="well",
                          h4("Temperature – Temperature not to exceed"),
                          div(style="width:100%;", "This module was developed to support 4T3 and 6T3 calculations used in New Mexico water quality criteria."),
                          div(style="width:100%;", "4T3 Temperature = temperature not to be exceeded for four or more consecutive hours in a 24-hour period, on more than three consecutive days."),
                          div(style="width:100%;", "6T3 Temperature = temperature not to be exceeded for six or more consecutive hours in a 24-hour period, on more than three consecutive days."),
                          div(style="width:100%;", "This module allows the user to select the hour window (i.e., 4 for 4T3 or 6 for 6T3) and number of consecutive days used in the calculation (i.e., 3 for both 4T3 and 6T3)."),
                          div(style="width:100%", "The table returns the temperature not to exceed calculated for the selected time period and the number of days in that period with sufficient data to include in the calculation."),
                          br(),
                          div(style = "width:100%", "For more information about the 4T3 and 6T3 calculations visit:"),
                          a('https://www.env.nm.gov/surface-water-quality/wp-content/uploads/sites/25/2019/10/Air-Water08-01-2011.pdf', href='https://www.env.nm.gov/surface-water-quality/wp-content/uploads/sites/25/2019/10/Air-Water08-01-2011.pdf', target='_blank'),
                          br(),
                          br(),
                          div(style="width:100%", "The moving window calculation for temperature not to exceed is computationally intensive. Results may take one minute or more to generate.")
      ), 
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
            water_keys_in_favor_order <- c("Water.Temp.C","WATER.TEMP.C","Water_Temp_C","WATER_TEMP_C")
            possible_water_columns <- water_keys_in_favor_order[water_keys_in_favor_order %in% variables_avail]
            if (length(possible_water_columns)==0){
              water_to_select <- variables_avail[grep('temp',variables_avail,ignore.case=TRUE)][1]
            }else{
              water_to_select <- possible_water_columns[1]
            }
            
            selectizeInput(ns("temp_name"),label ="Select temperature column",
                           choices=variables_avail,
                           multiple = FALSE,
                           selected = water_to_select,
                           options = list(hideSelected = FALSE))
          })
          
          output$hour_input <- renderUI({
            numericInput(ns("hour_num"), label = "Select number of hours for the time window calculation", value = 4, min = 1, max = 24, step = 1)
          })
          
          output$day_input <- renderUI({
            numericInput(ns("day_num"), label = "Select number of days for the time window calculation", value = 3, min = 1, max = 365, step = 1)
          })
          
          output$summary_window <- renderUI({
            radioButtons(ns("summarise_by"), "Summarize by", choices = c("year/month"="year/month"
                                                                         ,"year"="year"
                                                                         ,"year/season"="year/season"
                                                                         ,"season"="season"),
                         selected = "year/month")
          })
          
          
          output$display_tempExceed_button <- renderUI({
            actionButton(inputId=ns("display_tempExceed"), label="Display temperature not to exceed",class="btn btn-primary")
          })
          
          observeEvent(input$display_tempExceed, {
            output$errorDiv <- renderUI({})
            
            if(class(cont_temp_df %>% rename("temp_var" = input$temp_name) %>% pull(temp_var)) == "character"){
              output$errorDiv <- renderUI({div(h4("Select numeric column"), class="alert alert-danger")})
            } else{
              tryCatch({
                # using data.table for faster processing
                # Step 1: Calculate the minimum temperature for every # consecutive hours
                
                metric_nm <- paste0(input$hour_num, "T", input$day_num)  
                
                cont_temp_dt <- as.data.table(cont_temp_df %>% rename("temp_var" = input$temp_name))
                
                index_time <- cont_temp_dt$date.formatted[1] + hours(input$hour_num)
                df_start <- cont_temp_dt %>% dplyr::filter(date.formatted < index_time) %>% nrow()
                
                min_temp_hr <- NULL
                
                for(i in df_start:nrow(cont_temp_dt)){
                  end_time <-  cont_temp_dt$date.formatted[i]
                  start_time <- end_time - hours(input$hour_num)
                  
                  temp <- ifelse(all(is.na((cont_temp_dt[date.formatted > start_time & date.formatted <= end_time]$temp_var))), NA, cont_temp_dt[date.formatted > start_time & date.formatted <= end_time, min(temp_var, na.rm = TRUE)]) 
                  
                  min_temp_hr <- c(min_temp_hr, temp)
                }
                
                cont_temp_dt$min_temp_hr <- c(rep(NA, df_start-1), min_temp_hr) 
                
                # Step 2: Calculate the maximum of the # hour minimum temperature for each calendar day
                cont_data_hr_sum <- cont_temp_dt %>% 
                  mutate(date.fm = lubridate::date(date.formatted)) %>% 
                  group_by(date.fm) %>% 
                  summarize(window_max = ifelse(all(is.na(min_temp_hr)), NA, max(min_temp_hr, na.rm = TRUE))) 
                
                cont_data_hr_sum <- as.data.table(cont_data_hr_sum)
                
                # Step 3: Calculate the minimum temperature every # +1  days
                min_temp_day <- NULL
                
                for(j in (input$day_num + 1):nrow(cont_data_hr_sum)){
                  end_date <- cont_data_hr_sum$date.fm[j]
                  start_date <- end_date - days(input$day_num)
                  
                  temp <- ifelse(all(is.na(cont_data_hr_sum[date.fm >= start_date & date.fm <= end_date]$window_max)),NA,cont_data_hr_sum[date.fm >= start_date & date.fm <= end_date, min(window_max, na.rm = TRUE)])
                  min_temp_day <- c(min_temp_day, temp)
                }
                
                cont_data_hr_sum$min_temp_day <- c(rep(NA, input$day_num), min_temp_day)
                
                # Step 4: Summarize by year
                cont_data_hr_sum$year <- lubridate::year(cont_data_hr_sum$date.fm)
                
                cont_data_hr_sum <- cont_data_hr_sum %>% mutate(year = lubridate::year(date.fm),
                                                                month = lubridate::month(date.fm), 
                                                                season = case_when(
                                                                  month %in% c(12,1,2) ~ "Winter",
                                                                  month %in% c(3:5) ~ "Spring",
                                                                  month %in% c(6:8) ~ "Summer",
                                                                  month %in% c(9:11) ~ "Fall"
                                                                ))
                
                if(input$summarise_by == "year/month"){
                  ret_table <- cont_data_hr_sum %>% 
                    group_by(year, month) %>% 
                    summarize(sum_max = max(min_temp_day, na.rm = TRUE)%>% round(3),, num_days = sum(!is.na(min_temp_day))) %>% 
                    rename("Year" = "year", "Month" = "month", !!metric_nm := "sum_max", "Number of days" = "num_days")
                }
                
                if(input$summarise_by == "year"){
                  ret_table <- cont_data_hr_sum %>% 
                    group_by(year) %>% 
                    summarize(sum_max = max(min_temp_day, na.rm = TRUE)%>% round(3),, num_days = sum(!is.na(min_temp_day))) %>% 
                    rename("Year" = "year", !!metric_nm := "sum_max", "Number of days" = "num_days")
                }
                
                if(input$summarise_by == "year/season"){
                  ret_table <- cont_data_hr_sum %>% 
                    group_by(year, season) %>% 
                    summarize(sum_max = max(min_temp_day, na.rm = TRUE)%>% round(3),, num_days = sum(!is.na(min_temp_day))) %>% 
                    rename("Year" = "year", "Season" = "season", !!metric_nm := "sum_max", "Number of days" = "num_days")
                }
                
                if(input$summarise_by == "season"){
                  ret_table <- cont_data_hr_sum %>% 
                    group_by(season) %>% 
                    summarize(sum_max = max(min_temp_day, na.rm = TRUE) %>% round(3), num_days = sum(!is.na(min_temp_day))) %>% 
                    rename("Season" = "season", !!metric_nm := "sum_max", "Number of days" = "num_days")
                }
                
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
                                                                        metric_nm, 
                                                                        "_",
                                                                        input$summarise_by)),
                                 list(extend = "excel", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                          "_",
                                                                          input$temp_name,
                                                                          "_",
                                                                          metric_nm,
                                                                          "_",
                                                                          input$summarise_by)),
                                 list(extend = "pdf", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                        "_",
                                                                        input$temp_name,
                                                                        "_",
                                                                        metric_nm,
                                                                        "_",
                                                                        input$summarise_by))
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
            
            
            
            runjs(sprintf('document.getElementById("%s").scrollIntoView({ behavior: "smooth" });', ns("tempExceed_table")))

          }
          )
        } # end of if statement
      })
    }) # end of module server
}
