#' Continuous Data Exploration / All Parameter / Time Series (user interface side)
#' 
#' @param id

DataExplorationTSModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(
        class = "panel panel-default", width = "100%", style = "margin:10px;",
        div(class = "panel-heading"),
        div(
          class = "panel-body",
          uiOutput(ns("time_series_input_1")),
          uiOutput(ns("time_series_input_2")),
          div(
            id = ns("cp_shaded_region"),
            uiOutput(ns("time_series_input_3")),
          ), # div end
          uiOutput(ns("time_series_input_4")),
          hr(),
          fluidRow(
            column(
              width = 9,
              uiOutput(ns("time_series_input_5"))
            )
          ) # fluidRow close,
        ) # end of panel body
      ) # end of panel
    ),
    mainPanel(
      width = 9,
      fluidRow(
        shinydashboard::box(id=ns("summary_tab_help"), width=12, class="well",
                            h4("Any parameters – Daily summary plots"),
                            div(style="width:100%;", "Displays a time series of user-selected daily summary statistics (e.g., mean, median, standard deviation) for any processed continuous parameter, with the option to add percentile or min/max shading."))
      ),
      fluidRow(
        div(style = "width:100%", uiOutput(ns("tsError"))),
      ),
      fluidRow(div(style = "height:800px;overflow-y:auto", uiOutput(ns("display_time_series"))))
    ) # mainPanel end
  ) # sidebarLayout end
}

#' Continuous Data Exploration / All Parameter / Time Series (server side)
#' 
#' @param id 
#' @param dailyStats 
#' @param renderDataExp 
#'
#'
DataExplorationTSModuleServer <- function(id, dailyStats, renderDataExp, loaded_data) {
  localStats <- reactiveValues(stats = list())
  variables_avail <- reactiveValues(params = list())

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        localStats <- dailyStats
        variables_avail$params <- names(localStats$processed_dailyStats)
        localStats$stats <- localStats$processed_dailyStats
        clearPlot()
        clearContents()
        # print(localStats$stats)
      })


      observe({
        if (renderDataExp$render == TRUE) {
          output$time_series_input_1 <- renderUI({
            # variables_avail <- names(localStats$processed_dailyStats)
            selectizeInput(ns("dailyStats_ts_variable_name"),
              label = "Select variable name",
              choices = variables_avail$params,
              multiple = TRUE,
              selected = variables_avail$params[1],
              options = list(hideSelected = FALSE,  plugins = list("remove_button"))
            )
          })
          output$time_series_input_2 <- renderUI({
            selectizeInput(ns("dailyStats_ts_metrics"),
              label = "Select daily statistics metric",
              choices = c("mean", "median", "min", "max", "range", "sd", "var", "cv", "n"),
              multiple = FALSE,
              selected = "mean",
              options = list(hideSelected = FALSE)
            )
          })
          output$time_series_input_3 <- renderUI({
            div(
              radioButtons(ns("dailyStats_shading"), "Add shading",
                choices = c(
                  "No shading" = "noShading",
                  "25th & 75th percentiles" = "quantiles",
                  "Minimum & maximum" = "minMax"
                ),
                selected = "noShading"
              )
            )
          })
          output$time_series_input_4 <- renderUI({
            textInput(inputId = ns("dailyStats_ts_title"), label = "Plot title", value = "")
          })

          output$time_series_input_5 <- renderUI({
            tagList(
              actionButton(inputId = ns("display_ts"), label = "Display", class = "btn btn-primary")
            )
          })
        }
      })

      observeEvent(input$dailyStats_ts_metrics, {
        clearContents()
        clearPlot()
        
        if (!is.null(input$dailyStats_ts_metrics) & (input$dailyStats_ts_metrics == "mean" | input$dailyStats_ts_metrics == "median")) {
          shinyjs::show("cp_shaded_region")
        } else {
          reset(ns("dailyStats_shading"), asis=TRUE)
          shinyjs::hide("cp_shaded_region")
        }
        # click("display_ts")
      })
      
      observeEvent(input$display_ts, {
        clearContents()
        clearPlot()
        localStats <- dailyStats

        if (length(variables_avail$params) > 0 & length(input$dailyStats_ts_variable_name) > 0) {
          
          cols_to_plot <- paste(input$dailyStats_ts_variable_name, input$dailyStats_ts_metrics, sep = ".")
          shading_cols <- map(input$dailyStats_ts_variable_name, function(x) paste(x, c("max", "min", "q.25%", "q.75%"), sep = ".")) %>% unlist()
          
          data_to_plot <- localStats$processed_dailyStats[input$dailyStats_ts_variable_name] %>% 
            reduce(full_join, by = c("SiteID", "Date")) %>% 
            dplyr::select(all_of(c("SiteID", "Date", cols_to_plot, shading_cols))) %>% 
            pivot_longer(cols = all_of(c(cols_to_plot, shading_cols)), names_to = "Statistic",  values_to = "Value") %>% 
            mutate(Parameter = str_remove(Statistic, paste(c(paste0(".", input$dailyStats_ts_metrics), ".max", ".min", ".q.25%", ".q.75%"), collapse = "|")),
                   Statistic = str_remove(Statistic, paste0(Parameter, "."))) %>% 
            pivot_wider(names_from = Statistic, values_from = Value)
          
          g <-  ggplot(data_to_plot, aes(x = Date, group = Parameter))+
            theme_bw()+
            theme(
              strip.background = element_blank()
              ,strip.placement = "outside"
              ,axis.title.y = element_blank()
              ,text=element_text(size=12,face = "bold", color="cornflowerblue")
              ,strip.text.y = element_blank()
              ,plot.title = element_text(hjust=0.5)
              ,plot.subtitle = element_text(hjust=0.5, size = 9))
         
           if(input$dailyStats_shading == "noShading"){
             g <- g +  
               geom_line(aes(y = !!sym(input$dailyStats_ts_metrics), color = Parameter))+ 
               labs(title = input$dailyStats_ts_title)+
               facet_grid(Parameter ~ ., scales = "free_y")
           }
          if(input$dailyStats_shading == "minMax"){
            g <- g + 
              geom_ribbon(aes(ymin = min, ymax = max, fill = Parameter), color = NA, alpha = 0.5)+
              geom_line(aes(y = !!sym(input$dailyStats_ts_metrics), color = Parameter))+ 
              labs(title = input$dailyStats_ts_title, subtitle = "(Shading between daily min and max)")+
              facet_grid(Parameter ~ ., scales = "free_y")
          }
          if(input$dailyStats_shading == "quantiles"){
            g <- g + 
              geom_ribbon(aes(ymin = `q.25%`, ymax = `q.75%`, fill = Parameter), color = NA, alpha = 0.5)+
              geom_line(aes(y = !!sym(input$dailyStats_ts_metrics), color = Parameter))+ 
              labs(title = input$dailyStats_ts_title, subtitle = "(Shading between daily 25th and 75th percentiles)")+
              facet_grid(Parameter ~ ., scales = "free_y")
          }
   
          ggplot(data_to_plot, aes(x = Date, group = Parameter))+
            theme_bw()+
            theme(
              strip.background = element_blank()
              ,strip.placement = "outside"
              ,axis.title.y = element_blank()
              ,text=element_text(size=12,face = "bold", color="cornflowerblue")
              ,strip.text.y = element_blank()
              ,plot.title = element_text(hjust=0.5)
              ,plot.subtitle = element_text(hjust=0.5, size = 9)) + 
            geom_ribbon(aes(ymin = min, ymax = max, fill = Parameter), color = NA, alpha = 0.5)+
            #geom_line(aes(y = !!sym(input$dailyStats_ts_metrics), color = Parameter))+ 
            labs(title = input$dailyStats_ts_title, subtitle = "(Shading between daily min and max)")+
            facet_grid(Parameter ~ ., scales = "free_y")
          
            
          output$display_time_series <- renderUI({
            if(input$dailyStats_shading !="noShading"){
              plotOutput(ns("display_time_series_static"))
            } else{
              plotlyOutput(ns("display_time_series_plotly"))
            }
          })
          
          
          if(input$dailyStats_shading!="noShading"){
            output$display_time_series_static <- renderPlot(
             g, height = calculatePlotHeight(length(input$dailyStats_ts_variable_name) * 2)
            )
          }
          
          if(input$dailyStats_shading=="noShading"){
            output$display_time_series_plotly <- renderPlotly({
              ggplotly(g, height = calculatePlotHeight(length(input$dailyStats_ts_variable_name) * 2), dynamicTicks = TRUE) %>% 
                     plotly::layout(xaxis = list(type = "date")) %>% 
                     plotly::config(toImageButtonOptions = list(format = "png", 
                                                                filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_dailyTS", "_", input$dailyStats_ts_metrics, "_", input$dailyStats_shading)))
            })
          }
        }
      })
    
      clearContents <- function() {
        output$tsError <- renderUI({})
      }

      clearPlot <- function() {
        output$display_time_series <- renderPlotly({
          plotly_empty()
        })
      }
    }
  ) # end of module server
}
