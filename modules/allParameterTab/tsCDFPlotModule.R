#' Continuous Data Exploration / All Parameter / CDFs tab (user interface side)
#'
#' @param id 
#'
TsCDFPlotModuleUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("CDF_input_1")),
              uiOutput(ns("CDF_input_2")),
              uiOutput(ns("CDF_input_4")),
              uiOutput(ns("CDF_input_5")),
              uiOutput(ns("CDF_input_3")),
              hr(),
              uiOutput(ns("display_CDF_button"))
          )#end of panel body
      )#end of panel
    ),
    mainPanel(
      width = 9,
      column(
        width = 12, 
        fluidRow(shinydashboard::box(id=ns("cdf_tab_help"), width=12, class="well",
                                     h4("Any parameters – CDFs"),
                                     div(style="width:100%;", "Displays the annual cumulative distribution function (CDF) of daily means of any processed continuous parameter, with the option to add percentile or min/max shading. Seasons are defined as: Winter (December, January, February), Spring (March, April, May), Summer (June, July, August), Fall (September, October, November). The cumulative distribution function is sensitive to the amount of available data. Refer to the Days of available data table in the Summary tables tab to determine the number of points contributing to each trace."))),
        fluidRow(div(style="width:100%", uiOutput(ns("cdfError")))),
        fluidRow(div(uiOutput(ns("display_plot_CDF")))) #style = "height:800px;", 
      )
      
    ) # mainPanel end
  ) # sidebarLayout end
}

#' Continuous Data Exploration / All Parameter / CDFs tab (server side)
#'
#' @param id 
#' @param dailyStats 
#' @param renderCDFPlot 
#'
TsCDFPlotModuleServer <- function(id, dailyStats, renderCDFPlot, loaded_data) {
  
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveValues(params=list())
  variable_to_plot <- reactiveVal()
  
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observe({
        localStats <- dailyStats
        variables_avail$params <- names(localStats$processed_dailyStats)
        localStats$stats <- localStats$processed_dailyStats
        #print(localStats$stats)
      })
      
      observe({
        
        localStats <- dailyStats
        myList <- localStats$processed_dailyStats
        variables_avail$params <- names(localStats$processed_dailyStats)
        
        
        if(renderCDFPlot$render == TRUE) {
          output$CDF_input_1 <- renderUI({
            selectizeInput(ns("CDF_variable_name"),label ="Select variable name",
                           choices=variables_avail$params,
                           multiple = FALSE,
                           selected=variables_avail$params[1],
                           options = list(hideSelected = FALSE))
          })
          
          
          output$CDF_input_2 <- renderUI({
            div(
              radioButtons(ns("CDF_shading"), "Add shading", choices = c(
                "No shading" = "noShading",
                "25th & 75th percentiles"="quantiles",
                "Minimum & maximum"="minMax"
              ),
              selected = "minMax"))
            
          })
          
          # output$CDF_input_3 <- renderUI({
          #   numericInput(inputId=ns("cdf_plot_aspect_ratio"), label="Adjust plot aspect ratio",7.0,min=0,max=10,step=0.1)
          #   
          # })
          
          # output$CDF_input_3 <- renderUI({
          #   variable_to_plot <- ifelse(is.null(input$CDF_variable_name), variables_avail$params[1], input$CDF_variable_name)
          #   myData.all <- myList[[which(names(myList)==variable_to_plot)]]
          #   myData.all[,"year"] <- format(myData.all[,"Date"],"%Y")
          #   selectizeInput(ns("CDF_select_year"),label ="Select year",
          #                  choices=c("All", unique(myData.all[,"year"])),
          #                  multiple = FALSE,
          #                  selected = "All",
          #                  options = list(hideSelected = FALSE))
          # })
          
          output$CDF_input_4 <- renderUI({
            selectizeInput(ns("CDF_select_season"),label ="Select season",
                           choices=c("All","Fall", "Winter", "Spring","Summer" ),
                           multiple = FALSE,
                           selected = "All",
                           options = list(hideSelected = FALSE))
          })
          
          output$CDF_input_5 <- renderUI({
            
            textInput(inputId=ns("CDF_title"), label="Plot title",value="")
            
          })
          
          output$display_CDF_button <- renderUI({
            actionButton(inputId=ns("run_CDF"), label="Display",class="btn btn-primary")
          })
          
        }
      })
      
      observeEvent(input$run_CDF, {
        localStats <- dailyStats
        clearContents()
        clearPlot()
        
        #browser()
        
        myList <- localStats$processed_dailyStats
        variable_to_plot <- input$CDF_variable_name
        myData.all <- myList[[which(names(myList)==variable_to_plot)]]
        myData <- myData.all %>% 
          mutate(Year = lubridate::year(Date),
                 Month = lubridate::month(Date),
                 Season = case_when(
                   Month %in% c(12,1,2) ~ "Winter",
                   Month %in% c(3,4,5) ~ "Spring",
                   Month %in% c(6,7,8) ~ "Summer", 
                   Month %in% c(9,10,11) ~ "Fall"))

        mean_col <- paste0(input$CDF_variable_name,".mean")
        
        if (input$CDF_shading=="quantiles"){
          upper_col <- paste0(input$CDF_variable_name,".q.75%")
          lower_col <- paste0(input$CDF_variable_name,".q.25%")
        } else if (input$CDF_shading=="minMax"){
          lower_col <- paste0(input$CDF_variable_name,".min")
          upper_col <- paste0(input$CDF_variable_name,".max")
        } else if(input$CDF_shading=="noShading"){ # need something to generalize calculation but not plotted
          lower_col <- paste0(input$CDF_variable_name,".min")
          upper_col <- paste0(input$CDF_variable_name,".max")
        }
        
        if(input$CDF_select_season == "All"){
          data_to_plot <- myData %>% 
            mutate(Year = as.factor(Year)) %>% 
            group_by(Year) %>% 
            mutate(
              ecdf_mean = ecdf(!!sym(mean_col))(!!sym(mean_col)),
              ecdf_min = ecdf(!!sym(upper_col))(!!sym(mean_col)),
              ecdf_max = ecdf(!!sym(lower_col))(!!sym(mean_col)))
        } else{
          data_to_plot <- myData %>% 
            dplyr::filter(Season == input$CDF_select_season) %>% 
            mutate(Year = as.factor(Year)) %>% 
            group_by(Year) %>% 
            mutate(
              ecdf_mean = ecdf(!!sym(mean_col))(!!sym(mean_col)),
              ecdf_min = ecdf(!!sym(upper_col))(!!sym(mean_col)),
              ecdf_max = ecdf(!!sym(lower_col))(!!sym(mean_col)))
        }

        
        
        g <- ggplot(data_to_plot, aes(x = !!sym(mean_col)))+
          geom_line(aes(y = ecdf_mean, color = Year))+
          labs(y = "Proportion ≤ value", title = paste(input$CDF_select_season, input$CDF_title, sep = " "))+
          theme_classic()+
          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                ,plot.title = element_text(hjust=0.5)
                ,legend.position = "right")
        
        output$display_plot_CDF <- renderUI({
          if(input$CDF_shading!="noShading"){
            plotOutput(ns("display_plot_CDF_static"))
          } else{
            plotlyOutput(ns("display_plot_CDF_plotly"))
          }
        })
        
        
        if(input$CDF_shading!="noShading"){
          
          output$display_plot_CDF_static <- renderPlot({
            g <- g + geom_ribbon(aes(ymin = ecdf_min, 
                                     ymax = ecdf_max,
                                     fill = Year),
                                 alpha = 0.2)
            print(g)
          })
        }
        
        if(input$CDF_shading=="noShading"){
          output$display_plot_CDF_plotly <- renderPlotly({
            ggplotly(g)
          })
        }
        
      })
      
      
      
      #common
      renderErrorMsg <- function(msg) {
        output$cdfError <- renderUI({
          div(class="alert alert-danger" , msg) 
        })
      }
      clearContents <- function(){
        output$cdfError <- renderUI({})
      }
      
      clearPlot <- function(){
        output$display_plot_CDF <- renderPlotly({
          plotly_empty()
        })
      }
      
    })# end of server module
}

### DIY

