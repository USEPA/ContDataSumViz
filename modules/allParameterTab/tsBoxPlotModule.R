#' Continuous Data Exploration / All Parameter / Box Plots tab (user interface side)
#'
#' @param id 
#'
TsBoxPlotModuleUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class="panel panel-default",style="margin:10px;",
          div(class="panel-heading"),
          div(class="panel-body",
              uiOutput(ns("box_input_1")),
              uiOutput(ns("box_input_2")),
              uiOutput(ns("box_input_3")),
              uiOutput(ns("box_input_4")),
              uiOutput(ns("box_input_5"))
          )#end of panel body
      )# end of panel
    ),
    mainPanel(
      width = 9,
      fluidRow(
        shinydashboard::box(id=ns("boxplot_tab_help"), width=12, class="well",
                            h4("Any parameters – Boxplots"),
                            div(style="width:100%;", "Displays boxplots of a user-selected daily summary statistic (e.g., mean, median, standard deviation) for any processed continuous parameter, grouped by a user-selected time period. Seasons are defined as: Winter (December, January, February), Spring (March, April, May), Summer (June, July, August), Fall (September, October, November). Refer to the Days of available data table in the Summary tables tab to determine the number of points contributing to each boxplot."))
      ),
      fluidRow(div(style="width:100%", uiOutput(ns("boxPlotError"))),
                      plotlyOutput(ns("display_box_plots")))
    ) # mainPanel end
  ) # sidebarLayout end
}

#' Continuous Data Exploration / All Parameter / Box Plots tab (server side)
#'
#' @param id 
#' @param dailyStats 
#' @param renderTSBoxPlot 
#'
TsBoxPlotModuleServer <- function(id, dailyStats, renderTSBoxPlot, loaded_data) {
 
  localStats <- reactiveValues(stats=list())
  variables_avail <- reactiveValues(params=list())
  
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
            if(renderTSBoxPlot$render == TRUE) {
                    output$box_input_1 <- renderUI({
                      selectizeInput(ns("boxplot_variable_name"),label ="Select variable name",
                                     choices=variables_avail$params,
                                     multiple = FALSE,
                                     selected=variables_avail$params[1],
                                     options = list(hideSelected = FALSE))
                    })
                    
                    output$box_input_2 <- renderUI({
                      
                      selectizeInput(ns("boxplot_metrics"),label ="Select daily statistics metric",
                                     choices=c("mean","median","min", "max","range","sd","var","cv","n"),
                                     multiple = FALSE,
                                     selected="mean",
                                     options = list(hideSelected = FALSE))
                    })
                    
                    output$box_input_3 <- renderUI({
                      div(
                        radioButtons(ns("box_group"), "Group by", choices = c("Month"="Month"
                                                                          ,"Month (years side by side)"="month2"
                                                                          ,"Year"="Year"
                                                                          ,"Season"="Season"
                                                                          ,"Season (years side by side)"="season2"),
                                     selected = "Month"))
                      
                    })
                    
                    output$box_input_4 <- renderUI({
                      textInput(inputId=ns("box_title"), label="Plot title",value="")
                    })
                    
                    
                    output$box_input_5 <- renderUI({
                      actionButton(inputId=ns("display_box"), label="Display",class="btn btn-primary")
                    })
            }
          })
          observeEvent(input$display_box, {
            localStats <- dailyStats
            clearContents()
            clearPlot()
            
             tryCatch({
                    myList <- localStats$processed_dailyStats
                    variable_to_plot <- input$boxplot_variable_name
                    myData <- myList[[which(names(myList)==variable_to_plot)]] %>% 
                      mutate(Year = lubridate::year(Date) %>% as.factor(),
                             Month = lubridate::month(Date) %>% as.factor(),
                             Season = case_when(
                               Month %in% c(12,1,2) ~ "Winter",
                               Month %in% c(3,4,5) ~ "Spring",
                               Month %in% c(6,7,8) ~ "Summer", 
                               Month %in% c(9,10,11) ~ "Fall") %>% as.factor())
                    mean_col <- paste0(input$boxplot_variable_name,".",input$boxplot_metrics)

                     if(input$box_group %in% c("Year", "Month", "Season")) {
                      cols_selected = c("Date",input$box_group,mean_col)
                       
                     }else if(input$box_group=="month2"){
                      cols_selected = c("Date","Year","Month",mean_col)
                      
                    }else if(input$box_group=="season2"){
                      cols_selected = c("Date","Year","Season",mean_col)
                      
                    }
                    
                    data_to_plot <- myData[cols_selected]
                    
                    if (!all(is.na(data_to_plot[,mean_col]))&input$box_group!="month2"&input$box_group!="season2"){
                      output$display_box_plots <- renderPlotly({
                        
                        p2 <- ggplot(data=data_to_plot,aes(x=!!sym(isolate(input$box_group)),y=!!sym(isolate(mean_col)))) +
                          geom_boxplot()+
                          labs(title=isolate(input$box_title),x = isolate(input$box_group),y = isolate(input$boxplot_variable_name))+
                          theme_bw()+
                          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                                ,plot.title = element_text(hjust=0.5)
                                ,axis.text.x = element_text(angle=0, hjust=1))
                        p2 <- ggplotly(p2) %>% 
                          plotly::config(toImageButtonOptions = list(format = "png",
                                                                     filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                                       isolate(input$boxplot_variable_name), "_",
                                                                                       isolate(input$boxplot_metrics), "_",
                                                                                       isolate(input$box_group), "_",
                                                                                       "boxplot")))
                        print(p2)
                      })
                    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="month2"){
                      output$display_box_plots <- renderPlotly({
                        
                        p2 <- ggplot(data=data_to_plot,aes(x=Month,y=!!sym(isolate(mean_col)),fill=Year)) +
                          geom_boxplot()+
                          labs(title=isolate(input$box_title),x = "Month",y = isolate(input$boxplot_variable_name))+
                          theme_bw()+
                          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                                ,plot.title = element_text(hjust=0.5)
                                ,axis.text.x = element_text(angle=0, hjust=1))
                        #p2 <- 
                          ggplotly(p2) %>% plotly::layout(boxmode="group") %>% 
                          plotly::config(toImageButtonOptions = list(format = "png",
                                                                     filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                                       isolate(input$boxplot_variable_name), "_",
                                                                                       isolate(input$boxplot_metrics), "_",
                                                                                       isolate(input$box_group), "_",
                                                                                       "boxplot")))
                        # print(p2)
                      })
                    } else if(!all(is.na(data_to_plot[,mean_col]))&input$box_group=="season2"){
                      output$display_box_plots <- renderPlotly({
                        data_to_plot$Season = reorderSeason(data_to_plot$Season)
                        p2 <- ggplot(data=data_to_plot,aes(x=Season,y=!!sym(isolate(mean_col)),fill=Year)) +
                          geom_boxplot()+
                          labs(title=isolate(input$box_title),x = "Season",y = isolate(input$boxplot_variable_name))+
                          theme_bw()+
                          theme(text=element_text(size=16,face = "bold", color="cornflowerblue")
                                ,plot.title = element_text(hjust=0.5)
                                ,axis.text.x = element_text(angle=0, hjust=1))
                        #p2<- 
                          ggplotly(p2) %>% plotly::layout(boxmode="group") %>% 
                          plotly::config(toImageButtonOptions = list(format = "png",
                                                                     filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"),
                                                                                       isolate(input$boxplot_variable_name), "_",
                                                                                       isolate(input$boxplot_metrics), "_",
                                                                                       isolate(input$box_group), "_",
                                                                                       "boxplot")))
                        #print(p2)
                      })
                    }else{
                      renderErrorMsg(noBoxPlotDataFound)
                      clearPlot()
                    }
            }, error = function(err) {
              renderErrorMsg(paste("Process failed due to invalid data, error: ", err$message))
              clearPlot()
            })
            #runjs(sprintf('document.getElementById("%s").scrollIntoView({ behavior: "smooth" });', ns("display_box_plots")))
          })  #observeEvent end
          
          #common
          renderErrorMsg <- function(msg) {
            output$boxPlotError <- renderUI({
              div(class="alert alert-danger" , msg) 
            })
          }
          clearContents <- function(){
            output$boxPlotError <- renderUI({})
          }
          
          clearPlot <- function(){
            output$display_box_plots <- renderPlotly({
              plotly_empty()
            })
          }


    })# end of server module
}
