#' Display Time Series button
#' @description This module is related to 'Display Tie Series' button on the 
#' Upload File tab
#'
#' @param id 
#'
rawTSModuleUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  plotlyOutput(ns("display_all_raw_ts"))
}

#' Display Time Series button
#' @description This module is server side logic for the 'Display Tie Series' button
#'
#' @param id 
#' @param userSelectedValues 
#' @param formated_raw_data 
#'
rawTSModuleServer <- function(id, userSelectedValues, formated_raw_data, loaded_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      my_raw_choices = userSelectedValues$parmToProcess()
      raw_data <- formated_raw_data$derivedDF

      if (!is.null(my_raw_choices) & nrow(raw_data) != nrow(raw_data[is.na(raw_data$date.formatted),])){

        tryCatch({
          uploaded_raw_data <- raw_data %>% 
          dplyr::select(c(userSelectedValues$parmToProcess()), "Date" = c(date.formatted)) %>% 
          pivot_longer(cols = !Date, names_to = "parameter", values_to = "value")},
          error = function(e){
            if(str_detect(conditionMessage(e),"Can't combine")){
              shinyalert("Error in selected parameters", "Parameters with multiple classes (e.g., numeric and character) have been selected, which is not supported in the raw time series feature. Revise the parameter selection to include parameters with the same class.", "warning")
            }
          })

        p <- ggplot(data = uploaded_raw_data, aes(x=Date, y = value)) +
          geom_line(aes(colour=parameter)) +
          labs(title="Raw data", x="Date", y = "")+ # y="Parameters"
          scale_color_discrete(name = "Parameter")+
          facet_grid(parameter ~ ., scales = "free_y",switch = "both")+
          theme_bw()+
          theme(
             strip.background = element_blank()
            #,strip.text.y = element_blank()
            ,strip.placement = "outside"
            ,text=element_text(size=12,face = "bold", color="cornflowerblue")
            ,strip.text.y = element_blank()
            ,plot.title = element_text(hjust=0.5)
            ,axis.title.x = element_text(margin = margin(t = 5))
            ,legend.position="bottom"
            #,axis.text.x=element_text(angle=65, hjust=10)
          )

        #p = p + facet_grid(parameter ~ ., scales = "free_y",switch = "both")
        output$display_all_raw_ts <- renderPlotly({
          ggplotly(p, height = calculatePlotHeight(length(my_raw_choices) * 2), dynamicTicks = TRUE) %>% 
            plotly::layout(xaxis = list(type = "date")) %>% 
            plotly::config(toImageButtonOptions = list(format = "png", filename = paste0(str_remove(loaded_data$name, ".csv|.xlsx"), "_rawTS")))
          #%>% plotly::layout(legend = list(orientation = "h", x = 0.4, y = -0.3))
        })
        
        #shinyjs::runjs("$('html, body').animate({scrollTop: $(document).height()},2000)")
        # #click the button to hide the selection box
        shinyjs::runjs("$('#dateTimeBoxButton').click()")
      } else {
        #just to make sure
        shinyalert("Warning",invalidDateFormt,closeOnClickOutside = TRUE,closeOnEsc = TRUE,confirmButtonText="OK",inputId = "rawTsError")
      }
      observeEvent(input$rawTsError,{
        shinyjs::runjs("swal.close();")
      })
    })




}
