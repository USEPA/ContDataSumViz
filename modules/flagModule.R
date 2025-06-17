flagUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    fluidRow(
      div(radioButtons(inputId = ns("flagOpt"), 
                       label = "Data flags prepared using",
                       choices = c("ContDataQC", "Other"),
                       selected = "Other")), #, style="margin:10px;"
    ),
    fluidRow(
      uiOutput(ns("flag_next")),
      uiOutput(ns("flag_types")),
      uiOutput(ns("flag_codes"))
    ),
    fluidRow(
      actionButton(inputId = "runQS", label = "Run meta summary", class = "btn btn-primary") #, style = "margin-left: 10px;margin-right: 10px;margin-bottom: 20px;margin-top: 20px;"
    )
  )
}

flagServer <- function(id,  homeDTvalues, formated_raw_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      

      
      observeEvent(input$flagOpt,{
        selectedParams <- homeDTvalues$homeDateAndTime$parmToProcess()
        allCols <- names(formated_raw_data$derivedDF)[names(formated_raw_data$derivedDF) != "date.formatted"]
        
      if(input$flagOpt == "Other"){
        output$flag_next <- renderUI({
          
          dropdown_list <- lapply(seq_along(selectedParams), function(i){
            current_param <- selectedParams[i]
            vars_contain_col <- allCols %>% str_subset(current_param)
            selected_col <- vars_contain_col[vars_contain_col != current_param]
            
            selectInput(inputId = ns(paste0(current_param, "_flag")),
                        label = paste0(current_param, " flag column"),
                        choices = allCols,
                        selected = selected_col) 
          })
          
          tagList(dropdown_list)
          
        })
        
        output$flag_types <- renderUI({
          checkboxGroupInput(inputId = ns("excl_flags"),
                             label = "Quality flag types in the data",
                             choices = c("Fail", "Suspect", "Not known"))
        })
        
        output$flag_codes <- renderUI({
          code_list <- lapply(seq_along(input$excl_flags), function(i){
            textInput(inputId = ns(paste0(input$excl_flags[i], "_code")),
                      label = paste0(input$excl_flags[i], " code"))
          }) 
        })
        # create a column input for every selected
        
      } else if(input$flagOpt == "ContDataQC"){
        output$flag_next <- renderUI({
          fileInput("ContDataQC_config",
                    label = HTML("<b>Upload ContDataQC config file</b>"),
                    multiple = FALSE,
                    buttonLabel=list(tags$b("Browse"),tags$i(class = "fa-solid fa-folder")),
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    )
          )
        })
         
      }
        
      } 
      ) # end observeEvent
    }
  )
}