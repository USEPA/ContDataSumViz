flagUI <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  tagList(
    fluidRow(
      div(radioButtons(inputId = ns("flagOpt"),
                       label = "Data contains quality flags",
                       choices = c("Yes", "No"),
                       selected = "Yes")), #, style="margin:10px;"
    ),
    fluidRow(
      uiOutput(ns("flag_next")),
      uiOutput(ns("flag_types")),
      uiOutput(ns("flag_codes")),
      #uiOutput(ns("ContDataQC_config"))
    ),
    fluidRow(
      actionButton(inputId = "runQS", label = "Run meta summary", class = "btn btn-primary") #, style = "margin-left: 10px;margin-right: 10px;margin-bottom: 20px;margin-top: 20px;"
    )
  )
}

flagServer <- function(id,  homeDTvalues, formated_raw_data, flags) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      
       observeEvent(input$flagOpt,{


        flags$flagOptions <- input$flagOpt
        
        selectedParams <- homeDTvalues$homeDateAndTime$parmToProcess()
        allCols <- names(formated_raw_data$derivedDF)[names(formated_raw_data$derivedDF) != "date.formatted"]
        
       if(input$flagOpt == "Yes"){
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
        
        for(param in selectedParams){
          local({
            current_param <- param
            observeEvent(input[[paste0(current_param, "_flag")]], {
              
              flags$flagCols[[current_param]] <- input[[paste0(current_param, "_flag")]]
            })
          })
          
        }
        
        
        observeEvent(input$excl_flags, {
          for(fc in input$excl_flags){
            local({
              current_code <- fc
              observeEvent(input[[paste0(current_code, "_code")]],{
                flags$flagCodes[[current_code]] <- input[[paste0(current_code, "_code")]]
              })
              
            })
            
          }
        })
       } else if(input$flagOpt == "No"){
         output$flag_next <- renderUI({})
         output$flag_types <- renderUI({})
         output$flag_codes <- renderUI({})
       }
      # } else if(input$flagOpt == "ContDataQC"){
      #   output$flag_next <- renderUI({
      #     radioButtons(inputId = ns("ContDataQC_method"),
      #                  label = "Configuration used",
      #                  choices = c("Default", "Custom"),
      #                  selected = "Default")
      # 
      #   })
      # 
      #   observeEvent(input$ContDateQC_method,{
      #     if(input$ContDateQC_method == "Default"){
      # 
      #     } else if(input$ContDateQC_method == "Custom"){
      #       output$ContDataQC_config <- fileInput("ContDataQC_config",
      #                 label = HTML("<b>Upload ContDataQC config file</b>"),
      #                 multiple = FALSE,
      #                 buttonLabel=list(tags$b("Browse"),tags$i(class = "fa-solid fa-folder")),
      #                 accept = c(
      #                   "text/csv",
      #                   "text/comma-separated-values,text/plain",
      #                   ".csv")
      #       )
      #     }
      #   })
      # }

      } 
      ) # end observeEvent
    }
  )
}