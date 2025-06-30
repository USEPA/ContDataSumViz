calculateDailyStatsModuleUI <- function(id, readyForCalculation) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  if (readyForCalculation$status == TRUE) {
    
    tagList(
      
      div(actionButton(
        inputId = ns("calculateDailyStatistics2"),
        label = "Calculate daily statistics",
        class = "btn btn-primary",
        style="margin-left:10px;margin-right:10px;margin-top:20px;"
      )),
      # div(id = ns("calcsuccess"),  style = "margin:10px",
      #     p("Calculation successful")),
      p(id = ns("calcsuccess"), "Calculation successful", style = "margin:10px;display:none"),
      hr(),
      div(downloadButton(
        outputId = ns("saveDailyStatistics2"),
        label = "Save daily statistics (optional)",
        class = "btn btn-primary",
        style = "margin-left:10px;margin-right:10px;margin-bottom:20px;display:none;") #padding-left:15px;padding-right:15px;
      )
      
      
    )
  }
}

step5ui <- function(id){
  ns <- NS(id)
  div(id = ns("step5panel"), class="panel panel-default", style = "margin:10px;display:none",
      div(class="panel-heading", "Step 5: Explore & visualize data", style="font-weight:bold;"),
      div(a(href = "#mainTabs","Proceed to the tabs at the top of the page to visualize your data."), style = "margin-left:10px; margin-right:10px; margin-top:20px; margin-bottom:20px;")
  )
}

calculateDailyStatsModuleServer <- function(id, formated_raw_data, homeDTvalues, metaHomeValues, loaded_data, dailyStatusCalculated, processed, readyForCalculation, flags) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      dailyStats <- reactiveValues(processed = list())
      taskState <- reactiveValues(success = FALSE)

      observeEvent(readyForCalculation$status, {
        if (readyForCalculation$status == TRUE) {
          shinyjs::show(id = "calculateDailyStatistics2")
          if (dailyStatusCalculated$status == "finished") {
            shinyjs::show(id = "calcsuccess")
            shinyjs::show(id = "saveDailyStatistics2")
          }
        } else {
          shinyjs::hide(id = "calculateDailyStatistics2")
          shinyjs::hide(id = "calcsuccess")
          shinyjs::hide(id = "saveDailyStatistics2")
        }
      })


      observeEvent(input$calculateDailyStatistics2, {
        tryCatch(
          {
            withProgress(message = paste("Calculating the daily statistics"), value = 0, {
              incProgress(0, detail = "now... ")
              raw_data <- formated_raw_data$derivedDF

              variables_to_calculate <- homeDTvalues$homeDateAndTime$parmToProcess()

              dailyStats$processed <- sumStats.ContDataSumViz(
                fun.myParam.Name = variables_to_calculate,
                df.input = raw_data,
                flag.cols = flags$flagCols,
                flag.codes = flags$flagCodes
              )

              formated_raw_data$derivedDF <- dailyStats$processed$contData
              
              shinyjs::show(id = "calcsuccess")
              shinyjs::show(id = "saveDailyStatistics2")
              shinyjs::show(id = "step5panel")
              incProgress(1 / 1, detail = "Calculated the daily statistics")
              processed$processed_dailyStats <- dailyStats$processed$sumData
              dailyStatusCalculated$status <- "finished"
            })
          },
          error = function(parsingMsg) {
            #print(parsingMsg$message)
            dailyStatusCalculated$status <- "error"
          }
        )
      })

      output$saveDailyStatistics2 <- downloadHandler(
        filename = function() {
          name_in_file <- loaded_data$name
          if (endsWith(loaded_data$name, ".csv")) name_in_file <- sub(".csv$", "", loaded_data$name)
          if (endsWith(loaded_data$name, ".xlsx")) name_in_file <- sub(".xlsx$", "", loaded_data$name)

          if (metaHomeValues$metaVal$how_to_save2() == "save2") {
            paste0("saved_dailyStats_", name_in_file, "_dailyStats.csv")
          } else if (metaHomeValues$metaVal$how_to_save2() == "save1") {
            paste0("saved_dailyStats_", name_in_file, ".zip")
          } else if (metaHomeValues$metaVal$how_to_save2() == "save4") {
            paste0("saved_dailyStats_wqx_", name_in_file, ".csv")
          }
        },
        content = function(file) {
          if (metaHomeValues$metaVal$how_to_save2() == "save2") {
            combined_data <- Reduce(full_join, dailyStats$processed$sumData)
            write.csv(combined_data, file, row.names = FALSE)
          } else if (metaHomeValues$metaVal$how_to_save2() == "save4") {
            wqxData <- Reduce(full_join, dailyStats$processed$sumData)
            wqxData <- wqxData %>%
              pivot_longer(cols = !Date, names_to = "CharacteristicName", values_to = "Value")
            write.csv(wqxData, file, row.names = FALSE)
          } else if (metaHomeValues$metaVal$how_to_save2() == "save1") {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            files <- NULL
            for (i in 1:length(dailyStats$processed$sumData)) {
              name_i <- names(dailyStats$processed$sumData)[i]
              print(name_i)
              filename <- paste0("saved_dailyStats_", loaded_data$name, "_", name_i, "_dailyStats.csv")
              write.csv(dailyStats$processed$sumData[[i]], filename, row.names = FALSE)
              files <- c(filename, files)
            }
            zip::zip(file, files)
          }
        }
      )
    }
  )
}
