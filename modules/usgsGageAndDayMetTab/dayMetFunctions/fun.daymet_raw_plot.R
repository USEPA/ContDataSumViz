#' Plot daymet raw data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export

fun.dayMetRawPlot <- function(fun.daymet.data,
                              fun.daymet.vars.to.process,
                              fun.daymet.title = "Daymet",
                              fun.internal = TRUE) {
  tryCatch(
    {
      daymet_data_merged <- fun.daymet.data
      

      if (length(fun.daymet.vars.to.process) > 0) {
        # daymetProcessed$dayMetData <- dayMetData$data%>% select(year, yday, precip="prcp..mm.day.") %>%
        # mutate(Date=as.Date(yday, origin=paste(as.character(year - 1), "-12-31", sep="")))

        daymet_data_merged<- daymet_data_merged %>%
          select(c(fun.daymet.vars.to.process), c("year", "yday")) %>%
          mutate(Date = as.Date(yday, origin = paste(as.character(year - 1), "-12-31", sep = ""))) %>%
          select(c(fun.daymet.vars.to.process), "Date") %>%
          gather(key = "parameter", value = "value", -Date)
        
        main_range <- calculate_time_range(as.list(daymet_data_merged))
        mainBreaks <- main_range[[1]]
        main_x_date_label <- main_range[[2]]
        


        dayMetPlot <- ggplot(data = daymet_data_merged %>% mutate(parameter = case_when(
          parameter == "prcp..mm.day." ~ "Precipitation (mm)",
          parameter == "srad..W.m.2." ~ "Shortwave radiation (W m^-2)",
          parameter == "swe..kg.m.2." ~ "Snow water equivalent (kg m^-2)", 
          parameter == "tmax..deg.c." ~ "Maximum air temperature (degrees C)", 
          parameter == "tmin..deg.c." ~ "Minimum air temperature (degrees C)", 
          parameter == "vp..Pa." ~"Water vapor pressure (Pa)"
        ), Date = as.POSIXct(Date, format = "%Y-%m-%d"))
                             ) +
          #geom_line(aes(colour = parameter, y = value, x = as.POSIXct(Date, format = "%Y-%m-%d")), size = 0.8, ) +
          geom_line(aes(colour = parameter, y = value, x = Date), size = 0.8, ) +
          labs(title = fun.daymet.title, y = "Parameters", x = "Date") +
          # scale_color_discrete(
          #   labels = c("prcp..mm.day." = "Precipitation (mm)", "srad..W.m.2."="Shortwave radiation (W m^-2)", "swe..kg.m.2."="Snow water equivalent (kg m^-2)",
          #              "tmax..deg.c."="Maximum air temperature (degrees C)", "tmin..deg.c."="Minimum air temperature (degrees C)", 
          #              "vp..Pa."="Water vapor pressure (Pa)")
          # )+
          scale_x_datetime(date_labels = main_x_date_label, date_breaks = mainBreaks) +
          theme_bw() +
          theme(
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text.y = element_blank(),
            text = element_text(size = 10, face = "bold", color = "cornflowerblue"),
            plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 65, hjust = 10)
          )
        dayMetPlot <- dayMetPlot + facet_grid(parameter ~ ., scales = "free_y")
        return(dayMetPlot)
      }
    },
    error = function(err) {
      message("Error in fun.dayMetRawPlot function")
      print(err)
    }
  )
} ## FUN.fun.GageData.END
