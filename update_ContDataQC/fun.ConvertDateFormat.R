#' Validate Date and Time format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Operations:
# validate date and convert to correct format needed for the application
# uses lubridate package
#library(lubridate)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' @keywords internal
#' @export
fun.ConvertDateFormat <- function(fun.userDateFormat
                     ,fun.userTimeFormat
                     ,fun.userTimeZone ="UTC"
                     ,fun.userDateFieldName
                     ,fun.userTimeFieldName
                     ,fun.rawData
                     ,fun.date.org) {


  # print(fun.userDateFieldName)
  # print(fun.userTimeFieldName)
  # print(fun.userTimeZone)
  # print(fun.userDateFormat)
  # print(fun.userTimeFormat)
  # print(fun.date.org)
  
    if(fun.date.org == 'combined') {
        tmpDateData <- fun.rawData %>% pull(fun.userDateFieldName)
        if(fun.userTimeFormat == "None") {
          fun.userTimeFormat <- "Hour, Minute, Second"
          tmpDateData <- paste(tmpDateData, "00:00:00", sep=" ")
        }
        fun.rawData <- fun.sub.format.date(fun.userDateFormat, fun.userTimeFormat, fun.userTimeZone, fun.rawData, tmpDateData)
        
    } else if (fun.date.org == 'separate' & !is.null(fun.userTimeFieldName)) {
      
        if(sum(is.na(fun.rawData[fun.userTimeFieldName])) > 0) {
          fun.rawData[fun.userTimeFieldName][is.na(fun.rawData[fun.userTimeFieldName])] <- "00:00:00"
        }
        print(sum(is.na(fun.rawData[fun.userTimeFieldName])))
        tmpDateData <- fun.rawData %>% pull(fun.userDateFieldName)
        tmpTimeDate <- fun.rawData %>% pull(fun.userTimeFieldName)

        
        if(!is.null(fun.userTimeFormat) & fun.userTimeFormat == "None") {
          fun.userTimeFormat <- "Hour, Minute, Second"
          tmpCol <- paste(tmpDateData, "00:00:00", sep=" ")
        } else {
          tmpCol <- paste(tmpDateData, tmpTimeDate, sep=" ")
        }
        fun.rawData <- fun.sub.format.date(fun.userDateFormat, fun.userTimeFormat, fun.userTimeZone, fun.rawData, tmpCol)
        
    }
    return(fun.rawData)
}##FUN.fun.ConvertDateFormat.END

fun.sub.format.date <- function(fun.userDateFormat, fun.userTimeFormat, fun.userTimeZone, fun.rawData, tmpData) {
  
  withCallingHandlers({
  #tryCatch({
    if(fun.userDateFormat == "Year, Month, Day" && fun.userTimeFormat == "Hour, Minute") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::ymd_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%Y-%m-%d %H:%M"
    } else if(fun.userDateFormat == "Year, Day, Month" && fun.userTimeFormat == "Hour, Minute") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::ydm_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%Y-%d-%m %H:%M"
    } else if(fun.userDateFormat == "Month, Day, Year" && fun.userTimeFormat == "Hour, Minute") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::mdy_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%m-%d-%Y %H:%M"
    }  else if(fun.userDateFormat == "Day, Month, Year" && fun.userTimeFormat == "Hour, Minute") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::dmy_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%d-%m-%Y %H:%M"
    } else if(fun.userDateFormat == "Abbreviated month, Day of the month, Year" && fun.userTimeFormat == "Hour, Minute") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::mdy_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%m-%d-%Y %H:%M"
    } 
    
    else if(fun.userDateFormat == "Year, Month, Day" && fun.userTimeFormat == "Hour, Minute, Second") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::ymd_hms(tmpData, tz=fun.userTimeZone)) 
      ContData.env$myFormat.DateTime <- "%Y-%m-%d %H:%M:%S"
    } else if(fun.userDateFormat == "Year, Day, Month" && fun.userTimeFormat == "Hour, Minute, Second") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::ydm_hms(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%Y-%d-%m %H:%M:%S"
    } else if(fun.userDateFormat == "Month, Day, Year" && fun.userTimeFormat == "Hour, Minute, Second") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::mdy_hms(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%m-%d-%Y %H:%M:%S"
    }  else if(fun.userDateFormat == "Day, Month, Year" && fun.userTimeFormat == "Hour, Minute, Second") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::dmy_hms(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%d-%m-%Y %H:%M:%S"
    } else if(fun.userDateFormat == "Abbreviated month, Day of the month, Year" && fun.userTimeFormat == "Hour, Minute, Second") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::mdy_hms(tmpData))
      ContData.env$myFormat.DateTime <- "%m-%d-%Y %H:%M:%S"
    }
    
    else if(fun.userDateFormat == "Year, Month, Day" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::ymd_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%Y-%m-%d %H:%M"
    } else if(fun.userDateFormat == "Year, Day, Month" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::ydm_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%Y-%d-%m %H:%M"
    } else if(fun.userDateFormat == "Month, Day, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::mdy_hm (tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%m-%d-%Y %H:%M"
    }  else if(fun.userDateFormat == "Day, Month Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::dmy_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%d-%m-%Y %H:%M"
    } else if(fun.userDateFormat == "Abbreviated month, Day of the month, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::mdy_hm(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%m-%d-%Y %H:%M"
    }
    
    else if(fun.userDateFormat == "Year, Month, Day" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::ymd_hms(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%Y-%m-%d %H:%M:%S"
    } else if(fun.userDateFormat == "Year, Day, Month" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::ydm_hms(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%Y-%d-%m %H:%M:%S"
    } else if(fun.userDateFormat == "Month, Day, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::mdy_hms(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%m-%d-%Y %H:%M:%S"
    }  else if(fun.userDateFormat == "Day, Month Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::dmy_hms(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%d-%m-%Y %H:%M:%S"
    } else if(fun.userDateFormat == "Abbreviated month, Day of the month, Year" && fun.userTimeFormat == "Hour in 12-hour format, Minute, Second, AM/PM") {
      suppressWarnings(fun.rawData$date.formatted <- lubridate::mdy_hms(tmpData, tz=fun.userTimeZone))
      ContData.env$myFormat.DateTime <- "%m-%d-%Y %H:%M:%S"
    }
    
    missing_rows <- fun.rawData %>% mutate(row_num = row_number()+1) %>% dplyr::filter(is.na(date.formatted)) %>% pull(row_num)
    
    if(length(missing_rows)== 0){
    } else if(length(missing_rows)==nrow(fun.rawData)){
     warning("All date/times failed to parse")
    } else{
      warning(paste0("Row numbers of date/times that failed to parse (row 1 = header): ", paste(missing_rows, collapse = ", ")))
    }
    
    return(fun.rawData)
  },
  warning = function(w){
    if(length(conditionMessage(w))!=0){
       shinyalert("Date time parsing error", paste0(conditionMessage(w), "\n\nAll date/times failed to parse is indicative of incorrect date/time format selections. Some rows failed to parse is indicative of inconsistent date/time formatting or incorrect time zone selection. Review the rows that failed to parse for inconsistencies in date/time formatting or proximity to daylight savings time. Rows with date/times not included in time zones that observe daylight savings time will fail to parse if a daylight savings time-observing time zone is selected."), "warning") 
      }
    
  } ,
  message = function(m){
    if(length(conditionMessage(m)!=0)){
      shinyalert("Dat time parsing message", conditionMessage(m), "info")
    }
  }
  )
}

