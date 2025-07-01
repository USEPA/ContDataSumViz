sumStats.ContDataSumViz<- function(df.input, fun.myParam.Name, flag.cols, flag.codes){
   df.list <- list(contData = data.frame(),
                   sumData = list())
   
   excl_vals <- flag.codes %>% unlist() %>% unname()
   
   df.load <- df.input %>% 
     mutate(across(flag.cols %>% unlist() %>% unname(), as.character))

   for(i in fun.myParam.Name){
     
     if(length(excl_vals)>0){
       df.load <- df.load %>% 
         mutate(!!sym(i) := if_else(!!sym(flag.cols[[i]]) %in% excl_vals, NA, !!sym(i)))
     }

     df.ret <- df.load %>%
       rename("current_param" = i, "current_qf" = flag.cols[[i]]) %>%
       mutate(Date = date(date.formatted)) %>%
       group_by(SiteID, Date) %>% # TODO generalize SiteID col
       summarize(
       !!(paste(i, "mean", sep = ".")) := mean(current_param, na.rm = TRUE),
       !!(paste(i, "median", sep = ".")) := median(current_param, na.rm = TRUE),
       !!(paste(i, "min", sep = ".")) := suppressWarnings(min(current_param, na.rm=TRUE)),
       !!(paste(i, "max", sep = ".")) := suppressWarnings(max(current_param, na.rm=TRUE)),
       !!(paste(i, "range", sep = ".")) := suppressWarnings(max(current_param, na.rm=TRUE)) - suppressWarnings(min(current_param, na.rm=TRUE)),
       !!(paste(i, "sd", sep = ".")) := sd(current_param, na.rm=TRUE),
       !!(paste(i, "var", sep = ".")) := var(current_param, na.rm=TRUE),
       !!(paste(i, "cv", sep = ".")) := sd(current_param, na.rm=TRUE) / mean(current_param, na.rm = TRUE),
       !!(paste(i, "n", sep = ".")) := length(current_param),
       !!(paste(i, "q", "1%", sep = ".")) := quantile(current_param, probs = 0.01, na.rm = TRUE, names = FALSE),
       !!(paste(i, "q", "5%", sep = ".")) := quantile(current_param, probs = 0.05, na.rm = TRUE, names = FALSE),
       !!(paste(i, "q", "10%", sep = ".")) := quantile(current_param, probs = 0.1, na.rm = TRUE, names = FALSE),
       !!(paste(i, "q", "25%", sep = ".")) := quantile(current_param, probs = 0.25, na.rm = TRUE, names = FALSE),
       !!(paste(i, "q", "50%", sep = ".")) := quantile(current_param, probs = 0.50, na.rm = TRUE, names = FALSE),
       !!(paste(i, "q", "75%", sep = ".")) := quantile(current_param, probs = 0.75, na.rm = TRUE, names = FALSE),
       !!(paste(i, "q", "90%", sep = ".")) := quantile(current_param, probs = 0.90, na.rm = TRUE, names = FALSE),
       !!(paste(i, "q", "99%", sep = ".")) := quantile(current_param, probs = 0.99, na.rm = TRUE, names = FALSE)) %>% 
       mutate(across(everything(), ~ replace(., is.infinite(.), NA)),
              across(everything(), ~ replace(., is.nan(.), NA))) %>% 
       ungroup()
     
     df.list$sumData[[i]] <- df.ret
   }
   
  df.list$contData <- df.load
    
  return(df.list)
  
}
