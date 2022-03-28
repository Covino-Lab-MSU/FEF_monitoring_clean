
## opens all .csvs of co2 raw data 
  # concatonates
  # formats dates
  # removes unwanted columns
  # removes repeated rows from overlapping datafiles
opn_concat <- function(interfiles, location, site) {
  file_path <- paste(getwd(),interfiles,location,site, sep='/')
  path_list <- paste(file_path, list.files(file_path), sep= '/')
  data <- lapply(path_list, function(x) {
    dat <- read.table(x, skip = 0, header = TRUE, sep = ",", row.names = NULL, as.is = TRUE)
    # for each item in path list, grab the device number
    #dat$logr_no <- unlist(strsplit(x, "_"))[9]
    return(dat)
  })
  combined.data <- do.call(rbind, data)
  drops <- c("formatted_datetime")
  combined.data <- combined.data %>%
    mutate(datetime = lubridate::mdy(date) + hms(time))%>%
    arrange(datetime)%>%
    select(-one_of(drops))%>%
    distinct()%>%
    return(combined.data)
}

#checks TimeSteps
checkTimeSteps<- function(df=co2_raw$datetime){
  checkts<- c(1,2+which(diff(diff(df))!=0))
  return(checkts)
}  
  
DyBatt<- function(){
    tsBatt<- xts(dplyr::select(co2_raw, datetime,voltage_v), order.by=co2_raw$datetime)
    dygraph(tsBatt) %>% 
      dyRangeSelector()%>%
      dyHighlight(highlightCircleSize = 4, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = TRUE)%>%
      dyOptions(drawPoints = TRUE, pointSize = 2)%>%
      dyLegend(show = "always")
}





