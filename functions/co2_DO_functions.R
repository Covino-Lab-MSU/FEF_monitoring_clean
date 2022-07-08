
## opens all .csvs of co2 raw data 
  # concatonates
  # formats dates
  # removes unwanted columns
  # removes repeated rows from overlapping datafiles


# package loading function
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}


# open and concatonate CO2 data FOR 2021...We formatted these .csvs more extensively before writing a new 
# function for 2022 that allows for much of the processing to happen in R. 

opn_concat_2021 <- function(interfiles, year, location, site) {
  file_path <- paste(getwd(),interfiles,year, location,site, sep='/')
  path_list <- paste(file_path, list.files(file_path), sep= '/')
  data <- lapply(path_list, function(x) {
    dat <- read.table(x, skip = 0, header = TRUE, sep = ",", row.names = NULL, as.is = TRUE)
    # for each item in path list, grab the device number
    #dat$logr_no <- unlist(strsplit(x, "_"))[9]
    return(dat)
  })
  combined.data <- do.call(rbind, data)
  combined.data <- combined.data %>%
    mutate(datetime = lubridate::mdy(Date) + hms(Time))%>%
    arrange(datetime)%>%
    distinct()%>%
    return(combined.data)
}


# open and concatonate CO2 data FOR 2022...This function will eliminate additional heading rows, convert
# mV to V, combine separate date and time columns to one datetime variable to match column format of 
# 2021 data with much less formatting work. 

opn_concat_site_2022 <- function(interfiles, year, location, site) {
  file_path <- paste(getwd(),interfiles,year, location,site, sep='/')
  path_list <- paste(file_path, list.files(file_path), sep= '/')
  data <- lapply(path_list, function(x) {
    dat <- read.table(x, skip = 6, header = TRUE, sep = ",", row.names = NULL, as.is = TRUE)
    dat$year <- unlist(strsplit(x, "/"))[9]
    dat$shed <- unlist(strsplit(x, "/"))[10]
    dat$site <-unlist(strsplit(unlist(strsplit(x, "/"))[11], "_"))[1]
    return(dat)
  })
  combined.data <- do.call(rbind, data)
  combined.data <- combined.data %>%
    mutate(datetime = lubridate::mdy(Date) + hms(Time))%>%
    arrange(datetime)%>%
    distinct()%>%
  return(combined.data)
}

## opens all .txt files of o2 raw data 
# concatonates
# formats dates
# removes unwanted columns

opn_txt_concat <- function(interfiles, year, location, site) {
  file_path <- paste(getwd(), interfiles, year, location,site, sep='/')
  path_list <- paste(file_path, list.files(file_path), sep= '/')
  data <- lapply(path_list, function(x) {
    dat <- read.delim(x, skip = 4, header = FALSE, sep =',')
    colnames(dat) <- c('time_sec', 'BV_volts', 'temp_C', 'DO_mgL', 'Q')
    # for each item in path list, grab the device number
    dat$sensor_no <- gsub("\\..*","",unlist(strsplit(x, " "))[4])
    return(dat)
  })
  #drops <- c('time_sec') #timestamp on raw data is unix, counts seconds since January 1st, 1970 at UTC.
  combined.data <- do.call(rbind, data)
  combined.data <- combined.data %>%
    mutate(datetime = lubridate::as_datetime(time_sec))%>%
    mutate(datetime = lubridate::with_tz(datetime, tzone = "MST"))
  #select(-one_of(drops))%>%
  #distinct()%>%
  #arrange(datetime)%>%
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


DyQuality<- function(){
  tsBatt<- xts(dplyr::select(DO_raw, datetime,Q), order.by=DO_raw$datetime)
  dygraph(tsBatt) %>% 
    dyRangeSelector()%>%
    dyHighlight(highlightCircleSize = 4, 
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = TRUE)%>%
    dyOptions(drawPoints = TRUE, pointSize = 2)%>%
    dyLegend(show = "always")
}



       

#Dygraph for raw data - option to flag deviations by slope change 
DyRawDO<- function(df=stage_raw_prep,threshold = 0.2, flag='TRUE',max=12){
  if (flag){
    DO_df <- mutate(DO_raw, flag = ifelse(c(0,abs(diff(DO_mgL)))/DO_mgL > threshold, 11, 10))
    tsDO <- xts(dplyr::select(DO_df, datetime, ID, flag, DO_mgL, DO_mgL_avg), order.by=DO_df$datetime)
    dygraph(tsDO) %>% 
      dyAxis('y',label='mg/L',valueRange = c(8, max))%>%
      dyAxis('y2',label='ID',independentTicks=T)%>%
      dySeries('ID',axis='y2')%>%
      dyRangeSelector() %>%
      #dyHighlight(highlightCircleSize = 4, 
      #        highlightSeriesBackgroundAlpha = 0.2,
      #        hideOnMouseOut = TRUE)%>%
      #dyOptions(drawPoints = TRUE, pointSize = 2)%>%
      dyLegend(show = "always")
  }else{
    tsDO<- xts(dplyr::select(stage_df, datetime,ID,DO_mgL, DO_mgL_avg), order.by=DO_df$datetime)
    dygraph(tsStage) %>% 
      dyAxis('y',label='mm',valueRange = c(-100, max))%>%
      dyAxis('y2',label='ID',independentTicks=T)%>%
      dySeries('ID',axis='y2')%>%
      dyRangeSelector() %>%
      #dyHighlight(highlightCircleSize = 4, 
      #        highlightSeriesBackgroundAlpha = 0.2,
      #        hideOnMouseOut = TRUE)%>%
      #dyOptions(drawPoints = TRUE, pointSize = 2)%>%
      dyLegend(show = "always")
  }
}
