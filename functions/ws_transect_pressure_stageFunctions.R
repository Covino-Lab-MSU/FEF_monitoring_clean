#Functions

# Load or install as needed packages in a list

pkgTest <- function(x)
{
   if (x %in% rownames(installed.packages()) == FALSE) {
      install.packages(x, dependencies= TRUE)
   }
   library(x, character.only = TRUE)
}
#open_concat
# opening with this function allows user to import a specific year,
# watershed and transect 

#1. find files based on location and site
#2. concatonate all files in the folder
#3. add instrument no to each df
#4. eliminate duplicated dates and rename columns as needed



#checks TimeSteps (LK)
checkTimesteps <- function(df) {
   pstage_list <- split(df, df$position)
   ps_list <- lapply(pstage_list, function(x) {  
      cbind(x, set_interval = as.numeric((x$datetime[2]-x$datetime[1]), units = 'mins'))
   })
   ps_list <- lapply(ps_list, function(x) {
      cbind(x, actual_interval = as.numeric((x$datetime - lag(x$datetime)),units = 'mins'))
   })
   missed_ts <- lapply(ps_list, function(x) {
      x[x$set_interval!= x$actual_interval, ]
   })
   missed_ts <- do.call(rbind, missed_ts)
   return(missed_ts)
}



# Everything above is required for current script (2 June 2022)

###Dygraph for raw temperature data
DyTemp<- function(df, threshold='.2',airtemp='y'){
   tempRaw <- mutate(df, flag = ifelse(c(0,abs(diff(temperature_C)))/temperature_C > threshold, 100, 2))
   if(airtemp == 'y'){
      tsTemp<- xts(dplyr::select(tempRaw, datetime,ID,temperature_C,logger_temp), order.by=tempRaw$datetime)
   }else if(airtemp=='n'){
      tsTemp<- xts(dplyr::select(tempRaw, datetime,ID,temperature_C), order.by=tempRaw$datetime)
   }
   #launch dygraph using xts object
   dygraph(tsTemp, main = paste(site, df$trans_loc[1])) %>% 
      #adds time series launcher
      dyRangeSelector() %>% 
      #adds highlight/fade (controled by alpha) of series and formats circle.
      #dyHighlight(highlightCircleSize = 4, 
      #            highlightSeriesBackgroundAlpha = 0.2,
      #            hideOnMouseOut = TRUE)%>% 
      dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
      dyAxis('y',label='Degree C',valueRange = c(min(df$temperature_C)-5, max(df$temperature_C)+5))%>%
      dyAxis('y2',label='ID',independentTicks=T)%>%
      dySeries('ID',axis='y2')%>%
      #Assigns legend to follow cursor, also can choose 'always' to always see it
      dyLegend(show = "always")
}     




#Dygraph for raw stage - option to flag deviations by slope change 
DyRawPress<- function(df, threshold = 0.05, flag='TRUE', max= 100, min=50){
      if(flag){
            stage_df <- mutate(df, flag = ifelse(c(0,abs(diff(pressure_kPa)))/pressure_kPa > threshold, 100, 51))
            tsStage <- xts(dplyr::select(stage_df, datetime, ID, flag, pressure_kPa), order.by=stage_df$datetime)
            dygraph(tsStage, main = paste(site, df$trans_loc[1])) %>% 
              dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
              dyAxis('y',label='kPa',valueRange = c(min, max)) %>%
              dyAxis('y2',label='ID',independentTicks=T)%>%
              dySeries('ID',axis='y2')%>%
              dyRangeSelector() %>%
              dyLegend(show = "always")
      }else{
            tsStage<- xts(dplyr::select(df, datetime, ID, pressure_kPa), order.by=df$datetime)
            dygraph(tsStage, main = paste(site, df$trans_loc[1])) %>% 
              dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
              dyAxis('y',label='kPa',valueRange = c(min, max)) %>%
              dyAxis('y2',label='ID',independentTicks=T)%>%
              dySeries('ID',axis='y2')%>%
              dyRangeSelector() %>%
              dyLegend(show = "always")
      }
}


#Dygraph for raw pressure and precip - option to flag deviations by slope change 
DyRawPP<- function(df, threshold = 0.1,  max=100, min=50) {
   stage_df <- mutate(df, flag = ifelse(c(0, abs(diff(pressure_kPa)))/pressure_kPa > threshold, 100, 51))
   stage_df <- as.data.frame(stage_df)
   tsStage  <- xts(select(stage_df, datetime, precip, flag, pressure_kPa), order.by=stage_df$datetime)
   dygraph(tsStage, main = paste(site, df$trans_loc[1])) %>% 
     dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
     dyAxis('y',label='kPa',valueRange = c(min, max)) %>%
     dyAxis('y2',label='Precipitation mm', independentTicks=T, valueRange = c(8, -1))%>%
     dySeries('precip',axis='y2', stepPlot = TRUE, fillGraph = TRUE)%>%
     dyRangeSelector() %>%
     dyLegend(show = "always")
}


#Dygraph for raw stage - option to flag deviations by slope change 
DyRawStage<- function(df,threshold = 0.2, flag='TRUE', max=100, min=50){
   if (flag){
      stage_df<- mutate(df, flag = ifelse(c(0,abs(diff(hobo_wtr_depth_cm)))/hobo_wtr_depth_cm > threshold, 100, 2))
      tsStage<- xts(dplyr::select(stage_df, datetime,ID,flag, hobo_wtr_depth_cm), order.by=stage_df$datetime)
      dygraph(tsStage, main = paste(site, df$trans_loc[1])) %>% 
        dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
        dyAxis('y',label='H2O depth (cm)',valueRange = c(min, max)) %>%
        dyAxis('y2',label='ID',independentTicks=T)%>%
        dySeries('ID',axis='y2')%>%
        dyRangeSelector() %>%
        dyLegend(show = "always")
   }else{
      tsStage<- xts(dplyr::select(stage_df, datetime,ID, pressure_kPa), order.by=stage_df$datetime) %>% 
        dygraph(tsStage, main = paste(site, df$trans_loc[1])) %>% 
        dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
        dyAxis('y',label='H2O depth (cm)',valueRange = c(min, max)) %>%
        dyAxis('y2',label='ID',independentTicks=T)%>%
        dySeries('ID',axis='y2')%>%
        dyRangeSelector() %>%
        dyLegend(show = "always")
   }
}


completetimeseries<- function(df){
  #check if collection interval is consistent in dataset. Function as written only handles one   interval but can be modified if interval was changed. 
  ts_interval<- df$datetime[2] - df$datetime[1]
  #round datetime to nearest whole interval
  newdf <- df %>%
    rowid_to_column(var='ID') %>%
    mutate(datetime = round_date(datetime, as.period(ts_interval)))
  #create full timeseries 
  full_ts <- tibble(datetime=seq.POSIXt(df$datetime[1], df$datetime[length(df$datetime)], by=ts_interval))
  newrdf <- full_join(full_ts, newdf, by='datetime')
  return(newrdf)
}


AdjPress <- function(df,maxgap=8){
      #create Stage Adj dataframe and adj wt ht column
      stageAdj<-df%>%
            mutate(adj_press = pressure_kPa)
      #for loop that assigns corrected offset within ranges of IDs define in Cor
      for(i in 1:length(vert_correction$ID)){
            if(i<length(vert_correction$ID)){
                  #assigns final ID for each range
                  x<- vert_correction$ID[i+1]-1
                  #adds cumulative offset
                  stageAdj$pressure_kPa[vert_correction$ID[i]:x] <-stageAdj$pressure_kPa[vert_correction$ID[i]:x]+vert_correction$cumOffset[i]
            }
            #for last set of IDs, assigns the window from the final ID in vert_correction until end of TS
            if(i==length(vert_correction$ID)){
                  stageAdj$adj_press[vert_correction$ID[i]:length(stageAdj$pressure_kPa)]<-
                        stageAdj$pressure_kPa[vert_correction$ID[i]:length(stageAdj$pressure_kPa)]+vert_correction$cumOffset[i]
                  
            }
      }
      #deletes Bad IDs and interpolates between then if gap is less than 8 in a row
      stageAdj <- mutate(stageAdj, adj_press = na.approx(ifelse(ID %in% bad_id,NA, adj_press),maxgap=maxgap,na.rm=F))
      return(stageAdj)    
}





AdjStage <- function(df,maxgap=8){
   #create Stage Adj dataframe and adj wt ht column
   stageAdj <- df %>%
      mutate(adj_stage = hobo_wtr_depth_cm)
   #for loop that assigns corrected offset within ranges of IDs define in Cor
   for(i in 1:length(vert_correction$ID)){
      if(i<length(vert_correction$ID)){
         #assigns final ID for each range
         x<- vert_correction$ID[i+1]-1
         #adds cumulative offset
         stageAdj$adj_stage[vert_correction$ID[i]:x] <- stageAdj$hobo_wtr_depth_cm[vert_correction$ID[i]:x]+vert_correction$cumOffset[i]
      }
      #for last set of IDs, assigns the window from the final ID in vert_correction until end of TS
      if(i==length(vert_correction$ID)){
         stageAdj$adj_stage[vert_correction$ID[i]:length(stageAdj$hobo_wtr_depth_cm)]<-
            stageAdj$hobo_wtr_depth_cm[vert_correction$ID[i]:length(stageAdj$hobo_wtr_depth_cm)]+vert_correction$cumOffset[i]
         
      }
   }
   #deletes Bad IDs and interpolates between then if gap is less than 8 in a row
   stageAdj <- mutate(stageAdj, adj_stage = na.approx(ifelse(ID %in% bad_id,NA, adj_stage),maxgap=maxgap,na.rm=F))
   return(stageAdj)    
}


dyStageAdjonly<- function(df, max=100, min=0){
   tsStageAdj<- xts(dplyr::select(df, datetime, adj_stage, precip), order.by=df$datetime)
   dygraph(tsStageAdj, main = paste(site, df$trans_loc[1])) %>% 
      dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
      dyAxis('y',label='adjusted stage (cm)',valueRange = c(min, max))%>%
      dyAxis('y2',label='Precipitation mm',independentTicks=T, valueRange = c(10, -1))%>%
      dySeries('precip',axis='y2')%>%
      dyRangeSelector() %>%
      #dyHighlight(highlightCircleSize = 4, 
      #        highlightSeriesBackgroundAlpha = 0.2,
      #       hideOnMouseOut = TRUE)%>%
      dyOptions(drawPoints = FALSE, pointSize = 2)%>%
      dyLegend(show = "always")
}




dyPressAdj<- function(df,max=100, min = 0){
      tsStageAdj<- xts(dplyr::select(df, datetime,pressure_kPa, adj_press ,ID), order.by=df$datetime)
      dygraph(tsStageAdj, main = paste(site, df$trans_loc[1])) %>% 
        dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
        dyAxis('y',label='cm',valueRange = c(min, max))%>%
        dyAxis('y2',label='ID',independentTicks=T)%>%
        dySeries('ID',axis='y2')%>%
        dyRangeSelector() %>%
        #dyHighlight(highlightCircleSize = 4, 
        #        highlightSeriesBackgroundAlpha = 0.2,
        #        hideOnMouseOut = TRUE)%>%
        dyOptions(drawPoints = FALSE, pointSize = 2)%>%
        dyLegend(show = "always")
}



dyStageAdj<- function(df,max=50){
   tsStageAdj<- xts(dplyr::select(df, datetime, hobo_wtr_depth_cm, adj_stage ,ID), order.by=df$datetime)
   dygraph(tsStageAdj, main = paste(site, df$trans_loc[1])) %>% 
      dyAxis("x", drawGrid = FALSE, axisLabelWidth = 80) %>%
      dyAxis('y',label='cm',valueRange = c(0, max))%>%
      dyAxis('y2',label='ID',independentTicks=T)%>%
      dySeries('ID',axis='y2')%>%
      dyRangeSelector() %>%
      #dyHighlight(highlightCircleSize = 4, 
      #        highlightSeriesBackgroundAlpha = 0.2,
      #        hideOnMouseOut = TRUE)%>%
      dyOptions(drawPoints = FALSE, pointSize = 2)%>%
      dyLegend(show = "always")
}



#join to main stage database and interpolate offsets and calculate final stage (relative to reference gage position)
interpStage <- function(){
      
      stageCor<- left_join(stageAdj,stageOffset)%>%
            arrange(datetime)
      stageCor$offset[1] <- 0
      stageCor<-mutate(stageCor,interp_offset= ifelse(datetime <= lastdate,
                                                      na.approx(offset,na.rm=F),last_offset))%>%
            mutate(interp_offset=ifelse(is.na(interp_offset),0,interp_offset))%>%
            mutate(final_stage = adj_press+interp_offset)
      return(stageCor)
}
