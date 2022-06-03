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

# interfiles is a subsection of the filepath that links the working directory
# and the location folder
# location is the catchment name and a folder that contains site/transect folders
#(LK)
opn_concat_psite <- function(interfiles, year, location, site) {
   file_path <- paste(getwd(),interfiles, year, location, site, sep='/')
   path_list <- paste(file_path, list.files(file_path), sep= '/')
   positions <- c(1:4) # designate columns to keep
   data <- lapply(path_list, function(x) {
      dat <- read.table(x, skip = 1, header = TRUE, sep = ",", row.names = NULL, as.is = TRUE) %>%
         select(positions)
      dat <- rename(dat, ID = names(dat[1]), 
                Datetime = names(dat[2]),
                pressure_kPa = names(dat[3]),
                temperature_C = names(dat[4]))
      # for each item in path list, grab the cap_rod number
      dat$position <- unlist(strsplit(x, "_"))[7]
      dat$instrument_no <- unlist(strsplit(x, "_"))[8]
      return(dat)
   })
   combined.data <- do.call(rbind, data) %>%
      mutate(datetime = lubridate::mdy_hms(Datetime, tz = "America/Denver"))%>%
      distinct()%>%
      arrange(datetime) %>%
      select(-Datetime)
      
   return(combined.data)
}

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

# Copied from caprod stage functions for potential later use
#Dygraph for raw stage - option to flag deviations by slope change 
DyRawStage<- function(df=stage_raw_prep,threshold = 0.2, flag='TRUE',max=1200){
      if (flag){
            stage_df<- mutate(df, flag = ifelse(c(0,abs(diff(wtr_ht_avg)))/wtr_ht_avg > threshold, 1000,100))
            tsStage<- xts(dplyr::select(stage_df, datetime,ID,flag,wtr_ht_pt,wtr_ht_avg), order.by=stage_df$datetime)
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
      }else{
            tsStage<- xts(dplyr::select(stage_df, datetime,ID,water_ht1,water_avg), order.by=stage_df$datetime)
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




AdjStage <- function(df=stage_raw,maxgap=8){
      #create Stage Adj dataframe and adj wt ht column
      stageAdj<-df%>%
            mutate(adj_wtr_ht = wtr_ht_avg)
      #for loop that assigns corrected offset within ranges of IDs define in Cor
      for(i in 1:length(vert_correction$ID)){
            if(i<length(vert_correction$ID)){
                  #assigns final ID for each range
                  x<- vert_correction$ID[i+1]-1
                  #adds cumulative offset
                  stageAdj$adj_wtr_ht[vert_correction$ID[i]:x] <-stageAdj$wtr_ht_avg[vert_correction$ID[i]:x]+vert_correction$cumOffset[i]
            }
            #for last set of IDs, assigns the window from the final ID in vert_correction until end of TS
            if(i==length(vert_correction$ID)){
                  stageAdj$adj_wtr_ht[vert_correction$ID[i]:length(stageAdj$wtr_ht_avg)]<-
                        stageAdj$wtr_ht_avg[vert_correction$ID[i]:length(stageAdj$wtr_ht_avg)]+vert_correction$cumOffset[i]
                  
            }
      }
      #deletes Bad IDs and interpolates between then if gap is less than 8 in a row
      stageAdj <- mutate(stageAdj, adj_wtr_ht = na.approx(ifelse(ID %in% bad_id,NA, adj_wtr_ht),maxgap=maxgap,na.rm=F))
      return(stageAdj)    
}

dyStageAdj<- function(df= stageAdj,max=1200){
      tsStageAdj<- xts(dplyr::select(df, datetime,wtr_ht_avg,adj_wtr_ht,ID), order.by=df$datetime)
      dygraph(tsStageAdj) %>% 
            dyAxis('y',label='mm',valueRange = c(-150, max))%>%
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
            mutate(final_stage = adj_wt_ht+interp_offset)
      return(stageCor)
}
