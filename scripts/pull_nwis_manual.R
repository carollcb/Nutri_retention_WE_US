###Creating manual workflow to get Q and TP data from upstream gauges using dataretrieval package
#Don't need to run it again. We already have the candidate_sites file.

library(dataRetrieval)
library(tidyverse)

#pulling in the West sites with TP-TN and discharge data
candidate_sites_WE <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv") %>%
  dplyr::select(station_id, flow_station_id)


###---- Joining TP and TN conc and daily Q data
##USGS gauge id

parameterCd <- "00060" #discharge
parameterCd2 <- "00665" #TP: Phosphorus, water, unfiltered, milligrams per liter as phosphorus
parameterCd3 <- "00600" #Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, milligrams per liter

siteNumber <- "12334550" 

# Raw daily data:
Q_rawDailyData <- readNWISdv(
  siteNumber, parameterCd,
  "2000-10-01", "2015-09-30") %>%
  dplyr::select(site_no,Date, X_00060_00003)%>%
  rename(date_time = Date, flow_cfs = X_00060_00003)

TN_rawDailyData <- readNWISqw(
  siteNumber, parameterCd3,
  "2000-10-01", "2015-09-30") %>%
  dplyr::select(site_no,sample_dt, result_va)%>%
  rename(date_time = sample_dt, nitrogen_mgl = result_va)

TP_rawDailyData <- readNWISqw(
  siteNumber, parameterCd2,
  "2000-10-01", "2015-09-30") %>%
  dplyr::select(site_no,sample_dt, result_va)%>%
  rename(date_time = sample_dt, phosphorus_mgl = result_va)

Q_TN <- full_join(Q_rawDailyData, TN_rawDailyData, by = "date_time" )%>%
  dplyr::select(site_no.x, date_time, flow_cfs, nitrogen_mgl)%>%
  rename(site_no = site_no.x)

Q_TN_TP <- full_join(Q_TN, TP_rawDailyData, by = "date_time" )%>%
  rename(site_no = site_no.x)%>%
  mutate(station_id = site_no)%>%
  filter(flow_cfs > 0)%>%
  dplyr::select(station_id, site_no, date_time, flow_cfs, phosphorus_mgl, nitrogen_mgl)

saveRDS(Q_TN_TP, "data/new_Q_TP_TN/12334550.rds") ##gauges around lake Tahoe has several TN data for the same time of the year. how to fix it?



##SPARROW gauge id
wq_sparrow_TP <- read.csv("C:/Users/cbarbosa/Documents/wq_tp.txt",
                          colClasses = "character",
                          stringsAsFactors = FALSE) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(date_time = date)


wq_sparrow_TN <- read.csv("C:/Users/cbarbosa/Documents/wq_tn.txt",
                          colClasses = "character",
                          stringsAsFactors = FALSE) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(date_time = date)

parameterCd <- "00060" #discharge

siteNumber <- "12467000" 
sparrow_id <- "waCBP161"

# Raw daily data:
Q_rawDailyData <- readNWISdv(
  siteNumber, parameterCd,
  "2000-10-01", "2015-09-30") %>%
  dplyr::select(site_no,Date, X_00060_00003)%>%
  rename(date_time = Date, flow_cfs = X_00060_00003)

TN_sparrow<- wq_sparrow_TN %>%
  filter(station_id == sparrow_id)%>%
  dplyr::select(station_id, date_time, p00600)%>%
  rename(nitrogen_mgl = p00600)%>%
  mutate(nitrogen_mgl = as.numeric(nitrogen_mgl))

TP_sparrow<- wq_sparrow_TP %>%
  filter(station_id == sparrow_id)%>%
  dplyr::select(station_id, date_time, p00665)%>%
  rename(phosphorus_mgl = p00665)%>%
  mutate(phosphorus_mgl = as.numeric(phosphorus_mgl))

Q_TN <- full_join(Q_rawDailyData, TN_sparrow, by = "date_time" )%>%
  mutate(station_id = sparrow_id)%>%
  dplyr::select(station_id, site_no, date_time, flow_cfs,nitrogen_mgl)
  

Q_TN_TP <- full_join(Q_TN, TP_sparrow, by = "date_time" )%>%
  rename(station_id = station_id.x) %>%
  dplyr::select(station_id, site_no, date_time, flow_cfs,phosphorus_mgl, nitrogen_mgl)%>%
  as.data.frame()%>%
  filter(flow_cfs > 0)
  
saveRDS(Q_TN_TP, "data/new_Q_TP_TN/waCBP161.rds") 
