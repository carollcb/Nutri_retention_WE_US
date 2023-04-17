###Creating manual workflow to get Q and TP data from upstream gauges using dataretrieval package
#Don't need to run it again. We already have the candidate_sites file.

library(dataRetrieval)
library(tidyverse)

#pulling in the candidate sites with TP and discharge data
candidate_sites_WE <- readRDS("data/TP_flow_gauges.rds") %>%
  mutate(flow_station_id = as.character(flow_station_id)) %>%
  dplyr::filter(is_lake_gage == "TRUE")%>%
  dplyr::filter(station_id == flow_station_id)%>%
  rename(sites = flow_station_id)%>%
  dplyr::select(sites)

#write.csv(candidate_sites_WE, "data/candidate_sites_WE.csv")

siteNumber <- "10343500" 
#Info <- readNWISsite(siteNumber)
parameterCd <- "00060" #discharge
parameterCd2 <- "00665" #TP

# Raw daily data:
Q_rawDailyData <- readNWISdv(
  siteNumber, parameterCd,
  "2000-10-01", "2015-09-30") %>%
  dplyr::select(site_no,Date, X_00060_00003)%>%
  rename(date_time = Date, flow_cfs = X_00060_00003)

TP_rawDailyData <- readNWISqw(
  siteNumber, parameterCd2,
  "2000-10-01", "2015-09-30") %>%
  dplyr::select(site_no,sample_dt, result_va)%>%
  rename(date_time = sample_dt, phosphorus_mgl = result_va)

test_new <- merge(Q_rawDailyData, TP_rawDailyData, by = "date_time" )%>%
  dplyr::select(site_no.x, date_time, flow_cfs,phosphorus_mgl)%>%
  rename(site_no = site_no.x)

saveRDS(test_new, "data/nwis/10343500.rds")  
