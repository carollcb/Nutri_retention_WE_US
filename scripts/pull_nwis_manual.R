###Creating manual workflow to get Q and TP data from upstream gauges using dataretrieval package
#Don't need to run it again. We already have the candidate_sites file.

library(dataRetrieval)
library(tidyverse)

#pulling in the candidate sites with TP and discharge data
candidate_sites_WE <- readRDS("data/TN_flow_gauges.rds") %>%
  mutate(flow_station_id = as.character(flow_station_id)) %>%
  dplyr::filter(is_lake_gage == "TRUE")%>%
  dplyr::filter(station_id == flow_station_id)%>%
  rename(sites = flow_station_id)%>%
  dplyr::select(sites)

#library(mapview)
#candidate_sites_TP <- inner_join(candidate_sites_WE, candidate_sites, by="sites")
#mapview(candidate_sites_WE)
write.csv(candidate_sites_WE, "data/candidate_sites_WE_TN.csv", as.character(TRUE))

siteNumber <- "10343500" 
#Info <- readNWISsite(siteNumber)
parameterCd <- "00060" #discharge
parameterCd2 <- "00665" #TP: Phosphorus, water, unfiltered, milligrams per liter as phosphorus

parameterCd3 <- "00600" #Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, milligrams per liter


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
  rename(date_time = sample_dt, TP_mgl = result_va)

test_new <- merge(Q_rawDailyData, TP_rawDailyData, by = "date_time" )%>%
  dplyr::select(site_no.x, date_time, flow_cfs,TP_mgl)%>%
  rename(site_no = site_no.x)

saveRDS(test_new, "data/nwis_TN/10343500.rds")  


###TN workflow


parameterCd <- "00060" #discharge
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

test_new <- merge(Q_rawDailyData, TN_rawDailyData, by = "date_time" )%>%
  dplyr::select(site_no.x, date_time, flow_cfs, nitrogen_mgl)%>%
  rename(site_no = site_no.x)

saveRDS(test_new, "data/nwis_TN/12334550.rds") 
