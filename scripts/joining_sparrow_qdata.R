library(dplyr)
library(lubridate)
library(dataRetrieval)

candidate_sites_TP_TN_Lagos_lakes <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                              colClasses = "character",
                                              stringsAsFactors = FALSE
) 

candidate_sites_final <- candidate_sites_TP_TN_Lagos_lakes %>%
  dplyr::select(station_id, flow_station_id)

wq_sparrow_TP <- read.csv("C:/Users/cbarbosa/Documents/wq_tp.txt",
                                                            colClasses = "character",
                                                            stringsAsFactors = FALSE
) %>% mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(date_time = date)


wq_sparrow_TN <- read.csv("C:/Users/cbarbosa/Documents//wq_tn.txt",
                          colClasses = "character",
                          stringsAsFactors = FALSE
) %>% mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(date_time = date)


#candidate_sites_TPdata <- inner_join(candidate_sites_TP_TN_Lagos_lakes, wq_sparrow_TP, by="station_id")


###Testing manually joining TP and Q data

head(candidate_sites_final)

parameterCd <- "00060" #discharge
#parameterCd2 <- "00665" #TP: Phosphorus, water, unfiltered, milligrams per liter as phosphorus
#parameterCd3 <- "00600" #Total nitrogen [nitrate + nitrite + ammonia + organic-N], water, unfiltered, milligrams per liter
siteNumber <- "06828500" 
sparrow_id <- "neSRE3REPUB407"

TP_sparrow<- wq_sparrow_TP %>%
  filter(station_id == sparrow_id)%>%
  dplyr::select(station_id, date_time, p00665)%>%
  rename(phosphorus_mgl = p00665)

##teste juntar TP from sparrow
Q_rawDailyData <- readNWISdv(
  siteNumber, parameterCd,
  "2000-10-01", "2015-09-30") %>%
  dplyr::select(site_no,Date, X_00060_00003)%>%
  rename(date_time = Date, flow_cfs = X_00060_00003)%>%
  mutate(station_id = sparrow_id)
  
test_new <- merge(Q_rawDailyData, TP_sparrow, by = "date_time" )%>%
  dplyr::select(station_id.x, site_no, date_time, flow_cfs,phosphorus_mgl)%>%
  rename(station_id = station_id.x)

saveRDS(test_new, "data/nwis/neSRE3REPUB407.rds") 

