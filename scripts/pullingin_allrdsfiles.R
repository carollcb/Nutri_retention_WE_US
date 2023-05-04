library(tidyverse)
library(data.table)

nitrogen_files <- list.files(path= "data/nwis_TN_station_ids/",pattern = ".rds", full.names = T)%>%
map(readRDS) %>% 
  data.table::rbindlist()%>%
  rename(TN_mgl = nitrogen_mgl)
  dplyr::select(site_no,  date_time, flow_cfs, TN_mgl)%>%
  mutate(TN_mgl = as.numeric(TN_mgl))

saveRDS(nitrogen_files, "data/nwis_TN_station_ids/all_nitrogen.rds") 

##correcting columns names

r13239000 <- r13239000 %>%
rename(phosphorus_mgl = TP_mgl)
# dplyr::select(site_no,  date_time, flow_cfs, TN_mgl)%>%
#  mutate(TN_mgl = as.numeric(TN_mgl))
saveRDS(r13239000, "data/nwis/13239000.rds") 

