###Plottings to check nutrient retention estimation
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)

##Nitrogen
#Plotting N retention (y axis, gm2y-1 based on the eq above) x N loading (x axis, gm2y-1)
#result => r2=1

nitrogen_loads <- list.files(path= "data/results_TN/",pattern = ".csv", full.names = T)%>%
  map(read.csv) %>% 
  data.table::rbindlist()
 

##Phosphorus
#Plotting residence time (yr, y axis) x inflow P conc (x axis, µg l−1)

phosphorus_conc_files <- list.files(path= "data/nwis/",pattern = ".rds", full.names = T)%>%
  map(readRDS) %>% 
    data.table::rbindlist(fill=TRUE)%>%
   mutate(phosphorus_mgl = as.numeric(phosphorus_mgl))

phosphorus_conc_files$year <- year(phosphorus_conc_files$date_time)

phosphorus_conc_yrs <- group_by(phosphorus_conc_files, 
                          year, site_no) %>%
  dplyr::summarize(annual_median_TP = mean(phosphorus_mgl))%>%
  rename(flow_station_id = site_no)


#dplyr::select(site_no,  date_time, flow_cfs, TN_mgl)%>%
#  mutate(TN_mgl = as.numeric(TN_mgl))

upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                              colClasses = "character",
                                              stringsAsFactors = FALSE
) 

#I merged hydrolakes and LAGOS in QGis - not sure if it worked well. Maybe repeat this step!
hydrolakes_lagos <- st_read("shps/hydrolakes_lagos_merged.shp") %>% 
  rename(lagoslakeid = lagoslakei)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))


hydrolakes_upstream_sites <- inner_join(upstream_sites_lagos, hydrolakes_lagos, by="lagoslakeid")

upstream_conc_hydro <- inner_join(hydrolakes_upstream_sites, phosphorus_conc_yrs, by="flow_station_id")%>%
  group_by(flow_station_id, Res_tim)%>%
  select(flow_station_id, Res_tim, annual_median_TP)

ggplot(upstream_conc_hydro, aes(x=annual_median_TP, y=Res_tim)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

##Looking at res_time x discharge
discharge_upstream <- list.files(path= "data/nwis/",pattern = ".rds", full.names = T)%>%
  map(readRDS) %>% 
  data.table::rbindlist(fill=TRUE)%>%
  mutate(flow_cfs = as.numeric(flow_cfs))

discharge_upstream$year <- year(discharge_upstream$date_time)

discharge_upstream_yrs <- group_by(discharge_upstream, 
                                year, site_no) %>%
  dplyr::summarize(annual_median_flow = median(flow_cfs))%>%
  rename(flow_station_id = site_no)

upstream_flow_hydro <- inner_join(hydrolakes_upstream_sites, discharge_upstream_yrs, by="flow_station_id")%>%
  group_by(flow_station_id, Res_tim)%>%
  select(flow_station_id, Res_tim, annual_median_flow)

##Checking residence value values!! RT=1200 yrs??
ggplot(upstream_flow_hydro, aes(x=annual_median_flow, y=Res_tim)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)
