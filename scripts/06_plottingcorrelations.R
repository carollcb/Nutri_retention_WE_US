###Plottings to check nutrient retention estimation
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)
library(tibble)

##Nitrogen
#Plotting log(N retention/water discharge (y axis, gm2y-1/m3s-1) x Load( N loading/water discharge (x axis, gm2y-1/m3s-1)
#result => r2=1

#Using reduced sample so far
nitrogen_loads <- list.files(path= "data/results_TN/",pattern = ".csv", full.names = T)%>%
  map(read.csv) %>% 
  data.table::rbindlist()

write.csv(nitrogen_loads, "data/TN_loads_test.csv")

TN_loads <- read.csv("data/TN_loads_test.csv")%>%
  mutate(flow_station_id = as.character(flow_station_id)) #,
                     # colClasses = c("station_id" = "character", "flow_station_id" = "character")))


  

upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 

hydrolakes_lagos <- st_read("shps/joined_hydrolakes_lagos.shp") %>% 
  rename(lagoslakeid = lagoslakei)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))


hydrolakes_upstream_sites <- inner_join(upstream_sites_lagos, hydrolakes_lagos, by="lagoslakeid")


upstream_Nconc_hydro <- inner_join(hydrolakes_upstream_sites, TN_loads, by="flow_station_id")%>%
  mutate(flux_gyr = flux_kgy/1000)%>%
  group_by(flow_station_id, Dis_avg)%>%
  select(flow_station_id, Dis_avg, flux_gyr)

#R =((10^(1.00*(log(x/water discharge))- 0:39))/(water discharge))
upstream_Nconc_hydro <- upstream_Nconc_hydro %>%
  #na.omit() %>%
  mutate(Nret = ((10^(1.00*(log(flux_gyr/Dis_avg))- 0.39))/(Dis_avg)))

ggplot(upstream_Nconc_hydro, aes(x=(log(flux_gyr/Dis_avg)), y=(log(Nret/Dis_avg)))) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

##Phosphorus
#Plotting residence time (yr, y axis) x inflow P conc (x axis, µg l−1)

phosphorus_conc_files <- list.files(path= "data/nwis/",pattern = ".rds", full.names = T)%>%
  map(readRDS) %>% 
    data.table::rbindlist(fill=TRUE)%>%
   mutate(phosphorus_mgl = as.numeric(phosphorus_mgl))%>%
  rename(flow_station_id = site_no)

phosphorus_conc_files$year <- year(phosphorus_conc_files$date_time)

phosphorus_conc_yrs <- group_by(phosphorus_conc_files, 
                          year, flow_station_id) %>%
  dplyr::summarize(annual_median_TP = mean(phosphorus_mgl))#%>%
  #rename(flow_station_id = site_no)


#I merged hydrolakes and LAGOS in QGis - not sure if it worked well. Maybe repeat this step!
hydrolakes_lagos <- st_read("shps/hydrolakes_lagos_merged.shp") %>% 
  rename(lagoslakeid = lagoslakei)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  mutate(res_time_yr = Res_tim/365)


hydrolakes_upstream_sites <- inner_join(upstream_sites_lagos, hydrolakes_lagos, by="lagoslakeid")


upstream_conc_hydro <- inner_join(hydrolakes_upstream_sites, phosphorus_conc_yrs, by="flow_station_id")%>%
  mutate(ann_median_TP_ugl = annual_median_TP*1000)%>%
   group_by(flow_station_id, res_time_yr)%>%
  select(flow_station_id, res_time_yr, ann_median_TP_ugl)

upstream_conc_hydro <- upstream_conc_hydro %>%
  mutate(Pret_coef = ((1-(1.43/ann_median_TP_ugl))*((ann_median_TP_ugl)/(1 + (res_time_yr^0.5)))^0.88))
#Hejzlar says this number is something between 0.02 and 0.96 -> check that!

ggplot(upstream_conc_hydro, aes(x=ann_median_TP_ugl, y=res_time_yr)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

ggplot(upstream_conc_hydro, aes(x=res_time_yr, y=Pret_coef)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

##Disconsider from here on: Looking at res_time x discharge
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
  group_by(flow_station_id, res_time_yr)%>%
  select(flow_station_id, res_time_yr, annual_median_flow)

ggplot(upstream_flow_hydro, aes(x=annual_median_flow, y=res_time_yr)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)
