###Plottings to check nutrient retention estimation
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)
library(tibble)

## Loading data

upstream_sites_lagos <- read.csv("data/candidate_sites_TP_TN_Lagos_lakes.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 

#I merged hydrolakes and LAGOS in QGis - not sure if it worked well. Maybe repeat this step!
hydrolakes_lagos <- st_read("shps/joined_hydrolakes_lagos.shp") %>% 
  rename(lagoslakeid = lagoslakei)%>%
  mutate(lagoslakeid = as.character(lagoslakeid))%>%
  mutate(res_time_yr = Res_time/365)

hydrolakes_upstream_sites <- inner_join(upstream_sites_lagos, hydrolakes_lagos, by="lagoslakeid")

###--------Nitrogen ---------###
#Plotting log(N retention/water discharge (y axis, gm2y-1/m3s-1) x Load( N loading/water discharge (x axis, gm2y-1/m3s-1)
#result => r2=1

#Using reduced sample so far
#nitrogen_loads <- list.files(path= "data/results_TN/",pattern = ".csv", full.names = T)%>%
 # map(read.csv) %>% 
 # data.table::rbindlist()%>%
#  mutate(flow_station_id = as.character(flow_station_id))

nitrogen_loads_lt <- TN_loads %>%
  group_by(station_id) %>%
  summarise(lt_flux_kgy = median(fluxTN_kgy))


#long-term values?
upstream_Nconc_hydro_lt <- inner_join(hydrolakes_upstream_sites, nitrogen_loads_lt, by="station_id")%>%
  mutate(fluxTN_gyr = lt_flux_kgy/1000)%>%
  group_by(station_id, Dis_avg)%>%
  select(station_id, Dis_avg, fluxTN_gyr)%>%
  na.omit() %>%
  mutate(Nret = ((10^(1.00*(log(fluxTN_gyr/Dis_avg))- 0.39))/(Dis_avg)))

ggplot(upstream_Nconc_hydro_lt, aes(x=fluxTN_gyr, y=Nret)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

upstream_Nconc_hydro_lt_nooutl <- upstream_Nconc_hydro_lt[-15,]

ggplot(upstream_Nconc_hydro_lt_nooutl, aes(x=fluxTN_gyr, y=Nret)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

###--------Phosphorus ---------###
#Plotting residence time (yr, y axis) x inflow P conc (x axis, µg l−1)

phosphorus_conc_files <- list.files(path= "data/nwis_TP/",pattern = ".rds", full.names = T)%>%
  map(readRDS) %>% 
    data.table::rbindlist(fill=TRUE)%>%
   mutate(phosphorus_mgl = as.numeric(phosphorus_mgl))%>%
  rename(flow_station_id = site_no)

phosphorus_conc_files$year <- year(phosphorus_conc_files$date_time)

phosphorus_conc_yrs <- group_by(phosphorus_conc_files, 
                          year, flow_station_id) %>%
  dplyr::summarize(annual_median_TP = mean(phosphorus_mgl))#%>%
  #rename(flow_station_id = site_no)

phosphorus_conc_lt <- phosphorus_conc_yrs %>%
  na.omit() %>%
group_by(flow_station_id) %>%
  dplyr::summarise(lt_median_tp = median(annual_median_TP))


upstream_conc_hydro <- inner_join(hydrolakes_upstream_sites, phosphorus_conc_lt, by="flow_station_id")%>%
  mutate(lt_median_TP_ugl = lt_median_tp*1000)%>%
   group_by(flow_station_id, res_time_yr)%>%
  select(flow_station_id, res_time_yr, lt_median_TP_ugl)

upstream_conc_hydro <- upstream_conc_hydro %>%
  mutate(Pret_coef = ((1-(1.43/lt_median_TP_ugl))*((lt_median_TP_ugl)/(1 + (res_time_yr^0.5)))^0.88))
#Hejzlar says this number is something between 0.02 and 0.96 -> check that!

ggplot(upstream_conc_hydro, aes(x=lt_median_TP_ugl, y=Pret_coef)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

ggplot(upstream_conc_hydro, aes(x=res_time_yr, y=Pret_coef)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

##Not run from here on: Looking at res_time x discharge
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
