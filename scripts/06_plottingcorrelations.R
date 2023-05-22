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


## How many lakes x reservoirs?
#upstream_sites_lagos_sp <- st_as_sf(upstream_sites_lagos, coords= c("lon", "lat"), crs= 4326)
#mapview(upstream_sites_lagos_sp) #22 lakes and 18 reservoirs


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

nitrogen_loads_lt <- TN_loads_lagos %>%
  group_by(station_id) %>%
  summarise(lt_flux_kgy = median(fluxTN_kgy))

P_discharge_lt <- TP_loads_lagos %>%
  group_by(flow_station_id) %>%
  summarise(lt_flow_m3y = median(flow_m3y))

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
  select(flow_station_id, res_time_yr, lt_median_TP_ugl, station_id)

#upstream_dis_con <- inner_join(upstream_conc_hydro, P_discharge_lt, by="flow_station_id")%>%
#  mutate(Pin = lt_median_TP_ugl * lt_flow_m3y)

upstream_conc_hydro <- upstream_conc_hydro %>%
  mutate(Pret_coef = ((1-(1.43/lt_median_TP_ugl))*((lt_median_TP_ugl)/(1 + (res_time_yr^0.5)))^0.88))
#Hejzlar says this number is something between 0.02 and 0.96 -> check that!

#upstream_conc_hydro <- upstream_dis_con %>%
 # mutate(Pret_coef = ((1-(1.43/Pin))*((Pin)/(1 + (res_time_yr^0.5)))^0.88))

ggplot(upstream_conc_hydro, aes(x=lt_median_TP_ugl, y=Pret_coef)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

ggplot(upstream_conc_hydro, aes(x=res_time_yr, y=Pret_coef)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)


##P retention x P loads
TP_loads <- TP %>%
  rename(station_id = id)%>%
  rename(fluxTP_kgy = flux_kgy) 

P_loads_lt <- TP_loads %>%
  group_by(station_id) %>%
  summarise(lt_flux_kgy = median(fluxTP_kgy))


upstream_Pconc_hydro_lt <- inner_join(upstream_conc_hydro, P_loads_lt, by="station_id")%>%
  mutate(fluxTP_gyr = lt_flux_kgy/1000)%>%
  group_by(station_id)%>%
  select(station_id, Pret_coef, fluxTP_gyr)%>%
  na.omit() 

g2 <- ggplot(upstream_Pconc_hydro_lt, aes(x=fluxTP_gyr, y=Pret_coef)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

g1 + g2  ##check with the group -> accord Hejzlar those should be something between 0.02 and 0.96
