###Plottings to check nutrient retention estimation
library(sf)
library(tidyverse)
library(lubridate)
library(mapview)
library(tibble)
library(patchwork)

source("scripts/07_organizing-nutrient-loads-df_newQ.R")
## Loading data

upstream_sites_lagos <- read.csv("data/upstream_sites_final.csv",
                                 colClasses = "character",
                                 stringsAsFactors = FALSE
) 


## How many lakes x reservoirs?
#upstream_sites_lagos_sp <- st_as_sf(upstream_sites_lagos, coords= c("lon", "lat"), crs= 4326)
#mapview(upstream_sites_lagos_sp) #


#I've joined hydrolakes and LAGOS in QGis - 
hydrolakes_lagos <- st_read("D:/nutri_ret_shps/joined_hydrolakes_lagos_Final.shp") %>% 
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

nitrogen_loads_lt <- nutrient_loads_lagos %>%
  group_by(station_id) %>%
  summarise(lt_flux_kgy = median(fluxTN_kgy))

P_discharge_lt <- nutrient_loads_lagos %>%
  group_by(flow_station_id) %>%
  summarise(lt_flow_m3y = median(flow_m3y))

##long-term values?
#upstream_Nconc_hydro_lt <- inner_join(hydrolakes_upstream_sites, nitrogen_loads_lt, by="station_id")%>%
 # group_by(lagoslakeid, Dis_avg)%>%
 # summarise(total_lt_flux_kgy = sum(lt_flux_kgy))%>%
#mutate(fluxTN_gyr = total_lt_flux_kgy/1000)%>%
 # na.omit()%>%
  #mutate(Nret = ((10^(1.00*(log(fluxTN_gyr/Dis_avg))- 0.39))/(Dis_avg)))
  

#g1 <- ggplot(upstream_Nconc_hydro_lt, aes(x=total_lt_flux_kgy, y=Nret)) +
#  geom_point(size=2, shape=23)+
#  geom_smooth(method=lm)

###--------Phosphorus ---------###
#Plotting residence time (yr, y axis) x inflow P conc (x axis, µg l−1)

#new calculus
TP_data_test <- TNP %>%
  rename(station_id = id)%>%
  group_by(station_id, water_year) %>%
  summarize(Pin_ugl = (flux_TP_kgy/flow_m3y)*1000000)

TP_data_new <- nutrient_loads_lagos %>%
  group_by(flow_station_id) %>%
  summarize(Pin_ugl = sum(fluxTP_kgy)/sum(flow_m3y)*1000000)
 
upstream_conc_hydro_final <- inner_join(hydrolakes_upstream_sites, TP_data_test, by="station_id")%>%
  group_by(station_id, res_time_yr)%>%
  dplyr::select(lagoslakeid, res_time_yr, Pin_ugl, water_year)

#Hejzlar says this number is something between 0.02 and 0.96 -> check that!
#upstream_Pconc_hydro_nooutl <- upstream_conc_hydro_final[-(c(80,85)),]

P_loads_lt <- nutrient_loads_lagos %>%
  group_by(lagoslakeid, water_year) %>%
  summarise(lt_flux_kgy = median(fluxTP_kgy))


upstream_Pconc_hydro_lt <- merge(upstream_conc_hydro_final, P_loads_lt)%>%
  group_by(lagoslakeid, water_year,res_time_yr )%>%
  summarise(total_Pin_ugl = sum(Pin_ugl))%>%
  #mutate(fluxTP_gyr = total_lt_flux_kgy/1000)%>%
  #na.omit() %>%
  mutate(Pret_coef = ((1-(1.43/total_Pin_ugl))*((total_Pin_ugl)/(1 + (res_time_yr^0.5)))^0.88))%>%
  na.omit() 

#write.csv(upstream_Pconc_hydro_lt, "data/Pretention_df.csv")

ggplot(upstream_Pconc_hydro_lt, aes(x=res_time_yr, y=Pret_coef)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)


g2 <- ggplot(upstream_Pconc_hydro_lt, aes(x=total_Pin_ugl, y=Pret_coef)) +
  geom_point(size=2, shape=23)+
  geom_smooth(method=lm)

g1 <- ggplot(Nretention_ts, aes(x=totTNload_gm2yr, y=TN_removal_gNm2yr)) +
    geom_point(size=2, shape=23)+
    geom_smooth(method=lm)

g1 + g2  ##check with the group -> accord Hejzlar those should be something between 0.02 and 0.96
